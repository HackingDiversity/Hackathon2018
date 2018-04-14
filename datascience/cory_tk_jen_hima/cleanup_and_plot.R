library(data.table)
library(ggplot2)
library(foreach)
library(doMC)
registerDoMC(cores=12)
library(ggthemes)

# import data
dat <- fread('sample-data.csv', na.strings="NULL")

# remove duplicated rows
dat <- dat[!(duplicated(dat))]

# remove rows where all question counts is NA
dat <- dat[! (is.na(CountA) & is.na(CountB) & is.na(CountC) & is.na(CountD) & is.na(CountE) & is.na(CountF) & is.na(CountG) & is.na(CountH) & is.na(CountI) & is.na(CountJ))]

# remove rows where "acceptable answers" is NA
dat <- dat[! is.na(AcceptableAnswers)]

# CountJ is NA for all rows; drop CountJ column
dat[,CountJ := NULL]

# Remove rows where the "acceptable answer" has a count value of "NULL"
dat <- dat[! (AcceptableAnswers=="A" & is.na(CountA))]
dat <- dat[! (AcceptableAnswers=="B" & is.na(CountB))]
dat <- dat[! (AcceptableAnswers=="C" & is.na(CountC))]
dat <- dat[! (AcceptableAnswers=="D" & is.na(CountD))]
dat <- dat[! (AcceptableAnswers=="E" & is.na(CountE))]
dat <- dat[! (AcceptableAnswers=="F" & is.na(CountF))]
dat <- dat[! (AcceptableAnswers=="G" & is.na(CountG))]
dat <- dat[! (AcceptableAnswers=="H" & is.na(CountH))]
dat <- dat[! (AcceptableAnswers=="I" & is.na(CountI))]

# Get total number of responses
dat[, totalResponses := apply(.SD, 1, function(x) sum(x, na.rm=TRUE)), .SDcols=c("CountA","CountB","CountC","CountD","CountE","CountF","CountG","CountH","CountI")]

# Get corrected "Acceptable Answers" column by concatenating unique list of all acceptable answers, for ExamID, QuestionID, QuestionPartial combination
dat[, AcceptableAnswersNew := paste(sort(unique(AcceptableAnswers)), collapse=""), by=list(ExamID, QuestionID, QuestionPartial)]

# Remove old acceptable answers col
dat[,AcceptableAnswers := NULL]

# Remove duplicate rows
dat <- dat[!duplicated(dat)]

# Create separate data.table to calculate percent correct answers
dat.answers <- dat[,c("ExamID","QuestionID","QuestionPartial",paste("Count", c("A","B","C","D","E","F","G","H","I"), sep=""), "AcceptableAnswersNew")]

# Remove duplicate rows and set names
dat.answers <- dat.answers[!duplicated(dat.answers)]
setnames(dat.answers, c("ExamID","QuestionID","QuestionPartial",c("A","B","C","D","E","F","G","H","I"), "AcceptableAnswers"))

# Convert to long form and remove NAs
dat.answers.long <- melt(dat.answers, measure.vars=c("A","B","C","D","E","F","G","H","I"), value="Count", variable="Response")
dat.answers.long <- dat.answers.long[! is.na(Count)]

# Create index column for row-by-row assignment of correct responses
dat.answers.long[, indx := 1:.N]

# Test if response is contained within string of AcceptableAnswers, row-by-row
dat.answers.long[, isCorrect := grepl(Response, AcceptableAnswers), by=indx]

# Aggregate total responses as correct and incorrect, convert to wide
dat.answers.ag1 <- dat.answers.long[, list("Total"=sum(Count, na.rm=TRUE)), by=list(ExamID, QuestionID, QuestionPartial, isCorrect)]
dat.answers.wide <- dcast(dat.answers.ag1, ExamID+QuestionID+QuestionPartial~isCorrect)

# Change columnsn to "Correct" and "Incorrect"
setnames(dat.answers.wide, c("TRUE","FALSE"), c("Correct","Incorrect"))

# Calculate total responses and fraction correct
dat.answers.wide[, N_responses := Correct + Incorrect]
dat.answers.wide[, percentCorrect := Correct / (N_responses)]

# Remove NA rows
dat.answers.wide <- dat.answers.wide[! is.na(N_responses)]


# CLEAN table dat.answers.wide now contains "ExamID" "QuestionID" "QuestionPartial" "Incorrect" "Correct" "percentCorrect" "N_responses"

# Set keys for tables
setkey(dat.answers.wide, ExamID, QuestionID, QuestionPartial)
setkey(dat, ExamID, QuestionID, QuestionPartial)

# Merge tables
dat.merge <- merge(dat, dat.answers.wide)

# Remove rows with < 20 responses
dat.merge <- dat.merge[N_responses >= 20]
dat.answers.wide <- dat.answers.wide[N_responses >= 20]

# Get number of questions in exam
dat.answers.wide[, N_questions := .N, by=list(ExamID)]

ggplot(data=dat.answers.wide, mapping=aes(x=N_questions, y=percentCorrect)) + geom_point()

dat.answers.wide.ag <- dat.answers.wide[, list( .N,
                               q.25=quantile(percentCorrect, 0.25), 
                               q.50=quantile(percentCorrect, 0.50),
                               q.75=quantile(percentCorrect, 0.75)), by=QuestionID][order(q.50)]
                               
QuestionIDorder <- dat.answers.wide.ag$QuestionID

dat.answers.wide.ag[, QuestionID := factor(QuestionID, levels=QuestionIDorder)]

g1 <- ggplot(data=dat.answers.wide.ag[N>=3], mapping=aes(x=factor(QuestionID), ymin=q.25, y=q.50, ymax=q.75)) + geom_point() + labs(x="Ascending Median Score", y="Median Score", title="Median score for questions in at least 3 exams") + theme_few(22) + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Get questions that are present in at least 3 exams, with median score of less than 70%
badQuestionsIDs <- sort(dat.answers.wide.ag[N>=3][q.50 <= 0.70]$QuestionID)

# Get classes that contain these bad questions
N_badQuestions <- dat.merge[QuestionID %in% badQuestionsIDs][,list("N_bad"=.N), by=list(AttributeID, AttributeNamePartial)]
N_allQuestions <- dat.merge[, list(N_total = .N), by=list(AttributeID,AttributeNamePartial)]

# Set key for bad/all questions
setkey(N_badQuestions, AttributeID, AttributeNamePartial)
setkey(N_allQuestions, AttributeID, AttributeNamePartial)

# Merge and get proportion of questions that are often answered wrong
dat.mergedQuestions <- merge(N_badQuestions, N_allQuestions)
dat.mergedQuestions[, percentBad := N_bad / N_total]

# Change AttributeID order to ascend, based on proportion of bad questions
dat.mergedQuestions <- dat.mergedQuestions[order(percentBad)]
attributeOrder <- dat.mergedQuestions$AttributeNamePartial
dat.mergedQuestions[, AttributeNamePartial := factor(AttributeNamePartial, levels=attributeOrder)]

# Plot
g2 <- ggplot(data=dat.mergedQuestions, mapping=aes(x=indx, y=percentBad)) + geom_point() +
geom_text(aes(x = indx, y = (percentBad+0.03), label=AttributeID), color = "red", angle = 90, hjust=0) + ylim(-0.1,1.2) +
geom_text(aes(x = indx, y = (percentBad-0.03), label=N_bad), color = "black", angle = 90, hjust=1) +
theme_few(22) + labs(x="Ascending Rank", y="Proportion of within Attribute ID\nwith median score < 70%", title="The most difficult courses based on consistently missed questions")


