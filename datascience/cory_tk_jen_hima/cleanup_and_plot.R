library(data.table)
library(ggplot2)
library(foreach)
library(doMC)
registerDoMC(cores=12)
library(ggthemes)

# import data
dat <- fread('sample-data.csv', na.strings="NULL")

# Select and consolidate rows of data based on answer columns
# We assumed information cannot be gleaned without question-answer information (ie CountA, Count B,...)
# remove completey duplicated rows
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

# We next added some columns of metrics to help with the analysis and data cleaning.
# Get total number of responses in new Column
dat[, totalResponses := apply(.SD, 1, function(x) sum(x, na.rm=TRUE)), .SDcols=c("CountA","CountB","CountC","CountD","CountE","CountF","CountG","CountH","CountI")]

# It appeared that some questions had multiple answers which was represented by duplicate rows with different answers but the same CountA,CountB,etc.
# Make new corrected "Acceptable Answers" column by concatenating unique list of all acceptable answers, for ExamID, QuestionID, QuestionPartial combination
dat[, AcceptableAnswersNew := paste(sort(unique(AcceptableAnswers)), collapse=""), by=list(ExamID, QuestionID, QuestionPartial)]

# Remove old acceptable answers col
dat[,AcceptableAnswers := NULL]

# Remove duplicate rows
dat <- dat[!duplicated(dat)]

# Creating metrics to analyze the cleaned data
# We assumed the ExamID is the test given to a class of students
# Each ExamID had multiple QuestionPartials which corresponded to the questions on the exam
# Create separate data.table to calculate percent correct answers per QuestionPartial per exam
dat.answers <- dat[,c("ExamID","QuestionID","QuestionPartial",paste("Count", c("A","B","C","D","E","F","G","H","I"), sep=""), "AcceptableAnswersNew")]

# We are calculating the exam score per class (ExamID) based on the unique questions being equally weighted per exam.
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

# Merge tables to add percentageCorrect for each QuestionID to original data table
dat.merge <- merge(dat, dat.answers.wide)

# We filtered out rows with QuestionPartial frequency of less than 20 (we assumed these rows had too low sample size)
# Remove rows with < 20 responses
dat.merge <- dat.merge[N_responses >= 20]
dat.answers.wide <- dat.answers.wide[N_responses >= 20]

# Calculate exam scores based on the number of Questions in each exam                              
# Get number of questions in exam
dat.answers.wide[, N_questions := .N, by=list(ExamID)]

# Calculate median and quartiles for percentCorrect per QuestionID (regardless of ExamID)
dat.answers.wide.ag <- dat.answers.wide[, list( .N,
                               q.25=quantile(percentCorrect, 0.25), 
                               q.50=quantile(percentCorrect, 0.50),
                               q.75=quantile(percentCorrect, 0.75)), by=QuestionID][order(q.50)]

# Get ascending QuestionPartial order based on Median percentCorrect
QuestionIDorder <- dat.answers.wide.ag$QuestionID

# Set factor levels in ascending order
dat.answers.wide.ag[, QuestionID := factor(QuestionID, levels=QuestionIDorder)]

# Plot Median percentCorrect for QuestionPartials in AscendingOrder for QuestionPartials in at least 3 exams
# We assumed QuestionPartials with less than 3 appearances on exams to be too few to estimate the Median
g1 <- ggplot(data=dat.answers.wide.ag[N>=3], mapping=aes(x=factor(QuestionID), ymin=q.25, y=q.50, ymax=q.75)) + geom_point() + labs(x="Ascending Median Score", y="Median Score", title="Median score for questions in at least 3 exams") + theme_few(22) + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Get questions that are present in at least 3 exams, with median score of less than 70% to identify Questions that are consistently answered incorrectly
badQuestionsIDs <- sort(dat.answers.wide.ag[N>=3][q.50 <= 0.70]$QuestionID)
  
# Save a list of these questions that are consistently answered incorrectly
write.table(badQuestionIDs, file="badQuestions.txt", col.name=FALSE, row.name=FALSE, quote=FALSE)

# Get classes (AttributeID) that contain these difficult questions
N_badQuestions <- dat.merge[QuestionID %in% badQuestionsIDs][,list("N_bad"=.N), by=list(AttributeID, AttributeNamePartial)]
N_allQuestions <- dat.merge[, list(N_total = .N), by=list(AttributeID,AttributeNamePartial)]

# Set key for bad/all questions so they can be merged back into orginal table containing all metadata
setkey(N_badQuestions, AttributeID, AttributeNamePartial)
setkey(N_allQuestions, AttributeID, AttributeNamePartial)
dat.mergedQuestions <- merge(N_badQuestions, N_allQuestions)
dat.mergedQuestions[, percentBad := N_bad / N_total]

# Change AttributeID order to ascend, based on proportion of bad questions
dat.mergedQuestions <- dat.mergedQuestions[order(percentBad)]
attributeOrder <- dat.mergedQuestions$AttributeNamePartial
dat.mergedQuestions[, AttributeNamePartial := factor(AttributeNamePartial, levels=attributeOrder)]

# Plot classes (AttributeID) in ascending order of proportion of bad (incorrectly answered) questions
# Red label corresponds to AttributeID (which can be associated to a Class Name in AttributeNamePartial)
# Black label corresponds to the number of questions consistently answered incorrectly across all exams
g2 <- ggplot(data=dat.mergedQuestions, mapping=aes(x=indx, y=percentBad)) + geom_point() +
geom_text(aes(x = indx, y = (percentBad+0.03), label=AttributeID), color = "red", angle = 90, hjust=0) + ylim(-0.1,1.2) +
geom_text(aes(x = indx, y = (percentBad-0.03), label=N_bad), color = "black", angle = 90, hjust=1) +
theme_few(22) + labs(x="Ascending Rank", y="Proportion of within Attribute ID\nwith median score < 70%", title="The most difficult courses based on consistently missed questions")

# There are many questions that are answered incorrectly. Some of these questions make up a large 
# proportion of a given class (AttributeID) material, corresponding to the points further right on plot 2.
# Some classes have questions that are consistently answered incorrectly, but these questions are only
# a small fraction of material within the course (corresponding to points further left on plot 2).

# Do exams with more questions result in lower scores for students?
# To answer, we did a sliding-window analysis of scores with a window size of 30 and step size of 5.

dat.slidingWindow <- foreach(start=seq(0,max(dat.answers.wide$N_questions)-20, 3), .combine="rbind") %dopar% {
    dat.sub <- dat.answers.wide[N_questions >= start & N_questions <= (start+19)][, list(meanPercentCorrect=mean(percentCorrect, na.rm=TRUE))]   
    dat.sub[, windowStart := start]
    return(dat.sub[])
}

g3 <- ggplot(data=dat.slidingWindow, mapping=aes(x=windowStart, y=meanPercentCorrect)) + geom_point() + theme_few(22) +
labs(x="Number of questions on exam", y="Mean Percent Answers Correct", title="Test Scores as a Function of Number of Questions")

# The data from the sliding window analysis shows that the average score changes with exam length (represented by number of questions).
# Exams with intermediate numbers of questions tend to score the highest. Extremely long exams tend to have lower scores, as do very short exams.
# We can only speculate why this is: Perhaps students neglect to study for exams if they know they will be short. Also, long exams could be more taxing, reducing performance.

# We were curious if certain questions types tended to be more difficult. We looked at the distribution of percent questions answered correctly
# between foundation questiosn and application questions.

g4 <- ggplot(data=dat.merge[AttributeTypeName=="Question Complexity"], mapping=aes(x=AttributeNamePartial, y=percentCorrect)) + geom_boxplot(outlier.alpha=0) +
theme_few(18) + labs(x="Question Type", y="Percent Questions Answered Correctly", title="Application Questions are More Difficult\nthan Foundation Questions") +
ylim(0.6,1)

# plot g4 shows that application questions are slightly more difficult, having a lower distribution than foundation questions.
