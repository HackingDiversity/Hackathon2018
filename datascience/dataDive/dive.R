data <- read_csv("data.csv")
exams <- data[data$ObjectTypeName == "Exam",]
unique(exams$ExamID)


questions <- data[data$ObjectTypeName == "Question",]

#check for duplicated exam id and question ids*****STUCKS!!!!
#dups <- questions[c('QuestionID','ExamID')]
dates <- questions[,c("DateStart","DateEnd")]

complete <- dates[complete.cases(dates),]
dates <- dates[which(dates$DateEnd!='NULL'),]
dates <- dates[which(dates$DateStart!='NULL'),]

dates$DateStart <- as.Date(dates$DateStart)
dates$DateEnd <- as.Date(dates$DateEnd)
dates <- na.omit(dates)

dates$diff <- dates$DateEnd - dates$DateStart
hist(as.numeric(dates$diff))

#unique exam IDs
unique(questions$ExamID)

#analyze question correct numbers...etc.
questions$A <-'A'
questions$B <-'B'
questions$C <-'C'
questions$D <-'D'
questions$E <-'E'
questions$F <-'F'
questions$G <-'G'
questions$H <-'H'
questions$I <-'I'
questions$J <-'J'

answers <-c(sum(questions$CountA),sum(questions$CountB),sum(questions$CountC),sum(questions$CountD),sum(questions$CountE),sum(questions$CountF),sum(questions$CountG),sum(questions$CountH),sum(questions$CountI),sum(questions$CountJ))


questions$A <- as.integer(as.logical(grepl('A',questions$AcceptableAnswers)))
questions$B <- as.integer(as.logical(grepl('B',questions$AcceptableAnswers)))
questions$C <- as.integer(as.logical(grepl('C',questions$AcceptableAnswers)))
questions$D <- as.integer(as.logical(grepl('D',questions$AcceptableAnswers)))
questions$E <- as.integer(as.logical(grepl('E',questions$AcceptableAnswers)))
questions$F <- as.integer(as.logical(grepl('F',questions$AcceptableAnswers)))
questions$G <- as.integer(as.logical(grepl('G',questions$AcceptableAnswers)))
questions$H <- as.integer(as.logical(grepl('H',questions$AcceptableAnswers)))
questions$I <- as.integer(as.logical(grepl('I',questions$AcceptableAnswers)))
questions$J <- as.integer(as.logical(grepl('J',questions$AcceptableAnswers)))


questions$CountA <- as.numeric(questions$CountA)
questions$CountA[is.na(questions$CountA)] <- 0
questions$CountB <- as.numeric(questions$CountB)
questions$CountB[is.na(questions$CountB)] <- 0
questions$CountC <- as.numeric(questions$CountC)
questions$CountC[is.na(questions$CountC)] <- 0
questions$CountD <- as.numeric(questions$CountD)
questions$CountD[is.na(questions$CountD)] <- 0
questions$CountE <- as.numeric(questions$CountE)
questions$CountE[is.na(questions$CountE)] <- 0
questions$CountF <- as.numeric(questions$CountF)
questions$CountF[is.na(questions$CountF)] <- 0
questions$CountG <- as.numeric(questions$CountG)
questions$CountG[is.na(questions$CountG)] <- 0
questions$CountH <- as.numeric(questions$CountH)
questions$CountH[is.na(questions$CountH)] <- 0
questions$CountI <- as.numeric(questions$CountI)
questions$CountI[is.na(questions$CountI)] <- 0
questions$CountJ <- as.numeric(questions$CountJ)
questions$CountJ[is.na(questions$CountJ)] <- 0


questions$correct <- questions$A * as.numeric(questions$CountA) + questions$B * as.numeric(questions$CountB)+ questions$C * as.numeric(questions$CountC)+ questions$D * as.numeric(questions$CountD)+ questions$E* as.numeric(questions$CountE)+ questions$F * as.numeric(questions$CountF)+ questions$G * as.numeric(questions$CountG)+ questions$H * as.numeric(questions$CountH)+ questions$I * as.numeric(questions$CountI) + questions$J * as.numeric(questions$CountJ)
questions$total <- questions$CountA + questions$CountB +questions$CountC +questions$CountD +questions$CountE +questions$CountF +questions$CountG +questions$CountH +questions$CountI +questions$CountJ
questions$incorrect <- questions$total - questions$correct
questions$percentCorrect <- questions$correct / questions$total

#frequency table of total answers





myVector <- c(A = 1, B = 2, C = 3, D = 4)
par(las = 3)
barplot(rbind(myVector))



