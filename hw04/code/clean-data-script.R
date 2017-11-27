source("functions.R")
raw_data <- read.csv("../data/rawdata/rawscores.csv", colClasses=c(rep("numeric",16))) 

sink("../output/summary_rawscores.txt")
str(raw_data)
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data[,i]))
}
sink()

for (i in 1 : ncol(raw_data)) {
  current_col <- raw_data[,i]
  for (j in 1 : nrow(raw_data)) {
    if (is.na(current_col[j])) {
      current_col[j] <- 0
    }
  }
  raw_data[,i] <- current_col
}

raw_data$QZ1 <- rescale100(raw_data$QZ1,0, 12)
raw_data$QZ2 <- rescale100(raw_data$QZ2,0, 18)
raw_data$QZ3 <- rescale100(raw_data$QZ3,0, 20)
raw_data$QZ4 <- rescale100(raw_data$QZ4,0, 20)
raw_data$Test1 <- rescale100(raw_data$EX1, 0, 80)
raw_data$Test2 <- rescale100(raw_data$EX2, 0, 90)

homework <- raw_data[1:9]
a <- homework[1,]
b <- as.vector(a ,mode='numeric')
hws <- c(score_homework(b, drop = TRUE))
for (i in 2 : nrow(raw_data)) {
  hws <- c(hws, score_homework(as.vector(homework[i,], mode = "numeric"), drop = TRUE))
}
raw_data$Homework <- hws

quiz <- raw_data[11 : 14]
a <- quiz[1,]
b <- as.vector(a ,mode='numeric')
quizzes <- c(score_quiz(b, drop = TRUE))
for (i in 2 : nrow(raw_data)) {
  quizzes <- c(quizzes, score_quiz(as.vector(quiz[i,], mode = "numeric"), drop = TRUE))
}
raw_data$Quiz <- quizzes

att <- raw_data[10]
a <- att[1,]
b <- as.vector(a ,mode='numeric')
labs <- c(score_lab(b))
for (i in 2 : nrow(raw_data)) {
  labs <- c(labs, score_lab(as.vector(att[i,], mode = "numeric")))
}

raw_data$Lab <- labs

raw_data$Overall <- 0.1 * raw_data$Lab + 0.3 * raw_data$Homework + 0.15 * raw_data$Quiz +0.2 * raw_data$Test1 + .25 * raw_data$Test2

auto_grade <- function(score){
  if (score < 50) {
    "F"
  } else if (score < 60) {
    "E"
  } else if (score < 70) {
    "C-"
  } else if (score < 77.5) {
    "C"
  } else if (score < 79.5) {
    "C+"
  } else if (score < 82) {
    "B-"
  } else if (score < 86) {
    "B"
  } else if (score < 88) {
    "B+"
  } else if (score < 90) {
    "A-"
  } else if (score < 95) {
    "A"
  } else {
    "A+"
  }
}
  
overall <- raw_data["Overall"]
a <- overall[1,]
b <- as.vector(a ,mode='numeric')
grades <- c(auto_grade(b))
for (i in 2 : nrow(raw_data)) {
  grades <- c(grades, auto_grade(as.vector(overall[i,], mode = "numeric")))
}

raw_data$Grade <- grades



sink("../output/Lab-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Lab))
}
sink()

sink("../output/Homework-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Homework))
}
sink()

sink("../output/Quiz-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Quiz))
}
sink()

sink("../output/Test1-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Test1))
}
sink()

sink("../output/Test2-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Test2))
}
sink()

sink("../output/Overall-stats.txt")
for (i in 1 : ncol(raw_data)) {
  print_stats(summary_stats(raw_data$Overall))
}
sink()

sink("../output/summary-cleanscores.txt")
str(raw_data)
sink()

write.csv(raw_data, "../data/cleandata/cleanscores.csv")
