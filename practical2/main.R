library(editrules)

setwd("practical2")

df <- read.csv("dirty_iris.csv")
df.complete <- df[complete.cases(df), ]
print(paste(
  "Complete cases are: ",
  nrow(df.complete),
  " and their percentage: ",
  nrow(df.complete) / nrow(df) * 100,
  "%",
  sep = ""
))

attach(df.complete)

E <- editfile("rules.txt")

violations <- violatedEdits(E, df.complete)
summary(violations)
plot(violations)

boxplot(df.complete$Sepal.Length)
