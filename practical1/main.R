library(editrules)

setwd("practical1")
df <- read.table("people.txt", header=TRUE)
attach(df)
E <- editset(expression(
  age >= 0,
  age <= 150,
  age >= yearsmarried,
  status %in% c("married", "single", "widowed"),
  if (age <= 18) agegroup %in% c("child"),
  if (age >= 19 && age <= 64) agegroup %in% c("adult"),
  if (age >= 65) agegroup %in% c("elderly")
))

sm <- violatedEdits(E, df)
summary(sm)
plot(sm)
