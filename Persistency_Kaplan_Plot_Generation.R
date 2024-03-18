##############Install needed packages##############
install.packages("survival")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("survminer")
##########Load needed packages
library(survival)
library(openxlsx)
library(survminer)
library(ggplot2)
#################Read in data
GLP1<- read.csv("KaplanGLP1.csv")
#alternate: GLP<- read_excel(")
#################check data
View(GLP1)
#######################defining variables#######################
time <- GLP1$TotalDays
group <- GLP1$GenericTherapeuticClass
#####################Descriptive stats#######################
summary(time)
summary(group)
##################Kaplan Curve Fit########################3
kmsurvival1 <- survfit(Surv(time, ) ~group)
summary(kmsurvival1)
##########Plot generation##############
ggsurvplot(
  kmsurvival1,
  data = GLP1,
  pval = TRUE,  # Include p-value
  xlab = "Time",
  ylab = "Proportion on medication",
  title = "Persistency on Medication",
  surv.median.line = "v",  # Add median adherence line by day
  risk.table = FALSE, #put TRUE here if you want a Risk table
  risk.table.title = "Risk Table",
  palette = c("red", "cyan")  # Set custom colors for groups
) 
