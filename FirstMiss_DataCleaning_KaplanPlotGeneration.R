##############Install needed packages##############
install.packages("survival")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("survminer")
install.packages("dplyr")
##########Load needed packages
library(survival)
library(openxlsx)
library(survminer)
library(ggplot2)
library(dplyr)
#################Read in data
GLP<- read.csv("GLPClaims2021.csv") 
#alternate: GLP<- read_excel(")
View(GLP)

####Clean the data to remove duplicates and map first miss for KMPlot################
#Will need to edit to your data###
filtered_df_1<- GLP %>%
  group_by(Member) %>%
  filter(FirstMiss == 1) %>%
  arrange(Cumulative.Days) %>%
  slice_head(n = 1)
View(filtered_df_1)

filtered_df_0 <- GLP %>%
  group_by(Member) %>%
  mutate(all_zero_firstmiss = all(FirstMiss == 0)) %>%
  filter(all_zero_firstmiss) %>%
  arrange(desc(FirstMiss), desc(Cumulative.Days)) %>%
  slice_head(n = 1)

View(filtered_df_0)

GLP_final<- bind_rows(filtered_df_1, filtered_df_0)%>%
  arrange(Member)

View(GLP_final)

#######################defining variables#######################
time <- GLP_final$Cumulative.Days
event <- GLP_final$FirstMiss #if there is an event that intervention is mapping  
group <- GLP_final$Generic.Therapeutic.Class
#####################Descriptive stats#######################
summary(time)
summary(event) 
summary(group)
##################Kaplan Curve Fit########################3
kmsurvival2<- survfit(Surv(time,event) ~group)  ##if mapping first miss
summary(kmsurvival2)
##########Plot generation For First Missed Adherence##############
ggsurvplot(
  kmsurvival2, 
  data = GLP,
  pval = TRUE,  # Include p-value
  xlab = "Time",
  ylab = "Proportion without adherence miss",
  title = "Persistency on Medication to first missed adherence",
  surv.median.line = "hv",  # Add median adherence line by day
  risk.table = TRUE, #put TRUE here if you want a Risk table
  risk.table.title = "Risk Table",
  palette = c("red", "cyan")  # Set custom colors for groups
) 

