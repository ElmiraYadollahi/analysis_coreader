library(readr)
library(ggpubr)
library(plyr)

########################### Functions
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

##########################################################################

################ DATA PREP
setwd("/home/yadollah/Dropbox/2.PhD/Experiment_Geneve/Analysis/Categorizaton/")

day1_df <-
  read_csv(
    "all_correction_status_Day1_within.csv",
    col_types = cols(
      Book_Level = col_factor(levels = c("High", "Low")),
      Correction_Status = col_factor(levels = c("Corrected",
                                                "Not_Corrected")),
      Exp_Day = col_factor(levels = c("D1",
                                      "D2")),
      Mistake_Type = col_factor(levels = c("T1",
                                           "T2", "T3", "T4")),
      Robot_Hand_Condition = col_factor(levels = c("Pointing",
                                                   "Not_Pointing"))
    )
  )

day2_df <-
  read_csv(
    "all_correction_status_Day2_within.csv",
    col_types = cols(
      Book_Level = col_factor(levels = c("High", "Low")),
      Correction_Status = col_factor(levels = c("Corrected",
                                                "Not_Corrected")),
      Exp_Day = col_factor(levels = c("D1",
                                      "D2")),
      Mistake_Type = col_factor(levels = c("T1",
                                           "T2", "T3", "T4")),
      Robot_Hand_Condition = col_factor(levels = c("Pointing",
                                                   "Not_Pointing"))
    )
  )


df <- rbind(day1_df,day2_df)
##########################################################################

##########################################################################

### Frequency of correction status by type 2 of mistakes and book level
df_type = with(df, table(Correction_Status,Mistake_Type_2, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type_2", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Book_Level")





############################################################################################
### checking that the number of correctly identified mistakes doesnt discrease with time
gghistogram(df, x = "Mistake_Order", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"),
            add = "mean", rug = TRUE)

### Frequency of correction status by hand condition
df_type = with(df, table(Correction_Status, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 2)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Robot_Hand_Condition", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
p

### Frequency of correction status by hand condition and book_level
df_type = with(df, table(Correction_Status, Robot_Hand_Condition, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Robot_Hand_Condition", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Book_Level")


### Frequency of correction status by type of mistakes
df_type = with(df, table(Correction_Status,Mistake_Type))
df_type = prop.table(df_type, margin = 2)*100
df_type = data.frame(df_type)
ggbarplot(df_type, x = "Mistake_Type", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))

### Frequency of correction status by type of mistakes and book level
df_type = with(df, table(Correction_Status,Mistake_Type, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Book_Level")

### Frequency of correction status by type 2 of mistakes and book level
df_type = with(df, table(Correction_Status,Mistake_Type_2, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type_2", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Book_Level")

### Frequency of correction status by type of mistakes and book level
df_type = with(df, table(Mistake_Type,Correction_Status, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Book_Level", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Mistake_Type")

### Frequency of correction status by type 2 of mistakes and book level
df_type = with(df, table(Mistake_Type_2,Correction_Status, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Book_Level", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Mistake_Type_2")


### Frequency of correction status by type of mistakes and hand condition
df_type = with(df, table(Correction_Status,Mistake_Type, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Robot_Hand_Condition")

### Frequency of correction status by type of mistakes and hand condition
df_type = with(df, table(Correction_Status,Mistake_Type_2, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type_2", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Robot_Hand_Condition")


### Frequency of correction status by type of mistakes and hand condition
df_type = with(df, table(Mistake_Type,Correction_Status, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Robot_Hand_Condition", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Mistake_Type")


#############################################################3
# Aggreation of mistake status by kids
df_kids = with(df, table(Child_ID,Mistake_Type, Correction_Status, Robot_Hand_Condition, Book_Level))
df_kids = data.frame(df_kids)
df_kids
df_kids_corrected =  subset(df_kids, df_kids$Correction_Status == "Corrected")
#df_kids = df_kids_corrected
ggbarplot(df_kids, x = "Robot_Hand_Condition", y = "Freq", add = "mean_se",
           fill="Correction_Status", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)
ggbarplot(df_kids, x = "Correction_Status", y = "Freq", add = "mean_se",
          fill="Robot_Hand_Condition", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)

ggbarplot(df_kids, x = "Robot_Hand_Condition", y = "Freq", add = "mean_se",
           fill="Mistake_Type", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)

ggbarplot(df_kids, x = "Robot_Hand_Condition", y = "Freq", add = "mean_se",
          fill="Mistake_Type", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)

ggbarplot(df_kids, x = "Mistake_Type", y = "Freq", add = "mean_se",
          fill="Robot_Hand_Condition", palette = "aaas",
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)

df_kids = df_kids_corrected
ggbarplot(df_kids, x = "Book_Level", y = "Freq", add = "mean_se",
          fill="Robot_Hand_Condition", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Robot_Hand_Condition), label = "p.signif", label.y = 0.9)

df_kids_corrected =  subset(df_kids, df_kids$Correction_Status == "Corrected")
ggbarplot(df_kids_corrected, x = "Book_Level", y = "Freq", add = "mean_ci",
          fill="Book_Level", palette = "aaas", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = Book_Level), label = "p.signif", label.y = 0.9)

#################################33 STAT TEST
#### Test pointing effect on all children
res = table(df$Robot_Hand_Condition, df$Correction_Status) 
res
chisq.test(res) 
#### Test pointing effect on low level kids
df_low = subset(df,df$Book_Level == "Low")
res = table(df_low$Robot_Hand_Condition, df_low$Correction_Status) 
res
chisq.test(res) 
#### Test pointing effect on high level kids
df_high = subset(df,df$Book_Level == "High")
res = table(df_high$Robot_Hand_Condition, df_high$Correction_Status) 
res
chisq.test(res) 
#### Test pointing effect for each type of mistakes
###### T1
df_t1 = subset(df,df$Mistake_Type == "T1")
res = table(df_t1$Robot_Hand_Condition, df_t1$Correction_Status) 
res
chisq.test(res) 
###### T2
df_t2 = subset(df,df$Mistake_Type == "T2")
res = table(df_t2$Robot_Hand_Condition, df_t2$Correction_Status) 
res
chisq.test(res) 
###### T3
df_t3 = subset(df,df$Mistake_Type == "T3")
res = table(df_t3$Robot_Hand_Condition, df_t3$Correction_Status) 
res
chisq.test(res) 
###### T4
df_t4 = subset(df,df$Mistake_Type == "T4")
res = table(df_t4$Robot_Hand_Condition, df_t4$Correction_Status) 
res
chisq.test(res) 

##### Test pointing effect according to Mistake_Type_2
###### S1
df_s1 = subset(df,df$Mistake_Type_2 == "S1")
res = table(df_s1$Robot_Hand_Condition, df_s1$Correction_Status) 
res = prop.table(res,1)*100
res
chisq.test(res) 
###### S2
df_s2 = subset(df,df$Mistake_Type_2 == "S2")
res = table(df_s2$Robot_Hand_Condition, df_s2$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 
###### S3
df_s3 = subset(df,df$Mistake_Type_2 == "S3")
res = table(df_s3$Robot_Hand_Condition, df_s3$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 

##### Test pointing effect according to Mistake_Type_2 
######### Low
###### S1 and Low
df_s1 = subset(df,df$Mistake_Type_2 == "S1" & df$Book_Level == "Low")
res = table(df_s1$Robot_Hand_Condition, df_s1$Correction_Status) 
res = prop.table(res,1)*100
res
chisq.test(res) 
###### S2 and Low
df_s2 = subset(df,df$Mistake_Type_2 == "S2" & df$Book_Level == "Low")
res = table(df_s2$Robot_Hand_Condition, df_s2$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 
###### S3 and Low
df_s3 = subset(df,df$Mistake_Type_2 == "S3" & df$Book_Level == "Low")
res = table(df_s3$Robot_Hand_Condition, df_s3$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 

######### High
###### S1 and High
df_s1 = subset(df,df$Mistake_Type_2 == "S1" & df$Book_Level == "High")
res = table(df_s1$Robot_Hand_Condition, df_s1$Correction_Status) 
res = prop.table(res,1)*100
res
chisq.test(res) 
###### S2 and Low
df_s2 = subset(df,df$Mistake_Type_2 == "S2" & df$Book_Level == "High")
res = table(df_s2$Robot_Hand_Condition, df_s2$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 
###### S3 and Low
df_s3 = subset(df,df$Mistake_Type_2 == "S3" & df$Book_Level == "High")
res = table(df_s3$Robot_Hand_Condition, df_s3$Correction_Status) 
res = prop.table(res,1)*100
chisq.test(res) 
