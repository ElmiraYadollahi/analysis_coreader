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
day1_df <-
  read_csv(
    "Documents/CODING/DATA_ANALYSIS/CoReader/analysis_coreader/all_correction_status_Day1_within.csv",
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
    "Documents/CODING/DATA_ANALYSIS/CoReader/analysis_coreader/all_correction_status_Day2_within.csv",
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
### checking that the number of correctly identified mistakes doesnt discrease with time
gghistogram(df, x = "Mistake_Order", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"),
            add = "mean", rug = TRUE)

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

### Frequency of correction status by type of mistakes and book level
df_type = with(df, table(Mistake_Type,Correction_Status, Book_Level))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Book_Level", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Mistake_Type")

### Frequency of correction status by type of mistakes and hand condition
df_type = with(df, table(Correction_Status,Mistake_Type, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 3)*100
df_type
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Mistake_Type", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Robot_Hand_Condition")

### Frequency of correction status by type of mistakes and hand condition
df_type = with(df, table(Mistake_Type,Correction_Status, Robot_Hand_Condition))
df_type = prop.table(df_type, margin = 3)*100
df_type = data.frame(df_type)
p = ggbarplot(df_type, x = "Robot_Hand_Condition", y = "Freq", fill = "Correction_Status",  palette = c("#00AFBB", "#E7B800"))
facet(p, facet.by = "Mistake_Type")


#############################################################3
# Aggreation of mistake status by kids
df_kids = with(df, table(Child_ID,Mistake_Type))
df_kids = data.frame(df_kids)
