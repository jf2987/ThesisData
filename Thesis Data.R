## Thesis Missing Data Analysis 



# set working directory
setwd("C:/Users/cogps/Desktop/Thesis Analysis/Thesis Data Finalized")


## reading Data from 1st University 

University_1<-read.csv("University_1.csv")

## reading data from 2nd University
University_2 <-read.csv("University_2.csv")


## Mark empty cells as NA

University_1[ University_1 == "" ] <- NA

University_2[ University_2 == "" ] <- NA

## Delete the first two rows for they are labels
# https://stackoverflow.com/questions/7541610/how-to-delete-the-first-row-of-a-dataframe-in-r

View(head(University_1, n=2))
University_1 <- tail(University_1, -2)
View(head(University_1, n=2))


View(head(University_2, n=2))
University_2 <- tail(University_2, -2)
View(head(University_2, n=2))


## Delete person who answered 1 in Q81- this means they are younger than 
## 18

table(University_2$Q81) 


University_2<-University_2[!(University_2$Q81=="Less than 18 years of age"),]

table(University_2$Q81)

# the 18 or older question was not available for University_1
# for their subject pool consists of 18 year olds and older only


## Create a variable per data set for School
University_2['School'] <- "University_2"
University_1['School'] <- "University_1"

## Delete first 4 columns of University_2 - they are not needed and 
## were provided by Qualtrics default 
# these are "start date, end date, status, and IPAddress"

library(dplyr)
# https://datacornering.com/dplyr-error-in-select-unused-argument/
# Herein, I am deleting 4 columns including the start and end date
# for future reference, I might want to keep these columns -- 
## to explore the impact dates could have had on the study data. 

names(University_2[,1:7])
University_2<-University_2 %>% dplyr::select(-(1:4))
names(University_2[,1:7])

# delete additional default variables and the one that tells us wheteher
# participants are 18 years or older for now they all are now
# these are columns titled "Duration..in.seconds." "Finished" "RecordedDate" 
#"ResponseId", "RecipientLastName" RecipientFirstName"   
#"RecipientEmail","ExternalReference","LocationLatitude"     
#"LocationLongitude","DistributionChannel", "UserLanguage"         
#"Q81" 

names(University_2[,1:14])
University_2<-University_2 %>% dplyr::select(-(2:14))
names(University_2[,1:14])

## Delete similar columns for University_1 as for University_2
## since this was done in University_1 by an additional researcher
## their exported Qualtrics data looks slightly different than University_2
## "StartDate" "EndDate"   "Status"

names(University_1[,1:3])
University_1<-University_1 %>% dplyr::select(-(1:3))
names(University_1 [,1:3])


names(University_1 [,1:8])
#"Duration..in.seconds." "Finished","RecordedDate"         
#"ResponseId","DistributionChannel", "UserLanguage""Consent"

University_1<-University_1 %>% dplyr::select(-(2:8))
names(University_1 [,1:8])

# Merge into one single Data frame
# https://www.statology.org/r-combine-two-data-frames-with-different-columns/

library(dplyr)

Total <- bind_rows(University_2, University_1)

View(Total)

## Creating a Variable based on "and" conditions to identify what 
## of the 2 conditions subjects were exposed to
## https://datacornering.com/ifelse-and-na-problem-in-r/

View(Total)

Total$Condition <- ifelse(!is.na (Total$Q297) & !is.na (Total$Q298),
                                                 "BTW",
                                                 "wm")
table(as.factor(Total$Condition))

# this is the question that asked participants for the race
# of the subject of the vignette -- this is the Black transgender
## Race Question of Manipulation. BTW condition 
table(as.factor(Total$Q297))
## 37 failed the attention check

## BTW condition gender identity question 
table(as.factor(Total$Q298))
## 37 failed this second attention check

## wtm Race manipulation check 
table(as.factor(Total$Q293))
        ## 18 failed the attention check 

## wtm Race manipulation check 
table(as.factor(Total$Q294))
## 34 failed the attention check 

### Re-Coding Reverse Coded Items 
# symbolic racism - recoding 1,2,4,8 
# 1=4, 2=3, 3=2, 4=1

# # SR1.2_1 (question 1)
table(as.factor(Total$SR1.2_1))

# current coding
# Somewhat Agree Somewhat Disagree    Strongly Agree Strongly Disagree
# https://www.youtube.com/watch?v=bsOrBu1vucQ

# Recoding word responses to the original numerical values given in the 
## scale 
# https://condor.depaul.edu/phenry1/SR2Kinstructions.htm
# https://stackoverflow.com/questions/47521920/recode-multiple-columns-using-dplyr

# Recoding for items with the same response format: 1,2,6,7,8 for this scale
# has multiple types of questions with different response formats

library(plyr)
levels(as.factor(Total$SR1.2_1))



# colSums(is.na(Total))

# figure out the items of interest column index of items 
## 1,2,6,7,8
names(Total)

# recoding the responses into its numerical codes 
Total <- Total %>% mutate_at(c(30:31,35:37), list(~recode(.,"Strongly Agree"=1, 
                                                         "Somewhat Agree"=2, 
                                                         "Somewhat Disagree"=3, 
                                                         "Strongly Disagree" = 4)))
table(Total$SR1.2_1)

## the vast majority said they strongly disagreed with this item
## "It's really a matter of some people not trying hard 
## enough; if blacks would only try harder they could be just as well off as whites."
## this is good, it suggests less racial bias

par(mar = c(1, 1, 1, 1))
## this is a terrible plot of the responses
## i need to use GGplot2 next time
plot(as.factor(Total$SR1.2_1))
table(as.factor(Total$SR1.2_1))

# # Recoding for items  3 to fit Scale coding 
names(Total)
table(as.factor(Total$SR3_1))
## "Some say that black leaders have been trying 
## to push too fast.  Others feel that they haven't pushed fast enough.  What do you think?"  

Total <- Total %>% mutate_at(c(32), list(~recode(.,"Trying to push very much too fast"=1, 
                                                          "Going too slowly"=2, 
                                                          "Moving at about the right speed"=3)))

table(as.factor(Total$SR3_1))
plot(as.factor(Total$SR3_1))
## Overall, very few stated that Black leaders were moving too 
# fast


## recoding item 4
## "How much of the racial tension that exists 
### in the United States today do you think blacks are responsible for creating?"

names(Total)
# table(as.factor(Total$SR4_1))
Total <- Total %>% mutate_at(c(33), list(~recode(.,"All of it"=1, 
                                                 "Most"=2, 
                                                 "Some"=3,
                                                 "Not much at all"=4)))

table(as.factor(Total$SR4_1))
#There is alot more variability in this item in the 3's and 4's
plot(as.factor(Total$SR4_1))


# recoding item 5, SR5_1
## "How much discrimination against blacks do you 
## feel there is in the United States today, limiting 
# their chances to get ahead?"

names(Total)
table(as.factor(Total$SR5_1))
Total <- Total %>% mutate_at(c(34), list(~recode(.,"A lot"=1, 
                                                 "Some"=2, 
                                                 "Just a Little"=3,
                                                 "None at all"=4)))

plot(as.factor(Total$SR5_1))
# alot more variability in this question but overall the same trend


## Changing the variable for the Symbolic Racism scale
## to make them more interpretable 

names(Total)[30:37]<-paste0('SimRac', 1:8)

## Recoding reverse coded items in the symbolic racism scale
## These items include 1, 2, 4, 8, 3
# https://condor.depaul.edu/phenry1/SR2Kinstructions.htm
## the recoding format is exactly as instructed by Henry and Sears for their scale
# "....items 1, 2, 4, and 8 need to be recoded so that a 1 = 4, 2 = 3, 3 = 2, and 4 = 1." 

names(Total) # to get 1, 2, 4, and 8 index
# 30:31, 33, 37
table(as.factor(Total$SimRac1))

Total[,c(30,31,33,37)] <- 5-Total[,c(30,31,33,37)]

table(as.factor(Total$SimRac1))

# Recoding item 3 of Simbolic Racism Scale

names(Total)
table(as.factor(Total$SimRac3))

Total[,c(32)] <- 4-Total[,c(32)]

table(as.factor(Total$SimRac3))

## the Symbolic Racism Scale has been re-coded and is Therefore clean

## Create an Average score for Racism for Participants
# https://www.statology.org/average-across-columns-in-r/
# finding column index
# names(Total)
# TestRun<-Total
# TestRun$SymRac_mean<-rowMeans(TestRun[ , c(30:37)]) 
# 
# ## Doing Anova between conditions
# 
# one.way <- aov(SymRac_mean ~ Condition, data = TestRun)
# 
# summary(one.way)
# 
# # p < .001
# 
# tapply(TestRun$SymRac_mean, TestRun$Condition, mean, na.rm=TRUE)
# 
# tapply(TestRun$SymRac_mean, TestRun$Condition, sd, na.rm=TRUE)

# oddly enough they are more racist in the white man condition 

## Renaming Separate Spheres Ideology Scale Items
names(Total)
names(Total)[6:20]<-paste0('SepSpher', 1:15)

## Recoding Separate Spheres Ideology 

names(Total)
levels(as.factor(Total$SepSpher1))
table(Total$SepSpher1)
Total <- Total %>% mutate_at(c(6:20), list(~recode(.,"Strongly Disagree"=1, 
                                                 "Moderately Disagree"=2, 
                                                 "Slightly Disagree"=3,
                                                 "Neither Agree nor Disagree"=4,
                                                 "Slightly Agree"=5,
                                                 "Moderately Agree"=6,
                                                 "Strongly Agree"=7)))
table(Total$SepSpher1)
## SepSpher3

## Recoding Separate Spheres Ideology Scale items 3 ,5, 8, 13, 14 
# for they are reverse coded 
# https://psycnotes.wordpress.com/how-to-recode-in-r/
names(Total)
Total[,c(8,10,13,18,19)] <- 8-Total[,c(8,10,13,18,19)]

table(as.factor(Total$SepSpher3))

data.frame(colnames(Total))

## Create an Average score for the Sexism Scale
# https://www.statology.org/average-across-columns-in-r/
# finding column index
# names(Total)
# TestRun<-Total
# TestRun$Sexism_mean<-rowMeans(TestRun[ , c(6:20)]) 
# 
# ## Doing Anova between conditions
# 
# one.way <- aov(Sexism_mean ~ Condition, data = TestRun)
# 
# summary(one.way)
# 
# # p < .001
# 
# tapply(TestRun$Sexism_mean, TestRun$Condition, mean, na.rm=TRUE)
# 
# tapply(TestRun$Sexism_mean, TestRun$Condition, sd, na.rm=TRUE)
# 
# # oddly enough they are more sexist in the white man condition 
# 

## Renaming Transphobia Scale
names(Total)[21:29]<-paste0('TransPH', 1:9)
data.frame(colnames(Total))
View(Total)
# This scale has no reverse coded items
# names(Total)
# TestRun<-Total
# 
# ## convert the values from character to numeric
# ## https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
# library(dplyr)
# TestRun <- TestRun %>% 
#     mutate_at(c(21:29), as.numeric)
# 
# TestRun$Transphobia_mean<-rowMeans(TestRun[ , c(21:29)])
# 
# ## Doing Anova between conditions
# 
# one.way <- aov(Transphobia_mean ~ Condition, data = TestRun)
# 
# summary(one.way)
# 
# # p < .001
# 
# tapply(TestRun$Transphobia_mean, TestRun$Condition, mean, na.rm=TRUE)
# 
# tapply(TestRun$Transphobia_mean, TestRun$Condition, sd, na.rm=TRUE)
# # for this one-- folks were more transphobic in the 
# ##  Btw condition


## Renaming Sub scales of Political Ideology
## this one has no reverse coded items
## and no re-coding is necessary for this scale

    ## Liberals
names(Total)[38:43]<-paste0('PolOr_Lib', 1:6)
data.frame(colnames(Total))
    
    ##Conservative
names(Total)[44:49]<-paste0('PolOr_Con', 7:12)
data.frame(colnames(Total))

    ## Moderate
names(Total)[50:55]<-paste0('PolOr_Mod', 13:18)
data.frame(colnames(Total))

## COVID Scale Subscales Subscale Naming 
    ## Trust in Government and Media
names(Total)[56:58]<-paste0('COVID_TrustGovMEdia', 1:3)
data.frame(colnames(Total))
    ## Trust in Interpersonal Communicaiton
names(Total)[59:60]<-paste0('COVID_TrustInterper', 4:5)
data.frame(colnames(Total))
    ## Understanding Transmission
names(Total)[61]<-paste0('COVID_UnderTransm', 6)
data.frame(colnames(Total))
    ## COVID prevention Confidence 
names(Total)[62]<-paste0('COVID_PrevConf', 7)
data.frame(colnames(Total))
    ## Perceived Personal Suceptibility
names(Total)[63:64]<-paste0('COVID_PerSuc', 8:9)
data.frame(colnames(Total))
    ## Worry About Contracting COVID
names(Total)[65]<-paste0('COVID_WorrContr', 10)
data.frame(colnames(Total))
    ## Hand Hygiene
names(Total)[66:69]<-paste0('COVID_HandHygi', 11:14)
data.frame(colnames(Total))
    ## Social Distancing Behavior
names(Total)[70:73]<-paste0('COVID_SocialDist', 15:18)
data.frame(colnames(Total))


## Recoding COVID Scales
data.frame(colnames(Total)) ## roughly columns 66 to 69 for Hand Hygiend

levels(as.factor(Total$COVID_HandHygi11))

View(Total)
## the hand hygience and social distance subscales are not in the numerical
## response format. SO, they have to be recoded
names(Total)

## recoding Hand Hygiene

Total <- Total %>% mutate_at(c(66:69), list(~recode(.,"never"=1, 
                                                   "sometimes"=2, 
                                                   "usually"=3,
                                                   "always"=4)))

levels(as.factor(Total$COVID_HandHygi11))

## Recoding Covid Social Distance 70 73

table(as.factor(Total$COVID_SocialDist15))

Total <- Total %>% mutate_at(c(70:73), list(~recode(.,"No"=1, 
                                                    "Yes"=2)))

table(as.factor(Total$COVID_SocialDist15))

## Recoding the Manipulation Check's and The Hypothesis Guess

## Black Transgender Woman Manipulation Check
names(Total)
names(Total)[74]<-paste0('ManipulationCheck_BTW')
levels(as.factor(Total$ManipulationCheck_BTW))
data.frame(colnames(Total))

## white man Manipulation Check
names(Total)[75]<-paste0('ManipulationCheck_wm')
levels(as.factor(Total$ManipulationCheck_wm))
data.frame(colnames(Total))


## Hypothesis Guess
names(Total)[76]<-paste0('Hypothesis Guess')
levels(as.factor(Total$`Hypothesis Guess`))
data.frame(colnames(Total))

## Attempting to Create code for the re-coding of the columns to 

names (Total)

# Collapsing Demographic variables with "choose as many as applies to you"
# wording 

## https://stackoverflow.com/questions/22854112/how-to-skip-a-paste-argument-when-its-value-is-na-in-r
## https://www.rdocumentation.org/packages/tidyr/versions/1.1.4/topics/unite

library(tidyr)
data.frame(colnames(Total))
## So, for the Demographic Variables that allowed participants to choose
# more than one option, Qualtrics created individual variables per choice
# that which shows whether they chose that option or not.  

## So, I am creating a variable with the options chosen separated by a column

Total<-Total %>% unite("Sexuality_Collapsed",78:87, sep = ', ', na.rm = TRUE, remove = FALSE)

levels(as.factor(Total$Sexuality_Collapsed))

#write.csv(Total,"C:/Users/cogps/Desktop/CheckUp.csv")

# https://stackoverflow.com/questions/14568662/paste-multiple-columns-together
# https://stackoverflow.com/questions/13596155/join-merge-two-columns-inside-a-data-frame
# https://statisticsglobe.com/paste-multiple-columns-together-in-r
# https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas


## Make every empty space for this variable an NA

# Total[Total == ''] <- NA

## Do the same for Gender

data.frame(colnames(Total))
Total<-Total %>% unite("Gender_Collapsed",89:99, sep = ', ', na.rm = TRUE, remove = FALSE)

levels(as.factor(Total$Gender_Collapsed))

# Total[Total == ''] <- NA

## Do the same for Race and Ethnicity

data.frame(colnames(Total))
Total<-Total %>% unite("Race_Ethn_Collapsed",104:114, sep = ', ', na.rm = TRUE, remove = FALSE)

levels(as.factor(Total$Race_Ethn_Collapsed))

# Total[Total == ''] <- NA

## Do the same for Education

data.frame(colnames(Total))
Total<-Total %>% unite("Education_Collapsed",102:103, sep = ', ', na.rm = TRUE, remove = FALSE)

levels(as.factor(Total$Education_Collapsed))

# Total[Total == ''] <- NA

## Do the same for Relationship Orientation

data.frame(colnames(Total))
Total<-Total %>% unite("RelOr_Collapsed",119:120, sep = ', ', na.rm = TRUE, remove = FALSE)

levels(as.factor(Total$RelOr_Collapsed))

# Mark all created empty cells with NA
Total[Total == ''] <- NA


## Delete Condition Variables since we no longer need them 
## I Must not do this for this is relevant
# names(Total)
# Total<-Total %>% dplyr::select(-(118))
# 
# ## Delete all conditional variables
# # since we no longer need them 
# 
# names(Total)
# Total<-Total %>% dplyr::select(-(2:5))



## Get rid of ID column
data.frame(colnames(Total))
Total<-Total %>% dplyr::select(-(122))
data.frame(colnames(Total))

write.csv(Total,"C:/Users/cogps/Desktop/Thesis Analysis/Clean_Data_2022_Latest.csv", row.names=FALSE)


## some NA's are coming back as <NA> and others as NA --
## https://stackoverflow.com/questions/16253789/what-is-the-difference-between-na-and-na

## trying Little's imputation Technique 
# https://www.youtube.com/watch?v=LmyRcu75XEI
# https://stackoverflow.com/questions/66075314/bayloredpsych-package-for-rstudio-cant-install-package
# devtools::source_url("https://github.com/njtierney/naniar/blob/master/R/mcar-test.R?raw=TRUE")
library(naniar)
library(misty)

## making all variables numeric
## https://stackoverflow.com/questions/23466222/how-can-i-change-all-factor-variables-into-numeric-variables-in-a-bulk
# Test <- data.frame(lapply(Test, function(x) as.numeric(as.character(x))))




# getting a subset with only 2:69 which are the ordinal variables 
TotalScalesOnly<-Total[ , 2:69]
data.frame(colnames(TotalScalesOnly))
head(TotalScalesOnly,10)

## Making all variables numeric

TotalScalesOnly <- data.frame(lapply(TotalScalesOnly, function(x) as.numeric(as.character(x))))

head(TotalScalesOnly,10)

## seeing if the MCAR test functions work

mcar_test(TotalScalesOnly)

# p value not significant- apparently tho it gives me a warning
## "in norm::prelim.norm (data) : NAs introduced by coercion to integer range
## p value not significant-- therefore data is MCAR
# as to the warning https://mailman.stat.ethz.ch/pipermail/r-help/2006-February/087333.html
## it appears we dont have to worry about it for its a bug

na.test(TotalScalesOnly, digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE)
# p value not significant 


## figuring out the amount of missingness per column and row
## https://stackoverflow.com/questions/52240224/calculate-the-percentage-of-missing-values-per-column-using-r

library(purrr)

map(Total, ~mean(is.na(.)))

# levels(as.factor(Total$ManipulationCheck_wm))

## need to get rid of the id variable 
## i did a while back 

# names(Total)
# Total<-Total %>% dplyr::select(-(117))
# names(Total)

## I need to dummy code the string variables before I re-run the MCAR
## So, i need to code all variables within the data set as 1 if not NA, else NA

TestRun <- ifelse(!is.na (Total),1,NA)
                    

head(TestRun, 10)
                # it worked 
# make sure its a data frame

TestRun<-as.data.frame(TestRun)

## MCAR Test
mcar_test(TestRun)
            # this test is not working because now each variable is coded as 
            # as a dichotomy 
#Error: Problem with `mutate()` column `d2`.
# i `d2 = purrr::pmap_dbl(...)`.
# x Lapack routine dgesv: system is exactly singular: U[1,1] = 0
# i The error occurred in group 1: miss_pattern = 1.
# Run `rlang::last_error()` to see where the error occurred.
# In addition: Warning message:
#     In norm::prelim.norm(data) : NAs introduced by coercion to integer range
na.test(TestRun, digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE)
# Error: Following variables have no variance

na.test(Total, digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE)
# Error: Following variables have no variance:




mcar_test(Total)
            # the complete data frame (with actual numbers is non sig)
            # NA's introduced by coercion 
# the documentation of this function states that this warning will occur for 
## non numeric columns, so its fine 
    # not sig



## Visualizing Missing Data Patterns
## https://www.youtube.com/watch?v=9a3CW8fKA3k&t=359s
            # take out categorical variables
Test<- Total[ , -all_of(c(1,70:120))]
data.frame(colnames(Test))
            # check for columns missing more than 5% of data
percentmiss<-function(x){
    sum(is.na(x))/length(x)*100}
            # using the function for columns 
apply(Test, 2, percentmiss)
# no column is missing more than 5 percent 
            # check for missing percent but this time for rows
missing <-apply(Test, 1, percentmiss)
# there are around 10 rows missing 100 percent of the data
summary(missing)
table(missing)
# there are around 9 rows missing 100 percent of the data

# subset a object with the rows that are missing less than 5 percent of the data
Test_Clean<-Test[missing < 5,]

## double check for column missing values after the exclusion
## of rows with more than 5 percent missing values
Test_Clean_No_Cat<- Test_Clean[ , -all_of(c(1,70:120))]
apply(Test_Clean_No_Cat, 2, percentmiss)
        # now all of the columns have 
        # 0 percent misSing values 
        ## so, technically there is nothing to impute if the rows that are missing
# 100 percent of the data are deleted


## Now I need to re run the MCAR for the Test_Clean
## this is the data set with rows with more than 5 percent 
## missing data exluded
library(purrr)
#Test_Clean<-Test[missing < 5,]
# names (Test_Clean)
na.test(Test_Clean, digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE)
            # not significant 
names (Test_Clean)

mcar_test(Test_Clean)
            # Not significant 

apply(Test_Clean, 1, percentmiss)
            # per the subset, no rows are missing more than 5 % data


## So, since data is MCAR AND there is not enough missing data
# for multiple imputation, then the MGCFA's with covariates
# proceeds


###############################################
# Question: should I leave the rows that have the DV's 100 percent empty?
# yes 
# I at least know which school the missing rows came from- so there is still some
# info on them

# Data set cleaned
# Total_Cleaned<-Total[missing < 5,]
# 
# names(Total_Cleaned)


## Figuring out if there are rows that are completely empty 
# missing <-apply(Test, 1, percentmiss)
# table(apply(Total, 1, percentmiss))
# Total_Check<-Total[missing > 5, ]
# names(Total_Check)
# View(Total_Check)
# table(apply(Total_Check, 1, percentmiss))
# excluding the progress column 
# library(dplyr)
# names(Total_Check)
# Total_Check_1<-Total_Check %>% dplyr::select(-(1))
# table(apply(Total_Check_1, 1, percentmiss))
# table(apply(Total_Check, 1, percentmiss))
            # its not the progress bar that is accounting for 
            # the non missing portions

# Excluding the Progress Column 
## Don't do this
# library(dplyr)
# names(Total)
# Total<-Total %>% dplyr::select(-(1))
# table(apply(Total, 1, percentmiss))

## Figuring out Covariates for MGCFA

## Since my data is derived from ordinal scales, Koh & Zumbo (2008)
# state that an estimation method for such data should be applied
# p.471-472

## Preliminary Analyses-
# correlations for divergent and convergent validity

## correlation of items 6 through 20 for Separate Spheres Ideology
data.frame(colnames(Total))
# https://www.rdocumentation.org/packages/multilevel/versions/2.6/topics/item.total
#install.packages("multilevel")
library(multilevel)
levels(as.factor(Total$Condition))

# Separate Spheres Ideology Scale 
SepSpheres_ITC<-item.total(Total[,6:20])
SepSpheres_wm_ITC<-item.total(subset(Total, Total$Condition == "wm")[,6:20])
SepSpheres_BTW_ITC<-item.total(subset(Total, Total$Condition == "BTW")[,6:20])

write.csv(SepSpheres_ITC, "C:/Users/cogps/Desktop/SepSpheres_ITC.csv", row.names=FALSE)
write.csv(SepSpheres_ITC, "C:/Users/cogps/Desktop/SepSpheres_wm_ITC.csv", row.names=FALSE)
write.csv(SepSpheres_ITC, "C:/Users/cogps/Desktop/SepSpheres_BTW_ITC.csv", row.names=FALSE)

data.frame(colnames(Total))


## transphobia scale 21:29
## scale is not numeric- so i need to convert it
library(dplyr)
Total <- Total %>% 
     mutate_at(c(21:29), as.numeric)
 
Transphobia_ITC<-item.total(Total[,21:29])
Transphobia_ITC_wm<-item.total(subset(Total, Total$Condition == "wm")[,21:29])
Transphobia_ITC_BTW<-item.total(subset(Total, Total$Condition == "BTW")[,21:29])

write.csv(Transphobia_ITC, "C:/Users/cogps/Desktop/Transphobia_ITC.csv", row.names=FALSE)
write.csv(Transphobia_ITC_wm, "C:/Users/cogps/Desktop/Transphobia_ITC_wm.csv", row.names=FALSE)
write.csv(Transphobia_ITC_BTW, "C:/Users/cogps/Desktop/Transphobia_ITC_BTW.csv", row.names=FALSE)


## idk what I was trying to do here
wm$Item.Total-BTW$Item.Total

# symbolic racism 30:37
data.frame(colnames(Total))

Racism_ITC<-item.total(Total[,30:37])
Racism_ITC_wm<-item.total(subset(Total, Total$Condition == "wm")[,30:37])
Racism_ITC_BTW<-item.total(subset(Total, Total$Condition == "BTW")[,30:37])

write.csv(Racism_ITC, "C:/Users/cogps/Desktop/Racism_ITC.csv", row.names=FALSE)
write.csv(Racism_ITC_wm, "C:/Users/cogps/Desktop/Racism_ITC_wm.csv", row.names=FALSE)
write.csv(Racism_ITC_BTW, "C:/Users/cogps/Desktop/Racism_ITC_BTW.csv", row.names=FALSE)


##
wm$Item.Total-BTW$Item.Total

## Creating Row means of three main scales
Total$SymRac_mean<-rowMeans(Total[ , c(30:37)]) 
Total$Transph_mean<-rowMeans(Total[ , c(21:29)]) 
Total$Separ_Spheres_mean<-rowMeans(Total[ , c(6:20)]) 

## correlations between the three scales
# https://stackoverflow.com/questions/3798998/cor-shows-only-na-or-1-for-correlations-why
DV_Corr<-cor(x = as.matrix(subset (Total, select = c(Separ_Spheres_mean, Transph_mean, SymRac_mean))), 
    method = "pearson", use = "pairwise.complete.obs")

write.csv(DV_Corr, "C:/Users/cogps/Desktop/DV_Corr.csv", row.names=FALSE)
?cor
## Integrate a MLM analysis to MGCFA in variance testing

