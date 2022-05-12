## MGCFA 2022 Test Run
## https://www.youtube.com/watch?v=Kt2lZxcAGWs&t=2s

setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

## Delete rows that which are completely empty for the variables of interest
## https://www.r-bloggers.com/2021/06/remove-rows-that-contain-all-na-or-certain-columns-in-r/
## https://stackoverflow.com/questions/51596658/remove-rows-which-have-all-nas-in-certain-columns

# library(tidyr)

## I need to figure out what the difference between these two methods are
## this is what is impacting my latent means code

## CleanData<-CleanData %>% drop_na(6:20)

## test run drop_na Function
## https://www.r-bloggers.com/2021/06/remove-rows-that-contain-all-na-or-certain-columns-in-r/
# df=data.frame(Col1=c("A","B","C","D",
#                      "P1","P2","P3")
#               ,Col2=c(NA,8,NA,9,10,8,9)
#               ,Col3=c(NA,7,6,8,NA,7,8)
#               ,Col4=c(NA,NA,7,7,NA,7,7))
# df

# library(tidyr)
# df %>% drop_na(2:4)
## so, this one deletes them if they had NA on any of the columsn mentioned
## they dont necessarly have to have ALL of the columns NA
## this is not what I wanted

## Now test running the other code

# df=data.frame(Col1=c("A","B","C","D",
#                      "P1","P2","P3")
#               ,Col2=c(NA,8,NA,9,10,8,9)
#               ,Col3=c(NA,7,6,8,NA,7,8)
#               ,Col4=c(NA,NA,7,7,NA,7,7))
# df
# 
# df[!apply(is.na(df[,2:4]), 1, all),]
## this code worked

CleanData<-CleanData[!apply(is.na(CleanData[,6:20]), 1, all),]

names(CleanData)

## 

library(lavaan)

head(CleanData)

table(CleanData$Condition)
## with the apply function we get
## BTW 333, wm 332

## with the drop_na() we get
## 335 and 343

## the first lavaan

overall.model = ' SSIT =~ SepSpher1 + SepSpher2 + SepSpher3 + SepSpher4 + SepSpher5 + SepSpher6 + SepSpher7 + SepSpher8 + SepSpher9 + SepSpher10 + SepSpher11 + SepSpher12 + SepSpher13 + SepSpher14 + SepSpher15'

overall.fit <- cfa(model = overall.model,
                   data = CleanData, 
                   meanstructure = TRUE) ##this is important 
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

#### Make table with the results #### 

library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
kable(table_fit)
library(semTools)
library(semPlot)
## i got this function from pp. 4 of Hirschfeld & Von Brachel (2014)
moreFitIndices(overall.fit)
semPaths(overall.fit, "std")

# make a picture of the model

library(semPlot)

semPaths(overall.fit, 
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree")

## Do a model specific per condition : BTW

BTW.fit <- cfa(model = overall.model,
                   data = CleanData[CleanData$Condition=="BTW", ], 
                   meanstructure = TRUE) ##this is important 
summary(BTW.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## Make table with the results of BTW

table_fit[2, ] <- c("BTW Model", round(fitmeasures(BTW.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
## Make a semPlot 
library(semPlot)

semPaths(BTW.fit, 
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree")


## Do a model specific per condition : wm

wm.fit <- cfa(model = overall.model,
               data = CleanData[CleanData$Condition=="wm", ], 
               meanstructure = TRUE) ##this is important 
summary(wm.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## apparently only 331 of 332 rows were used for the white model-- I wonder why

table_fit[3, ] <- c("wm Model", round(fitmeasures(wm.fit, 
                                                   c("chisq", "df", "cfi",
                                                     "rmsea", "srmr")),3))
kable(table_fit)

## do a graph for wm Model
library(semPlot)

semPaths(wm.fit, 
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree")


## Configural Invariance

configural.fit <- cfa(model = overall.model,
              data = CleanData, 
              meanstructure = TRUE,
              group = "Condition") ##this is important 

summary(configural.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, 
                                                  c("chisq", "df", "cfi",
                                                    "rmsea", "srmr")),3))
kable(table_fit)

## Figure for Configural model of SSI

library(semPlot)

semPaths(configural.fit, 
         whatLabels = "std", 
         edge.label.cex = 1,
         layout = "tree")

#### Metric Invariance - setting the loadings to equal ####

metric.fit <- cfa(model = overall.model,
                      data = CleanData, 
                      meanstructure = TRUE,
                      group = "Condition",
                      group.equal=c("loadings")) ##this is important 

summary(metric.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, 
                                                          c("chisq", "df", "cfi",
                                                            "rmsea", "srmr")),3))
kable(table_fit)

    # the loadings between the groups are basically equal
 

#### Scalar Invariance -- setting the intercepts to equal####

scalar.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts")) ##this is important 

summary(scalar.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

#### Strict (Error) Invariance-- constrict the errors to be the same ####

strict.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts", "residuals")) ##this is important 
## wm 331 BTW 333

summary(strict.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)


table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)


## latent means
?lavPredict
predicted_scores <- lavPredict(strict.fit, type="ov")

table(CleanData$Condition)

View(predicted_scores)

predicted_scores <- as.data.frame(do.call(rbind, predicted_scores))

View(predicted_scores)

# the code breaks here for me -- its cuz I think there are rows
## that literally have nothing entered -- so complete rows missing for 
## the variables of interest 


#predicted_scores$Condition <- c(rep("wm", 331), rep("BTW", 333))

## is_na() -- BTW 335 and wm 343
## this is with the is_na()

predicted_scores$Condition <- c(rep("wm", 331), rep("BTW", 333))
## this was breaking because it was using the sample sizes that it used for the 
## strict model-- this can be viewed in the summary() output of said model
## my question now is-- why have some rows not acquired output for this model?

View(predicted_scores)
## 331 instead of 332 -- am unsure what is going on here 

predicted_scores$sum <- apply(predicted_scores[ , 1:15], 1, sum)
View(predicted_scores)

## Predicted Model Scores
tapply(predicted_scores$sum, predicted_scores$Condition, mean)                               
  
## Actual Data Average Score
names(CleanData)
CleanData$sum <- apply(CleanData[ , 6:20], 1, sum)

tapply(CleanData$sum, CleanData$Condition, mean, na.rm = TRUE)
?tapply
## latent means
latent_means<-lavPredict(strict.fit)
latent_means <- as.data.frame(do.call(rbind, latent_means))
# table(CleanData$Condition)

latent_means$Condition <- c(rep("wm", 331), rep("BTW", 333))
## 331 instead of 332
options(scipen=999)

View(latent_means)
# z scores
tapply(latent_means$SSIT, latent_means$Condition, mean) 

# real scores
tapply(CleanData$sum, CleanData$Condition, mean, na.rm = T)
tapply(CleanData$sum, CleanData$Condition, sd, na.rm = T)

## Calculate Effect Size

library(MOTE)
M <-tapply(CleanData$sum, CleanData$Condition, mean, na.rm = TRUE)
SD <-tapply(CleanData$sum, CleanData$Condition, sd, na.rm = TRUE)
N <- tapply(CleanData$sum, CleanData$Condition, length)

effect_size<-d.ind.t(M[1], M[2], SD[1], SD[2], N[1], N[2], a = .05)

effect_size$estimate
#"$d_s$ = -0.02, 95\\% CI [-0.17, 0.13]"

## to get rid of this error I needed to get na.rm into the tapply function
## the above code worked before but now I am getting this error. 
## Error in if (abs(ncp) > 37.62) print("The observed noncentrality parameter 
#of the noncentral t-distribution has exceeded 37.62 in magnitude 
#(R's limitation for accurate probabilities from the noncentral t-distribution) 
#in the function's iterative search for the appropriate value(s). The results 
#may be fine, but they might be inaccurate; use caution.") : 
#missing value where TRUE/FALSE needed

effect_size$statistic
# "$t$(662) = -0.27, $p$ = .788"

## Saving a CSV File with the Table with the Model Fir Indices for SepSphere Strict
write.csv(table_fit,  "C:/Users/cogps/Desktop/SepSphereFitStats.csv", row.names=FALSE)

#### Running a MGCFA for the Transphobia Scale
setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

## Find the location of the Items of Interest

## this code worked

names(CleanData)
CleanData<-CleanData[!apply(is.na(CleanData[,21:29]), 1, all),]
## 678 to 666 

library(lavaan)

table(CleanData$Condition)
## BTW 333 and wm 333


## the first lavaan
names(CleanData)
overall.model = ' TransPH =~ TransPH1 + TransPH2 + TransPH3 + TransPH4 + TransPH5 + TransPH6 + TransPH7 + TransPH8 + TransPH9'

overall.fit <- cfa(model = overall.model,
                   data = CleanData, 
                   meanstructure = TRUE) ##this is important 
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

#### Make Table ####

library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
kable(table_fit)

## Do a model specific per condition : BTW

BTW.fit <- cfa(model = overall.model,
               data = CleanData[CleanData$Condition=="BTW", ], 
               meanstructure = TRUE) ##this is important 
summary(BTW.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## 331 out of 333 were used for this model


## Make table with the results of BTW

table_fit[2, ] <- c("BTW Model", round(fitmeasures(BTW.fit, 
                                                   c("chisq", "df", "cfi",
                                                     "rmsea", "srmr")),3))
kable(table_fit)


## Do a model specific per condition : wm

wm.fit <- cfa(model = overall.model,
              data = CleanData[CleanData$Condition=="wm", ], 
              meanstructure = TRUE) ##this is important 
summary(wm.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## apparently only 330 of 333 rows were used for the white model

table_fit[3, ] <- c("wm Model", round(fitmeasures(wm.fit, 
                                                  c("chisq", "df", "cfi",
                                                    "rmsea", "srmr")),3))
kable(table_fit)


## Configural Invariance

configural.fit <- cfa(model = overall.model,
                      data = CleanData, 
                      meanstructure = TRUE,
                      group = "Condition") ##this is important 

summary(configural.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, 
                                                          c("chisq", "df", "cfi",
                                                            "rmsea", "srmr")),3))
kable(table_fit)

### MEtric Invariance

metric.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings")) ##this is important 

summary(metric.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Scalar Invariance

scalar.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts")) ##this is important 

summary(scalar.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Strict Invariance

strict.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts", "residuals")) ##this is important 
## wm 331 BTW 333

summary(strict.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)


table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## 
write.csv(table_fit,  "C:/Users/cogps/Desktop/Transphobia_FitStats.csv", row.names=FALSE)



## Fitting models with the Racism Scales Might be different 
setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

names(CleanData)
dim(CleanData)
## Delete fully empty rows
CleanData<-CleanData[!apply(is.na(CleanData[,30:37]), 1, all),]
dim(CleanData)
## goes from 678 to 667

names(CleanData)
overall.model = ' SimRac =~ SimRac1 + SimRac2 + SimRac3 + SimRac4 + SimRac5 + SimRac6 + SimRac7 + SimRac8'

library(lavaan)
overall.fit <- cfa(model = overall.model,
                   data = CleanData, 
                   meanstructure = TRUE) ##this is important 
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

#### Create Table for Symbolic Racism Fit Statistics
library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
kable(table_fit)

## Do a model specific per condition : BTW

BTW.fit <- cfa(model = overall.model,
               data = CleanData[CleanData$Condition=="BTW", ], 
               meanstructure = TRUE) ##this is important 
summary(BTW.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## Make table with the results of BTW

table_fit[2, ] <- c("BTW Model", round(fitmeasures(BTW.fit, 
                                                   c("chisq", "df", "cfi",
                                                     "rmsea", "srmr")),3))
kable(table_fit)


## Do a model specific per condition : wm

wm.fit <- cfa(model = overall.model,
              data = CleanData[CleanData$Condition=="wm", ], 
              meanstructure = TRUE) ##this is important 
summary(wm.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## apparently only 330 of 333 rows were used for the white model

table_fit[3, ] <- c("wm Model", round(fitmeasures(wm.fit, 
                                                  c("chisq", "df", "cfi",
                                                    "rmsea", "srmr")),3))
kable(table_fit)


## Configural Invariance

configural.fit <- cfa(model = overall.model,
                      data = CleanData, 
                      meanstructure = TRUE,
                      group = "Condition") ##this is important 

summary(configural.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, 
                                                          c("chisq", "df", "cfi",
                                                            "rmsea", "srmr")),3))
kable(table_fit)

### MEtric Invariance

metric.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings")) ##this is important 

summary(metric.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Scalar Invariance

scalar.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts")) ##this is important 

summary(scalar.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Strict Invariance

strict.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts", "residuals")) ##this is important 


summary(strict.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)


table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)


## Understanding Lavaan Degrees of freedom 
## https://stackoverflow.com/questions/52937569/understanding-degrees-of-freedom-in-lavaan

#### Hierarchal CFA ####
## https://www.youtube.com/watch?v=C8U9hanos1U


setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

names(CleanData)
dim(CleanData)
## Delete fully empty rows
CleanData<-CleanData[!apply(is.na(CleanData[,6:37]), 1, all),]
dim(CleanData)
## goes from 678 to 669

names(CleanData)
# overall.model = ' 
# SimRac =~ SimRac1 + SimRac2 + SimRac3 + SimRac4 + SimRac5 + SimRac6 + SimRac7 + SimRac8;
# SSIT =~ SepSpher1 + SepSpher2 + SepSpher3 + SepSpher4 + SepSpher5 + SepSpher6 + SepSpher7 + SepSpher8 + SepSpher9 + SepSpher10 + SepSpher11 + SepSpher12 + SepSpher13 + SepSpher14 + SepSpher15;
# TransPH =~ TransPH1 + TransPH2 + TransPH3 + TransPH4 + TransPH5 + TransPH6 + TransPH7 + TransPH8 + TransPH9
# global=~ SimRac + SSIT + TransPH
# 
# '

overall.model = ' 
SimRac =~ SimRac1 + SimRac2 + SimRac3 + SimRac4 + SimRac5 + SimRac6 + SimRac7 + SimRac8;
SSIT =~ SepSpher1 + SepSpher2 + SepSpher3 + SepSpher4 + SepSpher5 + SepSpher6 + SepSpher7 + SepSpher8 + SepSpher9 + SepSpher10 + SepSpher11 + SepSpher12 + SepSpher13 + SepSpher14 + SepSpher15;
TransPH =~ TransPH1 + TransPH2 + TransPH3 + TransPH4 + TransPH5 + TransPH6 + TransPH7 + TransPH8 + TransPH9
global=~ NA*SimRac + SSIT + TransPH
global~~ 1*global
'


library(lavaan)
overall.fit <- cfa(model = overall.model,
                   data = CleanData, 
                   meanstructure = TRUE) #TRUE) ##this is important 
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

#### Create Table for The hierarchal Model Fit Statistics
library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
kable(table_fit)

## Do a model specific per condition : BTW

BTW.fit <- cfa(model = overall.model,
               data = CleanData[CleanData$Condition=="BTW", ], 
               meanstructure = TRUE) ##this is important 
summary(BTW.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## Make table with the results of BTW

table_fit[2, ] <- c("BTW Model", round(fitmeasures(BTW.fit, 
                                                   c("chisq", "df", "cfi",
                                                     "rmsea", "srmr")),3))
kable(table_fit)


## Do a model specific per condition : wm

wm.fit <- cfa(model = overall.model,
              data = CleanData[CleanData$Condition=="wm", ], 
              meanstructure = TRUE) ##this is important 
summary(wm.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
## apparently only 330 of 333 rows were used for the white model

table_fit[3, ] <- c("wm Model", round(fitmeasures(wm.fit, 
                                                  c("chisq", "df", "cfi",
                                                    "rmsea", "srmr")),3))
kable(table_fit)


## Configural Invariance

configural.fit <- cfa(model = overall.model,
                      data = CleanData, 
                      meanstructure = TRUE,
                      group = "Condition") ##this is important 

summary(configural.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, 
                                                          c("chisq", "df", "cfi",
                                                            "rmsea", "srmr")),3))
kable(table_fit)

### MEtric Invariance

metric.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings")) ##this is important 

summary(metric.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Scalar Invariance

scalar.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts")) ##this is important 
## For Scalar fit we get a warning 

# Warning message:
#   In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#                       lavaan WARNING:
#                       The variance-covariance matrix of the estimated parameters (vcov)
#                     does not appear to be positive definite! The smallest eigenvalue
#                     (= 2.218850e-15) is close to zero. This may be a symptom that the
#                     model is not identified.
summary(scalar.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

## Strict Invariance

strict.fit <- cfa(model = overall.model,
                  data = CleanData, 
                  meanstructure = TRUE,
                  group = "Condition",
                  group.equal=c("loadings", "intercepts", "residuals")) ##this is important 
## Another warning 
# Warning message:
#   In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#                       lavaan WARNING:
#                       The variance-covariance matrix of the estimated parameters (vcov)
#                     does not appear to be positive definite! The smallest eigenvalue
#                     (= 1.374505e-15) is close to zero. This may be a symptom that the
#                     model is not identified.


summary(strict.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)


table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, 
                                                      c("chisq", "df", "cfi",
                                                        "rmsea", "srmr")),3))
kable(table_fit)

###

library(semTools)
moreFitIndices(strict.fit)
semPaths(strict.fit, "std")
library(semPlot)

semPaths(strict.fit, 
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree")


summary(strict.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
