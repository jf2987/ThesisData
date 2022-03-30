## MGCFA 2022 Test Run
## https://www.youtube.com/watch?v=Kt2lZxcAGWs&t=2s

setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

## Delete rows that which are completely empty for the variables of interest
## https://www.r-bloggers.com/2021/06/remove-rows-that-contain-all-na-or-certain-columns-in-r/

library(tidyr)
CleanData<-CleanData %>% drop_na(6:20)

names(CleanData)

## 

library(lavaan)

head(CleanData)

table(CleanData$Condition)
## BTW 333, wm 331

## the first lavaan

overall.model = ' SSIT =~ SepSpher1 + SepSpher2 + SepSpher3 + SepSpher4 + SepSpher5 + SepSpher6 + SepSpher7 + SepSpher8 + SepSpher9 + SepSpher10 + SepSpher11 + SepSpher12 + SepSpher13 + SepSpher14 + SepSpher15'

overall.fit <- cfa(model = overall.model,
                   data = CleanData, 
                   meanstructure = TRUE) ##this is important 
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## Make table with the results

library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3))
kable(table_fit)


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
kable(table_fit)


## Do a model specific per condition : wm

wm.fit <- cfa(model = overall.model,
               data = CleanData[CleanData$Condition=="wm", ], 
               meanstructure = TRUE) ##this is important 
summary(wm.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

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

## Metric Invariance - setting the loadings to equal

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
 

## Scalar Invariance -- setting the intercepts to equal

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

### Strict (Error) Invariance-- constrict the errors to be the same 

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


## latent means

predicted_scores <- lavPredict(strict.fit, type="ov")

table(CleanData$Condition)

View(predicted_scores)

predicted_scores <- as.data.frame(do.call(rbind, predicted_scores))

View(predicted_scores)

# the code breaks here for me -- its cuz I think there are rows
## that literally have nothing entered -- so complete rows missing for 
## the variables of interest 
predicted_scores$Condition <- c(rep("wm", 331), rep("BTW", 333))
View(predicted_scores)
 

predicted_scores$sum <- apply(predicted_scores[ , 1:15], 1, sum)
View(predicted_scores)

## Predicted Model Scores
tapply(predicted_scores$sum, predicted_scores$Condition, mean)                               
  
## Actual Data Average Score
names(CleanData)
CleanData$sum <- apply(CleanData[ , 6:20], 1, sum)

tapply(CleanData$sum, CleanData$Condition, mean)

## latent means
latent_means<-lavPredict(strict.fit)
latent_means <- as.data.frame(do.call(rbind, latent_means))
table(CleanData$Condition)

latent_means$Condition <- c(rep("wm", 331), rep("BTW", 333))

options(scipen=999)

View(latent_means)
# z scores
tapply(latent_means$SSIT, latent_means$Condition, mean) 

# real scores
tapply(CleanData$sum, CleanData$Condition, mean, na.rm = T)
tapply(CleanData$sum, CleanData$Condition, sd, na.rm = T)

## Calculate Effect Size

library(MOTE)
M <-tapply(CleanData$sum, CleanData$Condition, mean)
SD <-tapply(CleanData$sum, CleanData$Condition, sd)
N <- tapply(CleanData$sum, CleanData$Condition, length)

effect_size<-d.ind.t(M[1], M[2], SD[1], SD[2], N[1], N[2], a = .05)

effect_size$estimate
#"$d_s$ = -0.02, 95\\% CI [-0.17, 0.13]"

effect_size$statistic
# "$t$(662) = -0.27, $p$ = .788"