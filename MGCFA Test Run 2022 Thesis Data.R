## MGCFA 2022 Test Run
## https://www.youtube.com/watch?v=Kt2lZxcAGWs&t=2s

setwd("C:/Users/cogps/Desktop/Thesis Analysis")
CleanData <- read.csv("Clean_Data_2022_Latest.csv") 

library(lavaan)

head(CleanData)

table(CleanData$Condition)

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
