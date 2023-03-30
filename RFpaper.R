#SARS-CoV-2-Liantis-study-Risk-Factors

#libraries
library(tidyverse)
library(ggplot2)
library(psych)
library(factoextra)
library(randomForest)
library(rpart)
library(rpart.plot)
library(mlbench)
library(rstanarm)
library(bayestestR)
library(bayesplot)
library(insight)
library(broom)

load("SARS-CoV-2-Liantis-study-Risk-Factors")
data <- AnnWeH
rm(AnnWeH)

#table 1  
table(data$Organisation_type)
table(data$Urbanisation)
table(data$PCR_test_capacity)
table(data$PBM_disp_gloves)
table(data$PBM_surg_masks)
table(data$PBM_FFP2_masks)
table(data$PBM_disp_aprons)
table(data$EDU_alcohol_gel)
table(data$PBM_face_shields)
table(data$PBM_safety_glasses)
table(data$PBM_alcohol_gel)
table(data$Daily_cleaning)
table(data$EDU_disp_gloves)
table(data$EDU_surg_masks)
table(data$EDU_FFP2_masks)
table(data$EDU_disp_aprons)
table(data$EDU_face_shields)
table(data$EDU_safety_glasses)
table(data$EDU_alcohol_gel)
table(data$Ventilation_Indiv_Rooms)
table(data$Ventilation_Common_Areas)

#table 2
quantile(data$Ratio_Female)
quantile(data$Ratio_Older_84,na.rm = TRUE)
quantile(data$Ratio_Dementia,na.rm = TRUE)
quantile(data$Construction_year)

#table 3 univariable models
y <- cbind(as.numeric(data$pos),as.numeric(data$neg))

## community
### urbanisation
summary(glm(y~Urbanisation,binomial,data=data))

## residents
### ratio females
summary(glm(y~Ratio_Female,binomial,data=data))
### ratio older
summary(glm(y~Ratio_Older_84,binomial,data=data))
### ratio dementia
summary(glm(y~Ratio_Dementia,binomial,data=data))

## management
### type of nh
summary(glm(y~Organisation_type,binomial,data=data))
### pcr test capacity
summary(glm(y~PCR_test_capacity,binomial,data=data))
### ppe1
summary(glm(y~pbmtot1,binomial,data=data))
### ppe2
summary(glm(y~pbmtot2,binomial,data=data))
### daily cleaning
summary(glm(y~Daily_cleaning,binomial,data=data))
### educated
summary(glm(y~Educated,binomial,data=data))

## building
### construction year
summary(glm(y~Construction_year,binomial,data=data))
### ventilation
summary(glm(y~Ventilation,binomial,data=data))

#table 3 multivariable bayes model
datac <- data %>%
  filter(complete.cases(.))
model_bayes<- stan_glm(cbind(datac$pos,datac$neg)~pbmtot1+PCR_test_capacity+Ratio_Dementia+Educated+Ratio_Older_84+Ratio_Female+Construction_year+pbmtot2+Ventilation+Urbanisation,binomial,data=datac, seed=111)
model_bayes<- stan_glm(cbind(datac$pos,datac$neg)~pbmtot1+PCR_test_capacity+Ratio_Dementia+Educated+Ratio_Older_84+Ratio_Female+Construction_year+pbmtot2+Ventilation+Urbanisation+Organisation_type+Daily_cleaning,binomial,data=datac, seed=111)
describe_posterior(model_bayes)

#Figure 1
ggplot(data,aes(prev)) + geom_histogram(color="black",fill="#CCCCCC") +
  labs(x = "seroprevalence in %", y = "Number of nursing homes") +
  theme_classic()

#Figure 2

#random forest plot function
plot_rf <-
  function(rf) {
    imp <- importance(rf)[, "IncNodePurity"]
    imp_rel <- 100 * sort(imp / max(imp))
    par(mar = c(6, 10, 4, 1))
    plot(imp_rel, seq(imp_rel),
         ylab = "", xlab = "relative importance",
         axes = FALSE)
    axis(2, at = seq(imp_rel), labels = (names(imp_rel)), las = 1)
    axis(1)
    box()
    abline(h = seq(imp_rel), lty = 3, col = "grey")
    points(imp_rel, seq(imp_rel), pch = 16)
  }

plot_rf2 <-
  function(rf) {
    imp <- importance(rf)[, "%IncMSE"]
    imp_rel <- 100 * sort(abs(imp) / max(abs(imp)))
    par(mar = c(6, 10, 4, 1))
    plot(imp_rel, seq(imp_rel),
         ylab = "", xlab = "relative importance",
         axes = FALSE)
    axis(2, at = seq(imp_rel), labels = (names(imp_rel)), las = 1)
    axis(1)
    box()
    abline(h = seq(imp_rel), lty = 3, col = "grey")
    points(imp_rel, seq(imp_rel), pch = 16)
  }

subset1 <- datac%>%
  mutate(PPE1 = pbmtot1) %>%
  mutate(PPE2 = pbmtot2) %>%
  mutate(Education = Educated) %>%
  mutate(Ventilation = Ventilation) %>%
  mutate(Ratio_dementia = Ratio_Dementia) %>%
  mutate(Ratio_females = Ratio_Female) %>%
  mutate(Ratio_85_and_older = Ratio_Older_84) %>%
  mutate(PCR_capacity = PCR_test_capacity) %>%
  mutate(Construction_year = Construction_year) %>%
  mutate(Urbanisation = Urbanisation) %>%
  mutate(Organisation_type = Organisation_type) %>%
  mutate(Cleaning = Daily_cleaning) %>%
  select(prev, PPE1, PPE2, Education, Ventilation, Ratio_dementia, Ratio_females, Ratio_85_and_older, PCR_capacity, Construction_year, Urbanisation, Organisation_type, Cleaning)

set.seed(198745)
vars1<-names(subset1)[-1]
f<-as.formula(paste("prev~", paste(vars1, collapse="+")))
rf <- randomForest(f, data = subset1, importance=TRUE)
plot_rf(rf)
plot_rf2(rf)
