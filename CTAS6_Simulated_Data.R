# Author: Michael Mack
# Corresponding Author Contact: mack13@uw.edu
# Date Created: Nov. 30, 2018; Last Updated: September 10, 2019
# Description: This code: 1) Uses the lavaan package to conduct confirmatory factor analysis on CTAR response data, and 2) uses the userfriendlyscience package to compute scale reliability estimates.

# 2. Load Data and source files
# Dataset contains 3,333 observations for 21 fields. See metadata.csv for field descriptions. 
# mydata <- read.csv("fullset.csv", na.strings = c("", "NA"))
k1 <- read.csv("k1.csv", na.strings = c("", "NA")) # N = 1,667
k2 <- read.csv("k2.csv", na.strings = c("", "NA")) # N = 1,666
source("Functions.R")
source("Model-Specs.R")


############################## CFA ##############################

# Use dataset k1
fit11 <- lavaan::cfa(m1, data = k1, estimator = em, meanstructure = TRUE, ordered = names(k1[,v$itemspre])); summary(fit11, fit.measures = TRUE, rsq = TRUE, standardized = TRUE)
w1 <- scaleStructure(k1[,v$itemspre]); w1


########################### CFA by Group ########################

# Use dataset k1
fit11a <- lavaan::cfa(m1, data = k1[which(k1$gender=="Male"),], estimator = em, meanstructure = TRUE, ordered = names(k1[,v$itemspre])); summary(fit11a, fit.measures = TRUE, rsq = TRUE, standardized = TRUE)
fit11b <- lavaan::cfa(m1, data = k1[which(k1$gender=="Female"),], estimator = em, meanstructure = TRUE, ordered = names(k1[,v$itemspre])); summary(fit11b, fit.measures = TRUE, rsq = TRUE, standardized = TRUE)
w1a <- scaleStructure(k1[which(k1$gender=="Male"),v$itemspre]); w1a
w1b <- scaleStructure(k1[which(k1$gender=="Female"),v$itemspre]); w1b


######################### Multi-Group CFA #######################

# Use dataset k1
config <- lavaan::cfa(m4, data = k1, estimator = em, group=group.var, parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)
metric <- lavaan::cfa(m4, k1, estimator = em, group=group.var, group.equal = c("loadings"), parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)
scalar <- lavaan::cfa(m4, k1, estimator = em, group=group.var, group.equal = c("loadings", "thresholds"), parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)
strict <- lavaan::cfa(m4, k1, estimator = em, group=group.var,group.equal = c("loadings", "thresholds", "residuals"), parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)

## To constrain all thresholds for a single item:
partial1 <- lavaan::cfa(m5, k1, estimator = em, group=group.var, group.equal = c("loadings"), parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)

## To constrain a single threshold for a single item: 
partial2 <- lavaan::cfa(m6, k1, estimator = em, group=group.var, group.equal = c("loadings"), parameterization = "theta", ordered = names(k1[,v$itemspre]), meanstructure = TRUE)


##################### Latent Means Modeling #####################
   
# Use dataset k2     
fit <- sem(m7, k2, estimator = em, group=group.var, group.equal = c("loadings", "thresholds"), parameterization = "theta", ordered = names(k2[,c("ctar_q2_1", "ctar_q6_1", "ctar_q8_1", "ctar_q9_1", "ctar_q15_1", "ctar_q16_1")]), meanstructure = TRUE); summary(fit, fit.measures = TRUE, rsq = TRUE)





