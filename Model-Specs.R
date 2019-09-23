# Model specifications
v <- list()
v$itemspre <- c("ctar_q1_1",
                "ctar_q2_1",
                "ctar_q3_1",
                "ctar_q4_1",
                "ctar_q5_1",
                "ctar_q6_1",
                "ctar_q7_1",
                "ctar_q8_1",
                "ctar_q9_1",
                "ctar_q10_1",
                "ctar_q11_1",
                "ctar_q12_1",
                "ctar_q13_1",
                "ctar_q14_1",
                "ctar_q15_1",
                "ctar_q16_1",
                "ctar_q17_1")

# Specify fit indices of interest
v$fitindices <- c("npar", "chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")
v$fitindices_robust <- c("npar", "chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")

# Model designations using Lavaan format
m1 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1' ### m1 is original model with no correlated residuals
m2 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1
      
      # Correlated residuals
      ctar_q14_1 ~~ ctar_q16_1'

m3 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1
      ctar_q14_1 ~~ ctar_q16_1
      ctar_q13_1 ~~ ctar_q14_1' 

m4 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1
      
      # Correlated residuals
      ctar_q14_1 ~~ ctar_q16_1
      ctar_q13_1 ~~ ctar_q14_1
      ctar_q13_1 ~~ ctar_q16_1' 

m5 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1
      
      # Correlated residuals
      ctar_q14_1 ~~ ctar_q16_1
      ctar_q13_1 ~~ ctar_q14_1
      ctar_q13_1 ~~ ctar_q16_1

      # Constrained thresholds for single item (all)
      ctar_q17_1 | c(a11,a11)*t1; ctar_q17_1 | c(b11,b11)*t2; ctar_q17_1 | c(c11,c11)*t3'

m6 <- 'F1 =~ ctar_q1_1 + ctar_q2_1 + ctar_q3_1 + ctar_q4_1 + ctar_q5_1 + ctar_q6_1 + ctar_q7_1 + ctar_q8_1 + ctar_q9_1 + ctar_q10_1 + ctar_q11_1 + ctar_q12_1 + ctar_q13_1 + ctar_q14_1 + ctar_q15_1 + ctar_q16_1 + ctar_q17_1\n      ctar_q14_1 ~~ ctar_q16_1\n      ctar_q13_1 ~~ ctar_q14_1\n      ctar_q13_1 ~~ ctar_q16_1; 
      
      # Correlated residuals
      ctar_q14_1 ~~ ctar_q16_1
      ctar_q13_1 ~~ ctar_q14_1
      ctar_q13_1 ~~ ctar_q16_1

      # Constrained thresholds for single item (one)
      ctar_q1_1 | c(a11,a11)*t1; ctar_q1_1 | c(b11,b12)*t2; ctar_q1_1 | c(c11,c12)*t3'

m7 <- 'F1 =~ ctar_q2_1 + ctar_q6_1 + ctar_q8_1 + ctar_q9_1 + ctar_q15_1 + ctar_q16_1'

m8 <- 'F1 =~ ctar_q2_1 + ctar_q6_1 + ctar_q8_1 + ctar_q9_1 + ctar_q15_1 + ctar_q16_1

      # Setting the intercepts of the manifest variables to zero
      ctar_q2_1 ~ 0*1'


# Set estimator
em <- "WLSMV"

# Grouping variable
group.var <- as.character("gender")
# Group 1: Female
# Group 2: Male