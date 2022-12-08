#setwd("D:/Codes_ORI_Sim")
source("./R_Aux_scripts/MICoDa_Aux.R")  # includes various auxiliary functions including 
# the key R function MICoDa2
data_full <- readRDS("./metadata/data_full.rds") # data-set without any missing value
data_NA <- readRDS("./metadata/data_with_NA.rds")
data_full[c(1:3,8),]  #printing few rows
data_NA[c(1:3,8),]  #MICoDa will be applied to this one
sum(is.na(data_NA[,"other_a20"]))/nrow(data_NA) #30% data has a at least one misising category 


varlist <-  c("age", "bmi", "married","abnorm","educ_2","educ_3","educ_4") # covariate list
# MICoDa_V1: When all components of the missing rows are imputed
rt1<- MICoDa(data_NA, 6, varlist,iter=20, rescaleAll=T,Constant=30)
plot(rt1$diff, type='l',ylab= "Relative rMSPE",xlab="iter",main="Slower convergence due to rescaling of the whole row")   # Plotting relative rMSPE 
# between consecutive iterations
rt1$data_Imp[c(1:3,8), ]                                     # Imputed data
rowSums(rt1$data_Imp[c(1:3,8), 1:6])                            # Checking row total on the imputed data
rt1$Theta_est                            # Estimated coefficients from  ALR model at convergence
rt1$NArows       # Rows imputed


# MICoDa_V2: When only missing components of the missing rows are imputed
rt2<- MICoDa(data_NA, 6, varlist,iter=20, rescaleAll=F,Constant=30)
plot(rt2$diff, type='l',ylab= "Relative rMSPE",xlab="iter", main="Faster convergence due to partial rescaling of the rows")   # Plotting relative rMSPE 
# between consecutive iterations
rt2$data_Imp[c(1:3,8), ]                                     # Imputed data
rowSums(rt2$data_Imp[c(1:3,8), 1:6])                            # Checking row total on the imputed data
rt2$Theta_est                            # Estimated coefficients from  ALR model at convergence
rt2$NArows       # Rows imputed


