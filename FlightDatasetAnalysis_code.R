library(readxl)
library(tidyverse)
library(psych)
library(dplyr)
library(funModeling)

FAA1 <- read_excel("K:/Komal/UCin MSBA/6043/FAA1.xls")
head(FAA1)
names(FAA1)
dim(FAA1)

psych::describe(FAA1)
colSums(is.na(FAA1))

#Checking invalid and outlier observations
##1. Duration Validation
FAA1 <- FAA1 %>% mutate(Dur_quality = case_when(is.na(duration) ~ "Null",
                                                   duration<40 ~ "IV",TRUE ~ "V"))
##2. Ground Speed Validation
FAA1 <- FAA1 %>% mutate(SpGr_quality = case_when(is.na(speed_ground) ~ "Null",
                                                       speed_ground<30 | speed_ground>140 ~ "IV",TRUE ~ "V"))
##3. Air Speed Validation
FAA1 <- FAA1 %>% mutate(SpAir_quality = case_when(is.na(speed_air) ~ "Null",
                                                    speed_ground<30 | speed_ground>140 ~ "IV",TRUE ~ "V"))
##4. Height Validation
FAA1 <- FAA1 %>% mutate(Height_quality = case_when(is.na(height) ~ "Null",
                                                 height<6 ~ "IV",TRUE ~ "V"))
##5. Distance Validation
FAA1 <- FAA1 %>% mutate(Dis_quality = case_when(is.na(distance) ~ "Null",
                                                   distance>6000 ~ "IV",TRUE ~ "V"))
###Count of abnormal records
FAA1 %>% count(Dur_quality)
FAA1 %>% count(SpGr_quality)
FAA1 %>% count(SpAir_quality)
FAA1 %>% count(Height_quality)
FAA1 %>% count(Dis_quality)

#Replacing the missing values with  0
library(tidyr)
FAA1<-FAA1 %>% mutate(speed_air = replace_na(speed_air, 0))

#Deleting the abnormalities
FAA<- FAA1[!(FAA1$Dur_quality=="IV" | FAA1$SpGr_quality=="IV" | FAA1$SpAir_quality=="IV"
           | FAA1$Height_quality=="IV" | FAA1$Dis_quality=="IV"),]
dim(FAA)

#Summary statistics
#Dropping the quality columns
library(dplyr)
FAA <- select(FAA, -c(9:13))
dim(FAA)
summary(FAA)

#For plotting Histograms of all the variables
plot_num(FAA)

#Statistical analysis of the XY plots between different variables with distance
par(mfrow = c(2, 3))
plot(FAA$distance ~ FAA$no_pasg)
plot(FAA$distance ~ FAA$speed_ground)
plot(FAA$distance ~ FAA$speed_air)
plot(FAA$distance ~ FAA$height)
plot(FAA$distance ~ FAA$pitch)
plot(FAA$distance ~ FAA$duration)

#Computing correlation
NFAA <- FAA[, sapply(FAA, is.numeric)] #Type casting
cor(NFAA)


#Linear modelling
#1. speed_ground
fit<- lm(FAA$distance ~ FAA$speed_ground)
fit
summary(fit)


#2. speed_ground, height and aircraft
fit1 <- lm(FAA$distance ~ FAA$speed_ground + FAA$height)
fit1
summary(fit1)


residuals<-fit1$res
par(mfrow=c(1,2))
plot(FAA$speed_ground,residuals)
abline(h=c(-2,0,2),lty=2)
qqnorm(residuals)
abline(0,1)