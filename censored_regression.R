# dplyr is used for the pipe (%>%) and mutate() functions
library(dplyr)
# censReg is used to perform the censored regression
library(censReg)

# Read in data; concatenate media and temperature into a factor
# log10-transform counts; substitute "0" in logCount where Count was 0 - these
#   values are censored in the regression anyway, but censReg doesn't like the
#   -Inf values for some reason
read.csv("count_data") %>%
  mutate(Temp=factor(Temp),
         logCount=log10(Count),
         med_temp=factor(paste(Media,Temp,sep="")),
         day_factor=factor(Day,levels=c("21","14","0"))) %>%
  mutate(logCount=ifelse(Count==0,0,logCount)) -> data

# Reference level for med_temp is the first level alphabetically: BHI10
# Estimates "med_tempBHI6", "med_tempSMB10", and "med_tempSMB6" are therefore
# the contrasts "BHI 6°C - BHI 10°C", "SMB 10°C - BHI 10°C", and "SMB 6°C -
# BHI 10°C", respectively
summary(censReg(logCount ~ med_temp + day_factor, left=1, data=data))

# Re-level med_temp factor so that SMB6 is the reference level.
data %>%
  mutate(med_temp=relevel(med_temp,ref="SMB6")) -> data

# Reference level for med_temp is now SMB6
# Estimates "med_tempSMB10", "med_tempBHI6", and "med_tempBHI10" are the
# contrasts "SMB 10°C - SMB 6°C", "BHI 6°C - SMB 6°C", and "BHI 10°C - SMB 6°C"
# respectively
summary(censReg(logCount ~ med_temp + day_factor, left=1, data=data))

# Re-level med_temp factor so that BHI6 is the reference level.
data %>%
  mutate(med_temp=relevel(med_temp,ref="BHI6")) -> data

# Reference level for med_temp is now BHI6
# Estimates "med_tempSMB6", "med_tempSMB10", and "med_tempBHI10" are the
# contrasts "SMB 6°C - BHI 6°C", "SMB 10°C - BHI 6°C", and "BHI 10°C - BHI 6°C"
# respectively
summary(censReg(logCount ~ med_temp + day_factor, left=1, data=data))
