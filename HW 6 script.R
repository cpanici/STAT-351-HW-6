ilschool = read.csv("C:/Users/lwsc2/Downloads/ill_school_data.csv", header = T)

#1
#a
boxplot(ilschool[6], main = "Age in Years")
summary(ilschool[12])

#b
library(GmAMisc)
library(VIM)
# Removing missing obs
ilschool[ilschool == ""] = NA
hand_season <- data.frame(ilschool$Handed, ilschool$Favorite_Season)
colnames(hand_season) = c("Handed", "Season")
aggr(hand_season)

missing_removed = hand_season[complete.cases(hand_season),]

tab = table(missing_removed$Handed, missing_removed$Season)
# Still getting extra rows for some reason
tab = tab[-1,-1]
tab
chiperm(tab,B = 1000)
#1. H0: There is no significant association between handedness and favorite season.

#HA: There is a significant association between handedness and favorite season.

# It seems okay to remove rows with missing observations since we're still left with
# about 90% of the data left after removal. The missingness seems MCAR
# as well, unrelated to any particular variable.
# At alpha = .05, we fail to reject the null. There is not sufficient evidence to 
#suggest a significant association between handedness and favorite season.

#c
height_arm <- data.frame(ilschool$Height_cm, ilschool$Armspan_cm)
colnames(height_arm) = c("Height", "Armspan")
height_arm$Height = gsub("[^0-9.-]", "", height_arm)

library(mice)
height_arm = read.csv("C:/Users/lwsc2/Documents/College Files/2020 SPRING/STAT 351/HW 6/HW 6/heightarm.csv", header = T)
tempdata = mice(height_arm,maxit=50,meth='cart',seed=500)
fit = with(tempdata, lm(Height ~ Armspan))
summary(pool(fit))
# Intercept: 48.5136927  Std Err: 7.21623028
# Slope: 0.7159695  Std Err: 0.04276844

tempdata2 = mice(height_arm,maxit=50,meth='rf',seed=500)
fit = with(tempdata2, lm(Height ~ Armspan))
summary(pool(fit))
# Intercept: 47.4342745  Std Err: 6.77751962
# Slope: 0.7222957  Std Err: 0.04040624




