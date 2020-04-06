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
# H0: There is no significant association between handedness and favorite season.

#HA: There is a significant association between handedness and favorite season.

# It seems okay to remove rows with missing observations since we're still left with
# about 90% of the data left after removal. The missingness seems MCAR
# as well, unrelated to any particular variable.
# At alpha = .05, we fail to reject the null. There is not sufficient evidence to 
#suggest a significant association between handedness and favorite season.

#c
height_arm <- data.frame(ilschool$Height_cm, ilschool$Armspan_cm)
colnames(height_arm) = c("Height", "Armspan")
age_footspan = data.frame(ilschool$Ageyears, ilschool$Footlength_cm)
colnames(age_footspan) = c("Age", "Footspan")

library(mice)
height_arm = read.csv("C:/Users/lwsc2/Documents/College Files/2020 SPRING/STAT 351/HW 6/HW 6/heightarm.csv", header = T)
age_footspan = read.csv("C:/Users/lwsc2/Documents/College Files/2020 SPRING/STAT 351/HW 6/HW 6/agefootspan.csv", header = T)

height_arm = data.frame(age_footspan[1:2], height_arm[1:2])

tempdata = mice(height_arm,maxit=50,meth='cart',seed=500)
fit = with(tempdata, lm(Height ~ Armspan))
pool1 = summary(pool(fit))
pool1$estimate[1]
pool1$estimate[2]
pool1$std.error[1]
pool1$std.error[2]

tempdata2 = mice(height_arm,maxit=50,meth='rf',seed=500)
fit = with(tempdata2, lm(Height ~ Armspan))
summary(pool(fit))

