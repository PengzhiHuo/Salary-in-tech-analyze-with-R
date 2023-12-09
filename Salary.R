data[, 'Education.Level'] = as.character(data[, 'Education.Level'])
data[, 'Senior'] = as.character(data[, 'Senior'])

# set country into continent
levels(factor(data$Country))
continent <- c("China"="Asia","UK"="Europe","Australia"="Australia","USA"="North_America","Canada"="North_America")
for (i in 1:length(data$Country)){
  country = data[i, "Country"]
  data[i, "Continent"] = continent[country]
}
# set race to race_precise
levels(factor(data$Race))
for (i in 1:length(data$Race)){
  race = data[i, "Race"]
  if (race %in% c("Chinese", "Korean", "Asian")){
    data[i, "Race_precise"] = "Asian"
  }
  else if (race %in% c("African American", "Black")){
    data[i, "Race_precise"] = "African American"
  }
  else if (race %in% c("Welsh", "White")){
    data[i, "Race_precise"] = "White"
  }
  else {
    data[i, "Race_precise"] = race
  }
}

library('regclass')

# find correlations
quantitative <- c('Age','Years.of.Experience')

round(cor(data[,quantitative], y = data[,'Salary']),2)
for (i in quantitative){
  associate(data$Salary~data[,i], data = data, seed = 9750)
}
#Age                 0.73; p<0.05 & conclusive, there is an association
#Years.of.Experience 0.81; p<0.05 & conclusive, there is an association

categorical <- c('Gender','Country','Race_precise','Education.Level','Senior','Continent')
for (i in categorical){
  associate(data$Salary~data[,i], data = data, seed = 9750)
}
# all graph shows that there are some obvious extreme outliers in QQ plot except Education.Level and Race, 
# but the distributions are roughly symmetric, so we use Median test for Education.Level & Race and Rank test for the rest.

# Gender 
# p<0.05 and CI is between 0 and 0.07, there is an association and result is conclusive.

# Country
# p>0.05 and CI is between 0.374 and 0.463, so there is no association and result is conclusive.

# Race_precise
# p>0.05 and CI is between 0.463 and 0.553, there is no association and result is conclusive.

# Education.Level
# p<0.05 and CI is between 0 and 0.07, there is an association and result is conclusive.

# Senior
# p<0.05 and CI is between 0 and 0.07, there is an association and result is conclusive.

# Continent
# p>0.05 and CI is between 0.741 and 0.816, so there is no association and result is conclusive.

# categorical to dummy variable
for (i in categorical){
  temp = factor(data[,i])
  levels(temp) = 0:(length(levels(temp))-1)
  data[,paste(i,'numeric',sep="_")] = temp
}

# muticollinearity test
data_regression = data[, c('Salary','Age','Years.of.Experience','Gender_numeric','Education.Level_numeric','Senior_numeric')]
for (i in colnames(data_regression)){
  data_regression[,i] = as.numeric(data_regression[,i])
}
round(cor(data_regression), 2)
# correlation of Age and Years.of.Experience is 0.94, which shows they are highly correlated.
# So we keep Years.of.Experience and drop Age in the regression model.

m <- lm(Salary~Years.of.Experience+Gender+Education.Level+Senior, data = data)
summary(m)

# Coefficient of Senior is negative, but it does not make sense.  
mean(subset(data, Senior == 1)$Salary)>mean(subset(data, Senior == 0)$Salary)
# Since average salary of senior is greater than average salary of non-senior

m1 <- lm(Salary~Years.of.Experience+Senior, data = data)
summary(m1)
# Then I find out as long as Years.of.Experience+Senior in regression, Coefficient of Senior is always negative

senior1 <- subset(data, Senior == 1)
senior0 <- subset(data, Senior == 0)

library('ggplot2')

ggplot(senior0, aes(y = Salary, x = Years.of.Experience)) +
  geom_smooth(method = 'loess', color = 'blue', fill = 'blue', se = FALSE) +
  geom_smooth(data = senior1, aes(y = Salary, x = Years.of.Experience, colour="#000099"),
              method = 'loess', color = 'red', fill = 'red', se = FALSE)

mean(subset(senior1, Years.of.Experience>=10)$Salary)>mean(subset(senior0, Years.of.Experience>=10)$Salary)
mean(subset(senior1, Years.of.Experience<10)$Salary)>mean(subset(senior0, Years.of.Experience<10)$Salary)
# with higher Years.of.Experience, senior makes less money than non-senior. 
# however, with lower Years.of.Experience, senior makes more money than non-senior. 
# and Years.of.Experience explains more positive variation of salary than senior.
# This is why when they are in regression, the coefficient of Years.of.Experience is positive, but the coefficient of Senior is negative.

# graphs can show it more clearly, when Years.of.Experience > 5 and < 32, smooth line of senior is lower than the line of non-senior.

summary(m)$adj.r.squared
# adjusted R^2 for m is 0.7102 in a good range

# we try intersect variable for Years.of.Experience and Senior
m1 <- lm(Salary~Years.of.Experience*Senior+Gender+Education.Level, data = data)
summary(m1)
# coefficients make more sense now

summary(m1)$adj.r.squared
# adjusted R^2 for m1 is 0.7118, better

pairs(~m1$res+m1$fitted.values+Years.of.Experience+Gender_numeric+Education.Level_numeric+Senior_numeric, data = data_regression)
# residual v.s. fitted value looks randomly scattered. But constant error variance seems violated.

# so transform y to log10(y)
m2 <- lm(log10(Salary)~Years.of.Experience*Senior+Gender+Education.Level, data = data)
summary(m2)
pairs(~m2$res+m2$fitted.values+Years.of.Experience+Gender_numeric+Education.Level_numeric+Senior_numeric, data = data_regression)
# looks better , but there exists outliers.

summary(m2)$adj.r.squared
# adjusted R^2 for m is 0.6944 in a good range

# comparing p-value, Years.of.Experience, Gender, Education.Level have stronger effect, and senior has less effect
# further comparing t-value,
# Years.of.Experience have the greatest effect on salary, then Education.Level, then gender.

# limitation
# normality test
library(nortest)
lillie.test(m2$residuals)
hist(m2$residuals)
# p<0.05, reject H0. residuals do not follow normal distribution.
library(ALSM)
normal.cor.test(residuals(m2),sigma(m2)^2)
# but correlation coefficient between the ordered residuals and their expected values under normality is high (0.94), so we can ignore it.
