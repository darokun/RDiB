# Propensity scores

install.packages("MatchIt")
library(MatchIt)

# 1. a) Import dataset
data <- lalonde

# 1. b) Explore dataset
names(data)
dim(data)
str(data)

table(data$treat)
summary(data$age)
summary(data$educ)
table(data$black)
table(data$hispan)
table(data$married)
table(data$nodegree)
summary(data$re74)
hist(data$re74, breaks=50)
summary(data$re75)
hist(data$re75, breaks=50)

summary(data$re78) #main outcome
hist(data$re78, breaks=50)

# 2. Perform lm
linear.m <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree + 
                 re74 + re75, data = data)
summary(linear.m)
# on average, treated units earned 1548 dollars more in 1978 than non-treated units, held everything else stable. The effect is significant (p < 0.05)

# 3. Perform univariate logreg with covariates: 
# age, educ, black, hispan, married, nodegree, re74, re75

log.age <- glm(treat ~ age, data = data, family = binomial)
log.educ <- glm(treat ~ educ, data = data, family = binomial)
log.black <- glm(treat ~ black, data = data, family = binomial)
log.hispan <- glm(treat ~ hispan, data = data, family = binomial)
log.married <- glm(treat ~ married, data = data, family = binomial)
log.nodegree <- glm(treat ~ nodegree, data = data, family = binomial)
log.re74 <- glm(treat ~ re74, data = data, family = binomial)
log.re75 <- glm(treat ~ re75, data = data, family = binomial)
summary(log.age)
summary(log.educ)

cbind(exp(log.age$coef[2]), exp(log.educ$coef[2]), exp(log.black$coef[2]), 
      exp(log.hispan$coef[2]), exp(log.married$coef[2]), exp(log.nodegree$coef[2]), 
      exp(log.re74$coef[2]), exp(log.re75$coef[2]))

summary(log.nodegree)
exp(log.nodegree$coef[2])
# subjects with a high school degree were 1.64 times more likely to be in the treatment group (p-value <0.01)

# 4. Use logreg to get the PS
log.PS <- glm(treat ~ age + educ + nodegree + re74 + re75, data = data, family = binomial)
data$PS.value <- predict(log.PS, type="response")
boxplot(data$PS.value ~ data$treat)
# subjects in the treated group have a higher probability of being in the treated group?
# but results overlap (??)

# 5. weight PS
# for treated subjects: omega = 1/PS.value
# for controls: omega = 1/(1-PS.value)
data$w.ATE <- NULL
for(i in 1:length(data$treat)) {
  if(data$treat[i]==1) {
    data$w.ATE[i] <- 1/data$PS.value[i]
  }
  if(data$treat[i]==0) {
    data$w.ATE[i] <- 1/(1-data$PS.value[i])
  }
}

# 6. repeat question 2 with the weighted PS and compare to previous results
linear.mw <- lm(re78 ~ treat + black + hispan + married,weights = w.ATE, data = data)
summary(linear.mw)
summary(linear.m)

#####################
### END OF SCRIPT ###
#####################