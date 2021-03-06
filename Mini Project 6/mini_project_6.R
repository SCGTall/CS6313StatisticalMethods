# Question 1
# Note that vesinv is a qualitative variable.
# You can treat gleason as a quantitative variable.
# 1) Load and prepare data
prostate_cancer <- read.csv(
  file=file.path("./Mini Project 6/prostate_cancer.csv"))
str(prostate_cancer)
index = prostate_cancer$subject
pas = prostate_cancer$psa
logpsa = log(prostate_cancer$psa)
cancervol = prostate_cancer$cancervol
weight = prostate_cancer$weight
age = prostate_cancer$age
benpros = prostate_cancer$benpros
vesinv = prostate_cancer$vesinv
capspen = prostate_cancer$capspen
gleason = prostate_cancer$gleason
table(vesinv)
# vesinv is a qualitative variable with 2 values
# Automatically represent with 1 dummy variable: factor(vesinv)
vesinv.factor1 = ifelse(vesinv == 1, 1, 0)

# 2) Analyze the data with simple linear regression first
# cancervol vs psa
plot(cancervol, psa, type="p", main="cancervol vs psa")
abline(lm(psa ~ cancervol))
plot(cancervol, logpsa, type="p", ylab="log(psa)", main="cancervol vs log(psa)")
abline(lm(logpsa ~ cancervol))
paste("cancervol vs psa: ", cor(psa, cancervol),
      "; log(psa): ", cor(logpsa, cancervol))

# weight vs psa
plot(weight, psa, type="p", main="weight vs psa")
abline(lm(psa ~ weight))
plot(weight, logpsa, type="p", ylab="log(psa)", main="weight vs log(psa)")
abline(lm(logpsa ~ weight))
paste("weight vs psa: ", cor(psa, weight),
      "; log(psa): ", cor(logpsa, weight))

# age vs psa
plot(age, psa, type="p", main="age vs psa")
abline(lm(psa ~ age))
plot(age, logpsa, type="p", ylab="log(psa)", main="age vs log(psa)")
abline(lm(logpsa ~ age))
paste("age vs psa: ", cor(psa, age),
      "; log(psa): ", cor(logpsa, age))

# benpros vs psa
plot(benpros, psa, type="p", main="benpros vs psa")
abline(lm(psa ~ benpros))
plot(benpros, logpsa, type="p", ylab="log(psa)", main="benpros vs log(psa)")
abline(lm(logpsa ~ benpros))
paste("benpros vs psa: ", cor(psa, benpros),
      "; log(psa): ", cor(logpsa, benpros))

# factor(vesinv)1 vs psa
plot(vesinv.factor1, psa, type="p", main="factor(vesinv)1 vs psa")
abline(lm(psa ~ vesinv.factor1))
plot(vesinv.factor1, logpsa, type="p", ylab="log(psa)",
     main="factor(vesinv)1 vs log(psa)")
abline(lm(logpsa ~ vesinv.factor1))
paste("factor(vesinv)1 vs psa: ", cor(psa, vesinv.factor1),
      "; log(psa): ", cor(logpsa, vesinv.factor1))

# capspen vs psa
plot(capspen, psa, type="p", main="capspen vs psa")
abline(lm(psa ~ capspen))
plot(capspen, logpsa, type="p", ylab="log(psa)", main="capspen vs log(psa)")
abline(lm(logpsa ~ capspen))
paste("capspen vs psa: ", cor(psa, capspen),
      "; log(psa): ", cor(logpsa, capspen))

# gleason vs psa
plot(gleason, psa, type="p", main="gleason vs psa")
abline(lm(psa ~ gleason))
plot(gleason, logpsa, type="p", ylab="log(psa)", main="gleason vs log(psa)")
abline(lm(logpsa ~ gleason))
paste("gleason vs psa: ", cor(psa, gleason),
      "; log(psa): ", cor(logpsa, gleason))

# Nearly all regressions with log(pas) are better than that with psa, except
# for capspen. And regressions with log(pas) show better linear trend in our
# observation.
# Even for capspen, the diagram shows a good linear trend.
# Hence, we would use log(pas) afterwards.

# 3) Multiple linear regression
# Start with full model
fit1 = lm(logpsa ~ cancervol + weight + age +
            benpros + factor(vesinv) + capspen + gleason)
summary(fit1)
# Drop age
fit2 = update(fit1, . ~ . - age)
summary(fit2)
# Drop weight
fit3 = update(fit2, . ~ . - weight)
summary(fit3)
# Drop capspen (which prefer pas better than log(psa))
fit4 = update(fit3, . ~ . - capspen)
summary(fit4)
# Reject all H0 here
anova(fit1, fit2, fit3, fit4)
# We stop here. For it seems that we dropped 3 parameters which are clear not
# important and the other 4 parameters show good linear trend.

# 4) Verify by model selection with BIC
nullmd = lm(logpsa ~ 1)
fullmd = lm(logpsa ~ cancervol + weight + age +
              benpros + factor(vesinv) + capspen + gleason)
forward = step(nullmd, scope=list(lower=nullmd, upper=fullmd),
               direction="forward", k=log(length(logpsa)))
# When scope is missing, default for direction is "backward"
backward = step(fullmd, k=log(length(logpsa)))
# When scope is announced, default for direction is "both"
both = step(nullmd, scope=list(lower=nullmd, upper=fullmd),
            k=log(length(logpsa)))
# All stepwise selections choose the save model as fit4 which is chosen
# manually by us.

# 5) Verify the model assumptions
# Residual plot
plot(fitted(fit4), resid(fit4), main="Residual plot")
abline(h=0)
# QQ Plot
qqnorm(resid(fit4), main="QQ plot")
qqline(resid(fit4))
# Time series plot
maxabs = max(abs(resid(fit4)))
plot(index, resid(fit4), type='l', main="Time series plot",
     ylim=maxabs*c(-1, 1))
abline(h=0)

# 6) Predict the PSA level for a "common average" patient
summary(fit4)
# Get mode by names(sort(-table(vesinv)))[1]. Return a string here.
psa.predict = exp(-0.65013 + 0.06488*mean(cancervol) + 0.09136*mean(benpros) +
                  0.68421*(ifelse(names(sort(-table(vesinv)))[1]=='1', 1, 0)) +
                  0.33376*mean(gleason))
print(mean(cancervol))
print(mean(benpros))
print(ifelse(names(sort(-table(vesinv)))[1]=='1', 1, 0))  # mode
print(mean(gleason))
print(psa.predict)
