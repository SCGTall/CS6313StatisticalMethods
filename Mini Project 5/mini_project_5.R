# Question 1

# (a) Body temperature
bodytemp.heartrate <- read.csv(
  file=file.path("./Mini Project 5/bodytemp-heartrate.csv"))
male <-bodytemp.heartrate[which(bodytemp.heartrate$gender==1),]
female <- bodytemp.heartrate[which(bodytemp.heartrate$gender==2),]
#plots body temperature
boxplot(male$body_temperature, female$body_temperature, boxwex=.4,
        main = "Boxplots of Body Temperature", names = c('Male', 'Female'),
        col=c("grey", "white"), ylab="Body Temperature")
qqnorm(male$body_temperature, main='Q-Q Plot of Male', ylab="Body Temperature")
qqline(male$body_temperature)
qqnorm(female$body_temperature, main='Q-Q Plot of Female',
                                                      ylab="Body Temperature")
qqline(female$body_temperature)
#t,test function for the body temperature difference 
t.test(male$body_temperature, female$body_temperature,
       alternative ='two.sided', var.equal = F)

# (b) Heart rate
boxplot(male$heart_rate, female$heart_rate, boxwex=.4,
        main = "Boxplots of Heart Rate", names = c('Male', 'Female'),
        col=c("grey", "white"), ylab="Heart Rate")
qqnorm(male$heart_rate, main="Q-Q Plot of Male", ylab="Heart Rate")
qqline(male$heart_rate)
qqnorm(female$heart_rate, main="Q-Q Plot of Female", ylab="Heart Rate")
qqline(female$heart_rate)
#t,test function for the body temperature difference 
t.test(male$heart_rate, female$heart_rate,
       alternative='two.sided', var.equal=F)

# (c) Compare male and female
plot(x=bodytemp.heartrate$heart_rate, y=bodytemp.heartrate$body_temperature,
     type="p", xlab="Body Temperature", ylab="Heart Rate",
     main="Body Temperature and Heart Rate")
abline(lm(bodytemp.heartrate$body_temperature ~ bodytemp.heartrate$heart_rate))
cor(bodytemp.heartrate$body_temperature, bodytemp.heartrate$heart_rate)
#get the fitted regression line
lm(bodytemp.heartrate$body_temperature ~ bodytemp.heartrate$heart_rate)
#abline(temp)

#Scatter plots for body temperature and heart rate for males
plot(male$heart_rate, male$body_temperature,
     type="p", xlab="Body Temperature", ylab="Heart Rate",
     main="Scatter Plot for Male")
abline(lm(male$body_temperature ~ male$heart_rate))
cor(male$body_temperature, male$heart_rate)
lm(male$body_temperature ~ male$heart_rate)
# for females
plot(female$heart_rate, female$body_temperature, pch=1,
     type="p", xlab="Body Temperature", ylab="Heart Rate",
     main="Scatter Plot for Female")
abline(lm(female$body_temperature ~ female$heart_rate))
cor(female$body_temperature, female$heart_rate)
lm(female$body_temperature ~ female$heart_rate)
# mixed scatter plots
plot(bodytemp.heartrate$heart_rate, bodytemp.heartrate$body_temperature,
     col=c('blue', 'red')[unclass(bodytemp.heartrate$gender)],
     type="p", xlab="Body Temperature", ylab="Heart Rate",
     main="Scatter Plot for Male and Female")
legend("topleft", c("Male", "Female"), fill=c('blue', 'red'))
abline(lm(male$body_temperature ~ male$heart_rate), col='blue')
abline(lm(female$body_temperature ~ female$heart_rate), col='red')

# Question 2
# Large-sample z-interval and bootstrap percentile method interval.

# (a) Given lambda = 0.1 and n = 30, compare two intervals' accuracy.
# 1 - a = 0.95 => [0.025, 0.975].

# Large-sample z-interval
getInterval1 <- function(n, lambda) {
  population.mean = 1/lambda
  sample = rexp(n, rate=lambda)
  center = mean(sample)
  margin = qnorm(0.975) * sd(sample) / sqrt(n)
  if (center - margin > population.mean) {
    return (0)
  } else if (center + margin < population.mean) {
    return (0)
  } else {
    return (1)
  }
}

# Bootstrap percentile method interval
getInterval2 <- function(n, lambda) {  # 
  population.mean = 1/lambda
  sample = rexp(n, rate=lambda)
  lambda.est = 1 / mean(sample)
  sample.boot = c(mean(sample),
          replicate(999, expr=mean(rexp(n, rate=lambda.est)))) # 1000 times
  sample.boot = sort(sample.boot)
  if (sample.boot[25] > population.mean) {
    return (0)
  } else if (sample.boot[975] < population.mean) {
    return (0)
  } else {
    return (1)
  }
}

q2.round = 5000
q2.a.lambda = 0.1
q2.a.n = 30
q2.a.interval1s = replicate(q2.round, expr=getInterval1(q2.a.n, q2.a.lambda))
q2.a.accuracy1 = sum(q2.a.interval1s) / q2.round
q2.a.interval2s = replicate(q2.round, expr=getInterval2(q2.a.n, q2.a.lambda))
q2.a.accuracy2 = sum(q2.a.interval2s) / q2.round
sprintf("Lambda = %f, n = %d. Accuracy:", q2.a.lambda, q2.a.n)
sprintf("interval 1: %f; interval 2 %f.", q2.a.accuracy1, q2.a.accuracy2)

# (b) Traverse all lambdas and ns.
q2.b.lambdas = c(0.01, 0.1, 1, 10)
q2.b.ns = c(5, 10, 30, 100)
q2.intervals = c("interval 1", "interval 2")
q2.b.accuracies = array(0,
        dim=c(length(q2.b.lambdas), length(q2.b.ns), length(q2.intervals)))
for (i in 1:length(q2.b.lambdas)) {
  lambda = q2.b.lambdas[i]
  for (j in 1:length(q2.b.ns)) {
    n = q2.b.ns[j]
    interval1s = replicate(q2.round, expr=getInterval1(n, lambda))
    accuracy1 = sum(interval1s) / q2.round
    q2.b.accuracies[i, j, 1] = sum(interval1s) / q2.round
    interval2s = replicate(q2.round, expr=getInterval2(n, lambda))
    accuracy2 = sum(interval2s) / q2.round
    q2.b.accuracies[i, j, 2] = sum(interval2s) / q2.round
  }
}
print(q2.b.accuracies)

library(ggplot2)
# Different lambdas and one n.
for (j in 1:length(q2.b.ns)) {
  df = data.frame(
    lambda = rep(q2.b.lambdas, times=length(q2.intervals)),
    interval = rep(q2.intervals, each=length(q2.b.lambdas)),
    accuracy = c(q2.b.accuracies[, j, ]))
  img = ggplot(data=df, mapping=aes(x=lambda, y=accuracy, colour=interval)) +
    geom_line() + geom_point() + # line plus scatter
    scale_x_continuous(trans='log10') + # log scale in x coordinate
    ylim(0.5, 1) + # y range: [0, 1]
    labs(title=paste("Accuracy ( n = ", q2.b.ns[j], ")")) + # add title
    theme(plot.title = element_text(hjust = 0.5)) # center the title
  print(img)
}
# One lambda and different ns.
for (i in 1:length(q2.b.lambdas)) {
  df = data.frame(
    n = rep(q2.b.ns, times=length(q2.intervals)),
    interval = rep(q2.intervals, each=length(q2.b.ns)),
    accuracy = c(q2.b.accuracies[i, , ]))
  img = ggplot(data=df, mapping=aes(x=n, y=accuracy, colour=interval)) +
    geom_line() + geom_point() + # line plus scatter
    ylim(0.5, 1) + # y range: [0, 1]
    labs(title=paste("Accuracy ( lambda = ", q2.b.lambdas[i], ")")) + # add title
    theme(plot.title = element_text(hjust = 0.5)) # center the title
  print(img)
}

# (c) Analyse the result we got in (b)
q2.c.lambda = 0.1
q2.c.ns = c(5, 10, 30, 100, 300, 1000, 3000)
q2.c.accuracies = array(0,
          dim=c(length(q2.c.ns), length(q2.intervals)))
for (i in 1:length(q2.c.ns)) {
  n = q2.c.ns[i]
  interval1s = replicate(q2.round, expr=getInterval1(n, q2.c.lambda))
  accuracy1 = sum(interval1s) / q2.round
  q2.c.accuracies[i, 1] = sum(interval1s) / q2.round
  interval2s = replicate(q2.round, expr=getInterval2(n, q2.c.lambda))
  accuracy2 = sum(interval2s) / q2.round
  q2.c.accuracies[i, 2] = sum(interval2s) / q2.round
}
print(q2.c.accuracies)

q2.c.df = data.frame(
  n = rep(q2.c.ns, times=length(q2.intervals)),
  interval = rep(q2.intervals, each=length(q2.c.ns)),
  accuracy = c(q2.c.accuracies[, ]))
ggplot(data=q2.c.df, mapping=aes(x=n, y=accuracy, colour=interval)) +
  geom_line() + geom_point() + # line plus scatter
  scale_x_continuous(trans='log10') + # log scale in x coordinate
  ylim(0.5, 1) + # y range: [0, 1]
  labs(title=paste(
    "Accuracy with extra n ( lambda = ", q2.c.lambda, ")")) + # add title
  theme(plot.title = element_text(hjust = 0.5)) # center the title

