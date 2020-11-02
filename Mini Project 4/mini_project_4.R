# Question 1
# Populatioon corelation between gpa and act.

# Take a scatterplot of gpa against act
student = read.csv(file=file.path("./Mini Project 4/gpa.csv"))
gpa = student$gpa
act = student$act
plot(gpa, act, main="Scatterplot of GPA VS ACT", xlab="GPA", ylab="ACT")
abline(lm(act~gpa), col="red") 
# Better scatterplot with ggplot2
library(ggplot2)
ggplot(data=student, aes(x=gpa, y=act)) + # scatterplot
  geom_point(alpha=0.3) + # use alpha to represent frequency
  stat_smooth(method="lm", formula = y ~ x) + # linear relationship
  labs(title = "Scatterplot of GPA VS ACT") + # add title
  theme(plot.title = element_text(hjust = 0.5)) # center the title

# Bootstrap for the correlation between gpa and act
# Use install.package("boot")
library(boot)
cor(gpa, act)

# Non-parameteric bootstrap function
covarience.npar <- function(X, indices) {
  g = X$gpa[indices]
  a = X$act[indices]
  cor(g, a)
}
covarience.npar.boot = boot(student, covarience.npar,
                            R=999, sim="ordinary", stype="i")
covarience.npar.boot

# Confidence Interval
mean(covarience.npar.boot$t)
var(covarience.npar.boot$t)
# Get the 95% confidence interval by Percentile Bootstrap Method
sort(covarience.npar.boot$t)[c(25, 975)]

# Get the 95% confidence interval by boot.ci to check with percentile method
boot.ci(covarience.npar.boot, conf=0.95, type="perc")
# Specifically declare to use percentile method to avoid warning:
# In boot.ci(covarience.npar.boot) :
#   bootstrap variances needed for studentized intervals

# Question 2
# Confidence interval about manufacturing process can be established locally.

# (a)
voltage = read.csv(file=file.path("./Mini Project 4/VOLTAGE.csv"))

voltage.remote = voltage$voltage[voltage$location == 0]
voltage.local = voltage$voltage[voltage$location == 1]

voltage.histInterval = 0.2
voltage.breaks = seq(min(min(voltage.remote), min(voltage.local)),
                     max(max(voltage.remote), max(voltage.local)) +
                       voltage.histInterval,
                     voltage.histInterval)
hist(voltage.remote, breaks=voltage.breaks, probability=T,
     xlab="voltage", main="Histogram of Remote")
hist(voltage.local, breaks=voltage.breaks, probability=T,
     xlab="voltage", main="Histogram of Local")
# Draw two histograms together with ggplot2
voltage.locationName = ifelse(voltage$location == 0, "remote", "local")
df <- data.frame(
  location = voltage.locationName,
  voltage = voltage$voltage
)
ggplot(df, aes(x=voltage, color=location, fill=location)) +
  geom_histogram(bins=10, position="dodge") + # histgram
  labs(title = "Histogram of Voltage") + # add title
  theme(plot.title = element_text(hjust = 0.5)) # center the title

qqnorm(voltage.remote, ylab="voltage", main="QQ Plot of Remote")
qqline(voltage.remote)
qqnorm(voltage.local, ylab="voltage", main="QQ Plot of Local")
qqline(voltage.local)
boxplot(voltage.remote, voltage.local, boxwex=.3, names=c("Remote", "Local"),
        col=c("grey", "white"), main="Voltage Distribution", ylab="voltage")

# (b)
# Confidence Interval
voltage.center = mean(voltage.remote - voltage.local)
voltage.remote.se = var(voltage.remote) / length(voltage.remote)
voltage.local.se = var(voltage.local) / length(voltage.local)
voltage.margin = qnorm(0.975) * sqrt(voltage.remote.se + voltage.local.se)
voltage.ci = voltage.center + c(-1, 1) * voltage.margin
voltage.ci

# T-test
t.test(voltage.remote, voltage.local, alternative="two.sided",
       paired=F, var.equal=F, conf.level=0.95)

# Question 3
# Analysis about the theoretical model for vapor pressure.
vapor = read.csv(file=file.path("./Mini Project 4/VAPOR.csv"))

# Diagram 1
lim = c(0, max(max(vapor$theoretical), max(vapor$experimental)) * 1.1)
plot(x=vapor$temperature, y=vapor$theoretical, type="l", col="red",
     xlab="temperature", ylab="vapor pressure", ylim=lim,
     main=paste("Vapor Pressure"))
lines(x=vapor$temperature, y=vapor$experimental, type="l", col="blue")
legend("bottomright", c("theoretical", "experimental"), fill=c("red", "blue"))

# Diagram 2
lim = c(min(min(vapor$theoretical), min(vapor$experimental)) * 0.9,
        max(max(vapor$theoretical), max(vapor$experimental)) * 1.1)
plot(x=vapor$theoretical, y=vapor$experimental, type="p", xlim=lim, ylim=lim,
     xlab="theoretical", ylab="experimental", main=paste("Vapor Pressure"))
abline(0, 1)

vapor.difference = vapor$theoretical- vapor$experimental

# Diagram 3
qqnorm(vapor.difference, ylab="theoretical - experimental",
       main="QQ Plot of Difference")
qqline(vapor.difference)

# Confidence Interval
vapor.n = length(vapor$temperature)
vapor.mean = mean(vapor.difference)
vapor.sd = sd(vapor.difference)
vapor.margin = qt(0.975, vapor.n -1) * vapor.sd / sqrt(vapor.n)
vapor.ci = vapor.mean + c(-1, 1) * vapor.margin
vapor.ci

# T-test
t.test(vapor$theoretical, vapor$experimental, alternative="two.sided",
       paired=T, var.equal=F, conf.level=0.95)
qt(0.975, vapor.n -1)
