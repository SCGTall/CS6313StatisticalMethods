# Question 1
# Consider the dataset roadrace.csv posted on eLearning. It contains
# observations on 5875 runners who finished the 2010 Beach to Beacon 10K Road
# Race in Cape Elizabeth, Maine. You can read the dataset in R using read.csv
# function.

# (a) Create a bar graph of the variable Maine, which identifies whether a
# runner is from Maine or from somewhere else (stated using Maine and Away).
# You can use barplot function for this. What can we conclude from the plot?
# Back up your conclusions with relevant summary statistics.

rr = read.csv(file=file.path("./Mini Project 2/roadrace.csv"))# read csv
isMaine.values = c(sum(rr$Maine == "Maine"), sum(rr$Maine == "Away"))
isMaine.names = c("Maine", "Away")
bp = barplot(isMaine.values, names.arg=isMaine.names,
             main="Where are runners from?", space=1, col=c("grey", "white"))
text(x=bp, y=isMaine.values+200,
     labels=isMaine.values, xpd=T)# show numbers above each column

# (b) Create two histograms the runners’ times (given in minutes) — one for the
# Maine group and the second for the Away group. Make sure that the histograms
# on the same scale. What can we conclude about the two distributions? Back up
# your conclusions with relevant summary statistics, including mean, standard
# deviation, range, median, and interquartile range.

time.maine = rr$Time..minutes[rr$Maine == "Maine"]
time.away = rr$Time..minutes[rr$Maine == "Away"]
#par(mfcol=c(1, 2))
interval = 10
maxTime = max(max(time.maine), max(time.away))# use max to define the scale
h1 = hist(time.maine, breaks=seq(0, maxTime + interval, interval))
h2 = hist(time.away, breaks=seq(0, maxTime + interval, interval))
maxCount = max(max(h1$counts), max(h2$counts))
maxDensity = max(max(h1$density), max(h2$density))
# Frequency
hist(time.maine, xlab="time(min)", main="Runners from Maine",
     breaks=seq(0, maxTime + interval, interval), ylim=c(0,round(maxCount*1.1)))
hist(time.away, xlab="time(min)", main="Runners away Maine",
     breaks=seq(0, maxTime + interval, interval), ylim=c(0,round(maxCount*1.1)))
# Probability
hist(time.maine, probability=T, xlab="time(min)", main="Runners from Maine",
     breaks=seq(0, maxTime + interval, interval), ylim=c(0,maxDensity*1.1))
hist(time.away, probability=T, xlab="time(min)", main="Runners away Maine",
     breaks=seq(0, maxTime + interval, interval), ylim=c(0,maxDensity*1.1))
#par(mfcol=c(1, 1))

analyze <- function(v, s=""){
  cat("Vector: ", s, "\n")
  cat("mean = ", mean(v), "\n")
  cat("standard deviation = ", sd(v), "\n")
  cat("min = ", min(v), "\n")
  cat("range = ", max(v) - min(v), "\n")
  cat("max = ", max(v), "\n")
  cat("median = ", median(v), "\n")
  cat("interquartile range = ", IQR(v), "\n")
}
analyze(time.maine, "Maine")
analyze(time.away, "Away")

# try to use ggplot to draw interleaved histograms
# Caution: you must include ggplot2 before use ggplot function
library(ggplot2)
time2 <- data.frame(
  state = rr$Maine,
  time = rr$Time..minutes.
)
ggplot(time2, aes(x=time, color=state, fill=state)) +
  geom_histogram(bins=10, position="dodge") + # histgram
  labs(title = "Time Frequency Distribution") + # add title
  theme(plot.title = element_text(hjust = 0.5)) # center the title
ggplot(time2, aes(x=time, color=state, fill=state)) +
  geom_density(alpha=.3) + # density
  labs(title = "Time Density Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# (c) Repeat (b) but with side-by-side boxplots.

boxplot(time.maine, time.away, boxwex=.3, names=c("Maine", "Away"),
        col=c("grey", "white"), main="Time Distribution", ylab="time(min)")

# (d) Create side-by-side boxplots for the runners’ ages (given in years) for
# male and female runners. What can we conclude about the two distributions?
# Back up your conclusions with relevant summary statistics, including mean,
# standard deviation, range, median, and interquartile range.

age.male = as.integer(rr$Age[rr$Sex == "M"])
age.female = as.integer(rr$Age[rr$Sex == "F"])
boxplot(age.male, age.female, boxwex=.3, names=c("Male", "Female"),
        col=c("grey", "white"), main="Age Distribution", ylab="Age")
analyze(age.male, "Male")
analyze(age.female, "Female")

# Question 2
# Consider the dataset motorcycle.csv posted on eLearning. It contains the
# number of fatal motorcycle accidents that occurred in each county of South
# Carolina during 2009. Create a boxplot of data and provide relevant summary
# statistics. Discuss the features of the data distribution. Identify which
# counties may be considered outliers. Why might these counties have the highest
# numbers of motorcycle fatalities in South Carolina?

mc = read.csv(file=file.path("./Mini Project 2/motorcycle.csv"))# read csv
boxplot(mc$Fatal.Motorcycle.Accidents,
        boxwex=.5, main="Fatal Motorcycle Accidents", ylab="count")
analyze(mc$Fatal.Motorcycle.Accidents, "Motorcycle Accidents")
fma = mc$Fatal.Motorcycle.Accidents
lb = max(min(fma), quantile(fma, prob=0.25) - 1.5 * IQR(fma))
hb = min(max(fma), quantile(fma, prob=0.75) + 1.5 * IQR(fma))
mc$County[fma < lb | fma > hb]
mc$Fatal.Motorcycle.Accidents[fma < lb | fma > hb]
