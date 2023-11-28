data <- read.csv("dataset.csv")
data <- subset(data, data$Target=="Graduate" | data$Target=="Dropout")
grad <- subset(data, data$Target=="Graduate")
dropout <- subset(data, data$Target=="Dropout")

gradSize <- nrow(grad)
dropoutSize <- nrow(dropout)

## 5 ----
# We will find the 95% CI for the difference of mean age with variance unknown
gradAge <- grad$Age.at.enrollment
dropoutAge <- dropout$Age.at.enrollment

qqnorm(gradAge, main = "Age at Enrollment for Graduates")
qqline(gradAge)

qqnorm(dropoutAge, main = "Age at Enrollment for Dropouts")
qqline(dropoutAge)

t.test(gradAge, dropoutAge, var.equal = TRUE, conf.level = .95)


## 6 ----
classTime = data.frame(Time = data$Daytime.evening.attendance, Outcome = data$Target)
grad_evening <- sum(subset(classTime, Time == '0')$Outcome == "Graduate")
grad_morning <- sum(subset(classTime, Time == '1')$Outcome == "Graduate")

pie(labels = c("Graduated", "Dropped Out"), c(grad_evening, sum(subset(classTime, Time == '0')$Outcome == "Dropout")), main = "Evening Success Rates", col = c("Green", "Red"))
pie(labels = c("Graduated", "Dropped Out"), c(grad_morning,sum(subset(classTime, Time == '1')$Outcome == "Dropout")), main = "Morning Success Rates", col = c("Green", "Red"))

prop.test(c(grad_evening, grad_morning), c(nrow(subset(classTime, Time == '0')), nrow(subset(classTime, Time == '1'))), correct = FALSE, alternative = "less")

