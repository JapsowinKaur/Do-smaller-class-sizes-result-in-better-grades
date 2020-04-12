library(ggplot2)
class <- read.csv("class.csv")
head(class)

# Plotting class size vs mean score

plot(class,main = 'Class Size vs Mean Test Score')

# we observe an inverse relationship between class size and mean test score

abline(lm(class$mean_test_score ~class$class_size), data = class, col = 'red')

hist(class$class_size)
hist(class$mean_test_score)

n <- nrow(class)
print(n)

# Taking avg math as our exogenous variable. Generating a random normal distribution

set.seed(50)
avg_maths <- rnorm(n,67.7,9.4)
enrol <- rnorm(n,83,38.8)
PD <- rnorm(n, 13.1,12.6)
Reading_size <- rnorm(n, 28.6,6.2)
math_size <- rnorm(n, 29,6.3)
avg_verbal <- rnorm(n,74.7,7.4)

View(avg_maths)

class_new <- cbind(class,avg_maths,enrol, PD,Reading_size,math_size,avg_verbal)
class_size_dummy <- ifelse(class_new$class_size <30,1,0)
View(class_size_dummy)
class_new <- cbind(class_new, class_size_dummy)

class_subset <- subset(class_new, class_new$class_size <34 & class_new$class_size >25)
mean_test_score <- class_subset$mean_test_score
class_size <- class_subset$class_size
dummy <- class_subset$class_size_dummy
avgmath <- class_subset$avg_maths
enrollment <- class_subset$enrol
Percent_dis <- class_subset$PD 
RS <- class_subset$Reading_size
mathsize <- class_subset$math_size
avgverbal <- class_subset$avg_verbal

plot(class_subset)

# Running RDD

rdd <- lm(mean_test_score ~ avgmath + enrollment+ Percent_dis+ RS+ mathsize+avgverbal + dummy, data = class_subset)
summary(rdd)

ggplot(class_subset, aes(x = class_size, y = mean_test_score)) +
  geom_point() +
  geom_smooth(method = rdd, show.legend = F)

