#Part 1
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)

#Mean
mean(age)
#Median
median(age)
#For mode, use the DescTool library
install.packages("DescTools")
library(DescTools)
Mode(age$age)

#For mid range, using range
mean(range(age))

#For first quartile
quantile(age, prob=c(.25), type=1)
#For third quartile
quantile(age, prob=c(.75), type=1)
#Or other option used for quantiles :
Q1Q3 <- quantile(age, probs=c(0,0.25,0.5,0.75,1))
Q1Q3 #then pick the two values requested

#Five number summary of the tuple
fivenum(age)
summary(age)

#Boxplot
boxplot(age, data = age, main="Ages of People", xlab="Data", ylab="Age")

-------------------------------------------------------------
  

#Part 2:
  
#Import dataset1
salary <-read.csv("RStudio/nba_2017_nba_players_with_salary.csv")
#Import dataset2
data <-read.csv("RStudio/nba_2017_players_stats_combined.csv")

#2)
data<-data.frame(data)
head(data)

#Exclude column "X"
data<-data[,2:37]
head(data)

#Rename column "Rk" to "Player Number"
names(data)[names(data)=="Rk"] <- "Player Number"
names(data[1])

#3)
#Number of rows
nrow(data)
#Number of columns
ncol(data)

#4)
#Summarising columns 4:6
part4 <- data[4:6]
summarized <- summary(part4)
summarized
#To check for missing values
sum(is.na(part4))
sum(is.na(part4[1]))
sum(is.na(part4[2]))
sum(is.na(part4[3]))

#5)
#Scatter plot (non-statistical geom) of FG vs MP
plot(data$MP, data$FG, main="Field Goals vs. Minutes Played", xlab = "MP", ylab="FG", pch=1)

#Bar chart (statistical geom) 
#Aggregation
aggregate(data[27], by = data[3], FUN = mean)

#Removing PF-C value
pts_per_pos <- aggregate(data[27], by = data[3], FUN = mean)
pts_per_pos <- pts_per_pos[-3,]
pts_per_pos

#Install ggplot2 to plot bar chart
install.packages("ggplot2")
library(ggplot2)
dev.off()

#basic bar chart plot
ggplot(pts_per_pos) + geom_bar(aes(x=POSITION, y = POINTS), stat = "identity")

#Adding visualizations to the plot
ggplot(pts_per_pos) + geom_bar(aes(x=POSITION, y = POINTS), stat = "identity", color = "blue", fill ="#EFA595")


#6) 
#Find missing values
sum(is.na(data[,4:36]))
colSums(is.na(data))

#7)
#mean of X3P. column without null values
mean(data$X3P., na.rm=TRUE)

#Replace missing values with mean
data$X3P.[is.na(data$X3P.)] <- mean(data$X3P., na.rm=TRUE)
sum(is.na(data$X3P.))

data$X3P.

#mean of FT. column without null values
mean(data$FT., na.rm=TRUE)

#Replace missing values with mean
data$FT.[is.na(data$FT.)] <- mean(data$FT., na.rm=TRUE)
sum(is.na(data$FT.))

data$FT.

#Creating missing values in categorical values
#sample random 10 rows 
rows.to.missing <- sample(row.names(data[28]),10)
rows.to.missing

#Replace TEAM of random sample with NA
data[rows.to.missing,]$TEAM <- NA

colSums(is.na(data[28]))
data$TEAM


install.packages("dplyr")
library(dplyr)

#Find count of players per team
player_per_team<-count(data, TEAM)
player_per_team[order(player_per_team$n, decreasing = TRUE), ]


#Find weighted mean of the data
weighted.mean(player_per_team$n)

#Take a random sample of Teams
random_sample<-sample(row.names(data),10)
#Populate random sample with missing values
data$TEAM[is.na(data$TEAM)] <-data[random_sample,]$TEAM
data[random_sample,]$TEAM
data$TEAM


#8)
# Normalize "PACE" 
install.packages("caret")
library(caret)
process <- preProcess(as.data.frame(data$PACE), method=c("range"))
process

View(process)
norm_scale<-predict(process, as.data.frame(data$PACE))
norm_scale

#9)
# Dummy variables for POSITION
dummy <- model.matrix(~ 0 + POSITION, data= data)
dummy


#10) 
# Creating random samples for data preparation
#Sampling training data with 50% of the total
train.rows <- sample(rownames(data), dim(data)[1]*0.5)
#Sampling validation data with 30% of the total
valid.rows <- sample(setdiff(rownames(data), train.rows),dim(data)[1]*0.3)
#testing data with 20% of the total
test.rows <- setdiff(rownames(data), union(train.rows, valid.rows))
#Populating samples
train.data <- data[train.rows, ]
valid.data <- data[valid.rows, ]
test.data <- data[test.rows, ]

View(test.data)

#Column deletion
data_final <- data[-c(29:36)]
data_final

#Merge statistics with salary 
players_by_salary <- merge(data_final, salary, by = intersect(names(data_final), names(salary)))
View(players_by_salary)

players_by_salary[c(2,1,31)]

#11) 
#Binning
breaks <- c(0, 5, 10, 15, 20, 25, 30, 35)
discretized_column <- cut(players_by_salary$POINTS, breaks = breaks, labels = c("Bin 1", "Bin 2", "Bin 3", "Bin 4","Bin 5","Bin 6","Bin 7"))

df <- data.frame(numeric_column = players_by_salary$POINTS, discretized_column)

df


#12)
pairs(players_by_salary[,c(5,6,27)], pch = 20)
      
#13) 
# boxplot for salaries
boxplot(players_by_salary$SALARY_MILLIONS , main ="Player salaries", xlab="Players", ylab="$ in Millions", col="blue")


#Line chart for random sample
plot(test.data$AST,type = "o", col = "red", xlab = "Player", ylab = "Assists", main = "Assists chart")
      
      