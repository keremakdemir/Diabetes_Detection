#import libraries
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)

#Read data and check the first few rows
diabetes <- read.csv(file='diabetes.csv', header=TRUE)
head(diabetes)

names(diabetes)[names(diabetes) == 'DiabetesPedigreeFunction'] <- 'DPF'
names(diabetes)[names(diabetes) == 'BloodPressure'] <- 'BloodP'
names(diabetes)[names(diabetes) == 'SkinThickness'] <- 'SkinT'
names(diabetes)[names(diabetes) == 'Pregnancies'] <- 'Pregnancy'

#summary statistics
cat('Descriptive statistics of the data:')
summary(diabetes)

#correlation plot
png(filename = "Correlation.png",width = 4, height = 4, units = 'in', res = 400)
M<-cor(diabetes)
corrplot(M, order = 'AOE')
dev.off()

#pairs plot
png(filename = "Pair_plot.png",width = 10, height = 10, units = 'in', res = 175)
ggpairs(diabetes, aes(colour = Outcome, alpha = 0.4)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

cat('There seems to be no strong correlation among different variables.')

#changing outcome to factor
diabetes$Outcome <- as.factor(diabetes$Outcome)

#plotting density graphs for predictors
num_predictors <- length(diabetes)-1
names_predictors <- colnames(diabetes)

png(filename = "Density_plot.png",width = 13, height = 6, units = 'in', res = 200)
den1 <- ggplot(diabetes, aes(x = Pregnancy, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Pregnancies") + theme_bw() + theme(legend.position = "none")
den2 <- ggplot(diabetes, aes(x = Glucose, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Glucose") + theme_bw() + theme(legend.position = "none")
den3 <- ggplot(diabetes, aes(x = BloodP, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Blood Pressure") + theme_bw() + theme(legend.position = "none")
den4 <- ggplot(diabetes, aes(x = SkinT, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Skin Thickness") + theme_bw() + theme(legend.position = c(0.71, 0.82))
den5 <- ggplot(diabetes, aes(x = Insulin, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Insulin") + theme_bw() + theme(legend.position = "none")
den6 <- ggplot(diabetes, aes(x = BMI, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Body Mass Index") + theme_bw() + theme(legend.position = "none")
den7 <- ggplot(diabetes, aes(x = DPF, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Diabetes Pedigree Func.") + theme_bw() + theme(legend.position = "none")
den8 <- ggplot(diabetes, aes(x = Age, fill = Outcome)) + geom_density(alpha = 0.5) + labs(y="Density", x="Age") + theme_bw() + theme(legend.position = "none")
grid.arrange(den1, den2, den3, den4, den5, den6, den7, den8, ncol=4)
dev.off()

#plotting boxplots for predictors
png(filename = "Box_plot.png",width = 13, height = 6, units = 'in', res = 200)
box1 <- ggplot(diabetes, aes(x = Outcome, y = Pregnancy))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Pregnancies")
box2 <- ggplot(diabetes, aes(x = Outcome, y = Glucose))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Glucose")
box3 <- ggplot(diabetes, aes(x = Outcome, y = BloodP))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Blood Pressure")
box4 <- ggplot(diabetes, aes(x = Outcome, y = SkinT))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = c(0.5, 0.8)) + labs(y="Skin Thickness")
box5 <- ggplot(diabetes, aes(x = Outcome, y = Insulin))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Insulin")
box6 <- ggplot(diabetes, aes(x = Outcome, y = BMI))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Body Mass Index")
box7 <- ggplot(diabetes, aes(x = Outcome, y = DPF))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Diabetes Pedigree Func.")
box8 <- ggplot(diabetes, aes(x = Outcome, y = Age))+ geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y="Age")
grid.arrange(box1, box2, box3, box4, box5, box6, box7, box8, ncol=4)
dev.off()

cat('It seems like output is mostly correlated with glucose levels. Other revelant predictors might be pregnancies, body mass index, and age, but their predictive power may be lower than glucose levels. It makes sense that glucose levels are the leading predictor for diabetes detection.')

