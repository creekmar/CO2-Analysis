# Ming Creekmore 
# Class: Statistics for Bioinformatics
# Professor: Dr. Babbitt
# file: Unit1b.R
# Purpose: Analyzing the data of the CO2 built-in R dataset


# make a new folder structure for graphs
# CO2_graphs is the big folder that will hold folders of different groups of data
folder_name <- "CO2_graphs"
outpath <- file.path(getwd(), folder_name)
# file to write to
dir.create(outpath)
setwd(outpath)
file.create("output.txt")
filename <- file(file.path(outpath, "output.txt"))
sink(filename)

# This is a function that performs linear regression and residual plots on the data
# @name the name of the inner folder to create and the base name to give to the saved plots
# c1 c2, c3 c4, c5 c6 are the 3 groups of column ranges to get the data from
plant_regression <- function(name, c1, c2, c3, c4, c5, c6) {

    # making a folder to save all the charts made
    innerpath <- file.path(outpath, name)
    dir.create(innerpath)
    setwd(innerpath)

    # Plotting 3 separate graphs for each plant in CO2 dataset.
    # For specified columns
    # Plot of CO2 ambience concentration vs. CO2 uptake
    x <- CO2$conc[c1:c2]
    QN1 <- CO2$uptake[c1:c2]
    QN2 <- CO2$uptake[c3:c4]
    QN3 <- CO2$uptake[c5:c6]
    jpeg(paste(name,"1.jpeg"))
    plot(QN1~x, pch = 1, col = "black", xlab="CO2 Ambient Concentration", ylab="CO2 Uptake", main=name)
    dev.off()
    jpeg(paste(name,"2.jpeg"))
    plot(QN2~x, pch = 1, col = "blue", xlab="CO2 Ambient Concentration", ylab="CO2 Uptake", main=name)
    dev.off()
    jpeg(paste(name,"3.jpeg"))
    plot(QN3~x, pch = 1, col = "red", xlab="CO2 Ambient Concentration", ylab="CO2 Uptake", main=name)
    dev.off()

    # Plotting a graph with all plant data together
    new_x <- c(CO2$conc[c1:c2],CO2$conc[c3:c4],CO2$conc[c5:c6])
    QN <- c(CO2$uptake[c1:c2],CO2$uptake[c3:c4],CO2$uptake[c5:c6])
    jpeg(paste(name,"_all.jpeg"))
    plot(QN~new_x, pch = 1, col = "black", xlab="CO2 Ambient Concentration", ylab="CO2 Uptake", main=name)
    dev.off()

    # This was kind of cool, you can change the scales to be log based
    # But doesn't actually change the values
    jpeg(paste(name,"_log_scale.jpeg"))
    plot(QN~new_x, pch = 1, col = "black", log='x', xlab="CO2 Ambient Concentration with Log Scale", ylab="CO2 Uptake", main=name)
    dev.off()

    # Attempt Linearizing data by taking the log of the x-axis
    log_x <- log(new_x)
    jpeg(paste(name,"_log.jpeg"))
    plot(QN~log_x, pch = 1, col = "black", xlab="Log of CO2 Ambient Concentration", ylab="CO2 Uptake", main=name)
    dev.off()
    data <- data.frame(conc=log_x, uptake=QN)

    # doing linear regression
    model <-lm(data$uptake~data$conc)
    # printing model summary to file
    print(summary(model))
    # checking if residuals are normally distributed
    res <- resid(model)
    jpeg(paste(name,"_resid.jpeg"))
    plot(fitted(model),res)
    abline(0,0)
    dev.off()
    # another test for residuals
    jpeg(paste(name,"_qqnorm.jpeg"))
    qqnorm(res)
    qqline(res)
    dev.off()

    # go back to outer folder
    setwd(outpath)
    return(data.frame(conc=new_x,uptake=QN))

    # #shows outliers
    # boxplot(data$uptake)
    # boxplot(data$uptake)$out
    # data_no_out <- subset(data, !data$uptake%in%boxplot(data$uptake)$out)
    # jpeg(paste(name,"_no_outliers.jpeg"))
    # plot(data_no_out, pch = 1, col = "black")
    # dev.off()

}

# QN = Quebec nonchilled
QN = plant_regression("QN", 1,7,8,14,15,21)
# QC = Quebec Chilled
QC = plant_regression("QC", 22,28,29,35,36,42)
# MN = Mississippi Nonchilled
MN = plant_regression("MN", 43,49,50,56,57,63)
# MC = Mississippi Chilled
MC = plant_regression("MC", 64,70,71,77,78,84)

# performing 2 sample kolmogorov-smirnov test
print(ks.test(QN$uptake, QC$uptake))
print(ks.test(MN$uptake, MC$uptake))
print(ks.test(QN$uptake, MN$uptake))
print(ks.test(QC$uptake, MC$uptake))

# Perform two-way annova on whether plant type and treatment cause 
# different CO2 uptake
two_way <- aov(uptake ~ Type + Treatment, data = CO2)
print(summary(two_way))

# get row data for concentration type
# @start is the row to start on (assuming we start on first 7 rows)
#     Will get every row data with the concentration type as the specified row (every 7th row)
make_concentration_vectors <- function(start) {
    conc_v = vector()
    count = start
    concentration = CO2$conc[start]
    for(i in 1:12) {
        # print(count)
        conc_v <- c(conc_v, CO2$uptake[count])
        count = count+7
    }
    jpeg(paste(file.path(getwd(), concentration), ".jpeg"))
    hist(conc_v, main=paste("CO2 uptake for concentration", concentration), xlab = "CO2 uptake")
    dev.off()
    return(conc_v)
}

# Making histograms for each concentration type
innerpath <- file.path(outpath, "Concentration_Histograms")
dir.create(innerpath)
setwd(innerpath)
# print(getwd())
conc1 = make_concentration_vectors(1)
conc2 = make_concentration_vectors(2)
conc3 = make_concentration_vectors(3)
conc4 = make_concentration_vectors(4)
conc5 = make_concentration_vectors(5)
conc6 = make_concentration_vectors(6)
conc7 = make_concentration_vectors(7)
jpeg("all.jpeg")
hist(CO2$uptake, main="CO2 uptake for concentration")
dev.off()
jpeg("QN.jpeg")
hist(QN$uptake, main="QN")
dev.off()
jpeg("QC.jpeg")
hist(QC$uptake, main="QC")
dev.off()
jpeg("MN.jpeg")
hist(MN$uptake, main="MN")
dev.off()
jpeg("MC.jpeg")
hist(MC$uptake, main="MC")
dev.off()
setwd(outpath)

########################
# Random Forest ML
########################
library('ggplot2')
library('randomForest')

# new folder
innerpath <- file.path(outpath, "Random_Forest")
dir.create(innerpath)
setwd(innerpath)

ggplot(CO2, aes(uptake, conc, color = Type:Treatment)) + 
    geom_point() + labs(title = "All Data Scatterplot")
ggsave("scatter_all.jpeg")

new_d <- data.frame(CO2)
new_d$combined <- as.factor(paste(new_d$Type, new_d$Treatment))

# Dividing between training data and testing data
train <- data.frame(matrix(ncol=5, nrow=0))
colnames(train) <- c('Plant', 'Type', 'Treatment', 'conc', 'uptake')
test <- data.frame(matrix(ncol=5, nrow=0))
colnames(test) <- c('Plant', 'Type', 'Treatment', 'conc', 'uptake')

count = 1
for(i in 1:21){
    train <- rbind(train, new_d[c(count,(count+1)), ])
    test <- rbind(test, new_d[c((count+2),(count+3)), ])
    count = count + 4
}

# Random Forest
model <- randomForest(combined~conc+uptake, train, ntree = 500)
predict <- predict(model, test, probability = FALSE, decision.values = TRUE)
plot1 <- ggplot(train, aes(uptake, conc, color = combined)) + geom_point() + ggtitle("Training Data Scatterplot")
ggsave("train_scatter.jpeg")
plot2 <- ggplot(test, aes(uptake, conc, color = combined)) + geom_point() + ggtitle("Test Actual Scatterplot")
ggsave("test_actual.jpeg")
plot3 <- ggplot(test, aes(uptake, conc, color = predict)) + geom_point() + ggtitle("Test Predict Scatterplot")
ggsave("test_predict.jpeg")
print(summary(model))

correct = 0
count = 1
for(i in 1:42) {
    if(test$combined[i] == predict[i]) {
        correct = correct + 1
    }
    count = count + 1
}
accuracy = correct/(count-1)
print(paste("Accuacy: ", accuracy))

setwd(outpath)
sink()
close(filename)
print("I'm done")