xtrain <-read.table("./train/X_train.txt")
ytrain <-read.table("./train/Y_train.txt")
strain <-read.table("./train/subject_train.txt")

xtest <- read.table("./test/X_test.txt")
ytest <- read.table("./test/y_test.txt")
stest <- read.table("./test/subject_test.txt")

dataX <- rbind(xtrain, xtest)
dataY <- rbind(ytrain, ytest)
dataS <- rbind(strain, stest)

rm(xtrain)
rm(ytrain)
rm(strain)
rm(xtest)
rm(ytest)
rm(stest)

caract <- read.table("./features.txt")

promedioStdIndex <- grep("mean\\(\\)|std\\(\\)", caract[, 2])

dataX <- dataX[, promedioStdIndex]
names(dataX) <- gsub("\\(\\)", "", caract[promedioStdIndex, 2])
names(dataX) <- gsub("mean", "Mean", names(dataX)) 
names(dataX) <- gsub("std", "Std", names(dataX)) 
names(dataX) <- gsub("-", "", names(dataX)) 
rm(caract)
rm(promedioStdIndex)


activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2])) 
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8)) 
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8)) 
dataY[, 1] <- activity[dataY[, 1], 2]
names(dataY) <- "actividad"


names(dataS) <- "sujeto"
cleandata <- cbind(dataS, dataY, dataX)
rm(dataX)
rm(dataY)

SLen <- length(table(dataS)) 
activityLen <- dim(activity)[1]
colLen <- dim(cleandata)[2]
op <- as.data.frame(matrix(NA, nrow=SLen*activityLen, ncol=colLen))
colnames(op) <- colnames(cleandata)
f <- 1
for(i in 1:SLen) {
    for(j in 1:activityLen) {
        op[f, 1] <- sort(unique(dataS)[, 1])[i]
        op[f, 2] <- activity[j, 2]
        die1 <- i == cleandata$sujeto
        die2 <- activity[j, 2] == cleandata$actividad
        op[f, 3:colLen] <- colMeans(cleandata[die1&die2, 3:colLen])
        f <- f + 1
    }
}

write.table(op, "Completed.txt", row.name=FALSE)