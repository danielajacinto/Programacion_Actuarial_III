corr <- function(directorio, horizonte=0){
    #setwd(directorio)
    correlaciones <- vector("numeric",0)
    m <- 1
    for (c in 1:332){
        
        id1<-formatC(c,width = 3 ,flag = "0")
        readen <- read.csv(paste(id1, ".csv",sep=""),header = T)
        mydata <- data.frame(readen$sulfate,readen$nitrate)
        completo <- mydata[complete.cases(mydata),]
        l <- nrow(completo)
        
        if (l>horizonte){
            length(correlaciones) <- length(correlaciones)+1
            correlaciones[m] <- cor(completo[,1],completo[,2])
            m <- m+1
        }
    }
    correlaciones
}
cor <- corr("specdata", 150)
head(cor)