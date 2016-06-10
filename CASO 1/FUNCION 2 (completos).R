completos <- function(directorio, id = 1:332){
    #setwd(directorio)
    nobs <- vector("numeric", length(id))
    m<-1
    for (c in id){
        id1<-formatC(c,width = 3 ,flag = "0")
        readen <- read.csv(paste(id1, ".csv",sep=""), header=T)
        z <- (readen$sulfate)
        w <- (readen$nitrate)
        mydata <- data.frame(z, w)
        nobs[m] <- nrow(mydata[complete.cases(mydata),])
        m<-m+1
    }
    data.frame(id=id,nobs=nobs)
}
completos("specdata",id=30:25)