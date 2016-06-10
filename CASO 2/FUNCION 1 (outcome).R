mejor <- function(estado,resultado){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    z <- levels(factor(data[,7]))
    w <- c("infarto", "falla", "neumonia")
    
    if (estado %in% z == F){
        stop("estado inválido")
        break
    }
    if (resultado == "infarto") s <- 11
    else if (resultado == "falla") s <- 17
    else if (resultado == "neumonia") s <- 23
    else if (resultado %in% w == F){
        stop("resultado inválido")
        break
    }
    mydata <- data[data$State == estado,]
    mnd <- mydata[,c(2,s)]
    if (sum(mnd[,2]=="Not Available") < 1) {
        out <- mnd[order(as.numeric(mnd[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
        
    }
    else {
        final <- mnd[- grep("Not", mnd[,2]),]
        out <- final[order(as.numeric(final[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
    }
}

mejor("TX", "infarto")
mejor("TX", "falla")
mejor("MD", "infarto")
mejor("MD", "neumonia")
mejor("BB", "infarto")
mejor("NY", "infartu")