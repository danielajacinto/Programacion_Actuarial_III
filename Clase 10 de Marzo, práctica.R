z <- 5
i <- 1
resultado <- vector("numeric", 0)

while (z>=3 && z<=10) {
    length(resultado) <- length(resultado)+1
    resultado[i] <- z
    
    moneda <- rbinom(1,1,0.5)
    
    if (moneda==1){
        z <- z+1
        resultado[i]<-z
        
    } else {
        z <- z-1
        resultado[i]<- z
    }
    i <- i+1
}