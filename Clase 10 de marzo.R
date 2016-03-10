z <- 5
j <- 1
nuevo <- vector("numeric", 0)

while(z>=3 && z<=10) {
    length(nuevo) <- length(nuevo) + 1
    nuevo[j] <- z
    
    moneda <- rbinom(1, 1, 0.5)
    
    if(moneda==1){
        z <- z + 1
        nuevo[j] <- z
        
    } else {
        z <- z - 1
        nuevo[j] <- z
    }
    j <- j+1
}
print(nuevo)