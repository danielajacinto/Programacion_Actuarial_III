drunkwalk <- function(begin, n){
    numero <- vector("numeric", 0)
    for(i in 1:n) {
        
        numero[i] <- begin
        length(numero) <- length(numero) + 1
        
        moneda <- rbinom(1, 1, 0.5)
        
        if (moneda==1){
            begin <- begin + 1
            
        } else {
            begin <- begin - 1
        }
    }

numero 
plot(numero, type=l)
}

