s <- function(n,distribucion) {
    prom <- sapply(lapply(rep(n,n), distribucion), mean)
    hist(prom)
}
s(1000,runif)