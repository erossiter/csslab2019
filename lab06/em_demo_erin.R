rm(list = ls())
library(SDMTools)

set.seed(63110)

clust.1 <- rnorm(30, 3, 2)
clust.2 <- rnorm(30, 10, 2)
clust.3 <- rnorm(10, 15, 2)
N <- length(c(clust.1, clust.2, clust.3))
K <- 3

all.dat <- data.frame(x = c(clust.1, clust.2, clust.3),
                      p.clust.1 = rep(NA, length(N)),
                      p.clust.2 = rep(NA, length(N)),
                      p.clust.3 = rep(NA, length(N)))

## initial values
clust.mu <- c(3,4,5)

clust.sigma <- rep(1, K)

clust.pi <- rep(1/K, 3)

tolerance <- .015
log.lik <- Inf

not.converged <- TRUE
while(not.converged){    
    ## E Step - compute weights for each x_i and each mixture component k
    numerators <- matrix(NA, nrow = N, ncol = K)
    for(i in 1:nrow(all.dat)){
        for(j in 1:K){
            numerators[i,j] <- dnorm(all.dat$x[i], mean = clust.mu[j], sd = clust.sigma[j])*clust.pi[j]
        }
    }
    all.dat[,c(2:(K+1))] <- t(apply(numerators, 1, function(r) r/sum(r)))
    

    ## M Step
    for(j in 1:K){
        clust.mu[j] <- wt.mean(all.dat$x, all.dat[,j+1])
        clust.sigma[j] <- wt.sd(all.dat$x, all.dat[,j+1])
        clust.pi[j] <- mean(all.dat[,j+1])
    }

    
    ## Check log-likelihood (this is also ugly)
    new.log.lik <- 0
    for(j in 1:K){
        new.log.lik <- new.log.lik + sum(log((dnorm(all.dat$x, clust.mu[j], clust.sigma[j]) * clust.pi[j])))
    }

    
    # Stopping condition
    if(abs(new.log.lik - log.lik[length(log.lik)]) < tolerance){
        break
    } else {
        log.lik <- c(log.lik, new.log.lik)
    }
}


# Compare to truth
plot(1, type="n", xlab="", ylab="", xlim=c(-3, 15), ylim=c(0, .5), axes = FALSE)
axis(1, labels = FALSE)

# Add estimated distributions
for(j in 1:K){
    curve(dnorm(x, mean = clust.mu[j], sd = clust.sigma[j]), add = TRUE, col = j, lwd = 2, lty = 2)
}

# Add true values
curve(dnorm(x, mean = 3, sd = 2), add = TRUE, col = 1, lwd = 2)
curve(dnorm(x, mean = 10, sd = 2), add = TRUE, col = 2, lwd = 2)
curve(dnorm(x, mean = 15, sd = 2), add = TRUE, col = 3, lwd = 2)


# Add data
rug(all.dat$x[all.dat$p.clust.1 > .5], col = 1, lwd = 3)
rug(all.dat$x[all.dat$p.clust.2 > .5], col = 2, lwd = 3)
rug(all.dat$x[all.dat$p.clust.3 > .5], col = 3, lwd = 3)

hist(all.dat$x, freq = FALSE)

f_x <- function(x){
    dens <- 0
    for(j in 1:K){
        dens <- dens + dnorm(x, clust.mu[j], clust.sigma[j]) * clust.pi[j]
    }
    return(dens)
    
}
x_axis <- seq(min(all.dat$x), max(all.dat$x), .01)
lines(x = x_axis, y = f_x(x_axis))