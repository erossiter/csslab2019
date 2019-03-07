rm(list = ls())
library(SDMTools)

set.seed(63110)

clust.1 <- rnorm(30, 3.2, 2)
clust.2 <- rnorm(30, 6, 3)

all.dat <- data.frame(x = c(clust.1, clust.2),
                      p.clust.1 = rep(NA, length(c(clust.1, clust.2))),
                      p.clust.2 = rep(NA, length(c(clust.1, clust.2))))

## initial values
clust.1.mu <- 3
clust.1.sigma <- 2
clust.1.pi <- .5

clust.2.mu <- 3.1
clust.2.sigma <- 1
clust.2.pi <- .5

tolerance <- .015
log.lik <- Inf

not.converged <- TRUE
while(not.converged){    
    ## E Step
    all.dat$p.clust.1 <- (dnorm(all.dat$x, clust.1.mu, clust.1.sigma) * clust.1.pi)
    all.dat$p.clust.2 <- (dnorm(all.dat$x, clust.2.mu, clust.2.sigma) * clust.2.pi)
    all.dat[,c("p.clust.1", "p.clust.2")] <- all.dat[,c("p.clust.1", "p.clust.2")]/rowSums(all.dat[,c("p.clust.1", "p.clust.2")])
    
    ## M Step
    clust.1.mu <- wt.mean(all.dat$x, all.dat$p.clust.1)
    clust.1.sigma <- wt.sd(all.dat$x, all.dat$p.clust.1)
    clust.1.pi <- mean(all.dat$p.clust.1)

    clust.2.mu <- wt.mean(all.dat$x, all.dat$p.clust.2)
    clust.2.sigma <- wt.sd(all.dat$x, all.dat$p.clust.2)
    clust.2.pi <- mean(all.dat$p.clust.2)

    ## Check log-likelihood (this is also ugly)
    new.log.lik <- sum(log((dnorm(all.dat$x,
                                  clust.1.mu,
                                  clust.1.sigma) * clust.1.pi) +
                           (dnorm(all.dat$x,
                                  clust.2.mu,
                                  clust.2.sigma) * clust.2.pi)))

    # Stopping condition
    if(abs(new.log.lik - log.lik[length(log.lik)]) < tolerance){
        break
    } else {
        log.lik <- c(log.lik, new.log.lik)
    }
}


# Compare to truth
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, .2), axes = FALSE)
axis(1, labels = FALSE)

# Add estimated distributions
curve(dnorm(x, mean = clust.1.mu, sd = clust.1.sigma), add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(dnorm(x, mean = clust.2.mu, sd = clust.2.sigma), add = TRUE, col = "red", lwd = 2, lty = 2)

# Add true values
curve(dnorm(x, mean = 3.2, sd = 2), add = TRUE, col = "red", lwd = 2)
curve(dnorm(x, mean = 6, sd = 3), add = TRUE, col = "blue", lwd = 2)

# Add data
rug(all.dat$x[all.dat$p.clust.1 > .5], col = "blue", lwd = 3)
rug(all.dat$x[all.dat$p.clust.1 <= .5], col = "red", lwd = 3)
