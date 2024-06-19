# Imports ----
library(dagitty)
library(rethinking)
library(tidyverse)

# import real data
d <- read.csv("data.csv")
d <- data.frame()



# Run the model
m1 <- ulam(
  alist(
    E_G1 ~ dnorm(mu1, sigma1),
    mu1 <- b0_1 + b1_1*P_G1,
    b0_1 ~ dnorm(0,1),
    b1_1 ~ dnorm(0,1),
    sigma1 ~ dexp(1),
    
    E_G2 ~ dnorm(mu2, sigma2),
    mu2 <- b0_2 + b1_2*P_G2,
    b0_2 ~ dnorm(0,1),
    b1_2 ~ dnorm(0,1),
    sigma2 ~ dexp(1)
  ), data=d
)
plot(precis(m1))

P_seq1 <- seq(from=min(d$P_G1), to=max(d$P_G1), length.out=100)
P_seq2 <- seq(from=min(d$P_G2), to=max(d$P_G2), length.out=100)
mu <- link(m1, data=list(P_G1=P_seq1, P_G2=P_seq2))
mu_mean_1 <- apply(mu$mu1, 2, mean)
mu.PI_1 <- apply(mu$mu1, 2, PI)
mu_mean_2 <- apply(mu$mu2, 2, mean)
mu.PI_2 <- apply(mu$mu2, 2, PI)

plot(E_G1 ~ P_G1, data=d, col="blue")
lines(P_seq, mu_mean_1, lwd=2)
shade(mu.PI_1, P_seq)

points(E_G2 ~ P_G2, data=d, col="red")
lines(P_seq, mu_mean_2, lwd=2)
shade(mu.PI_2, P_seq)

dens(post$b1_1, show.HPDI = 0.95, col=rgb(0,0,1,1/4), xlim=c(0, 2.5))
dens(post$b1_2, show.HPDI = 0.95, col=rgb(1,0,0,1/4), add = TRUE)
