# Imports ----
library(dagitty)
library(rethinking)
library(tidyverse)

# import real data
data <- read.csv("data.csv")
# Keep only the columns we need
data <- select(data, participantID = Q18, Age = Q4, personality1 = Q9_1, personality2 = Q9_2,
            personality3 = Q9_3, personality4 = Q9_4, personality5 = Q9_5, personality6 = Q9_6,
            excitement = Q11, stress = Q12, gaming_exp = Q14, impression = Q15, enjoyment = Q20,
            game_version = Q5, coins = Q6, enemies = Q8)
# Delete rows 1 and 2 - these just have some Qualtrics meta-data we don't need
data <- data[-c(1:2),]
# There we have a full data-set containing all participants and both game types.
# Here, I convert everything to numeric
data <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))
# Now, I need to make a personality measure:
# This is done by [taking the average/adding then normalizing/something else] the personality sub-questions
# For now, I'm just taking the average
data=within(data,
          { personality = rowMeans( cbind(personality1,personality2, personality3,
                                          personality4, personality5, personality6), na.rm = T)})


# And then I can save this data-set:
write.csv(data, "data_clean.csv")
# After that, I split the data-set in two based on the game played:
d1 <- data[data$game_version == 1, ] # This is Moody
d2 <- data[data$game_version == 2, ] # This is Hyper

summary(data)
summary(d1)
summary(d2)

# Then, I make the data frame the model will use:
d <- data.frame(E_G1 = d1$enjoyment, P_G1 = d1$personality,
                E_G2 = d2$enjoyment, P_G2 = d2$personality)
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

traceplot(m1)

post <- extract.samples(m1)

from <- min(d$P_G1, d$P_G2)
to <- max(d$P_G1, d$P_G2)

P_seq1 <- seq(from=from, to=to, length.out=100)
P_seq2 <- seq(from=from, to=to, length.out=100)
mu <- link(m1, data=list(P_G1=P_seq1, P_G2=P_seq2))
mu_mean_1 <- apply(mu$mu1, 2, mean)
mu.PI_1 <- apply(mu$mu1, 2, PI)
mu_mean_2 <- apply(mu$mu2, 2, mean)
mu.PI_2 <- apply(mu$mu2, 2, PI)

plot(E_G1 ~ P_G1, data=d, col="blue",
     main = "Linear Regression Personality ~ Enjoyment",
     xlab = "Personality", 
     ylab = "Enjoyment")
lines(P_seq1, mu_mean_1, lwd=2, col = "blue")
shade(mu.PI_1, P_seq1)

points(E_G2 ~ P_G2, data=d, col="red")
lines(P_seq2, mu_mean_2, lwd=2, col = "red")
shade(mu.PI_2, P_seq2)

legend("topleft", 
       legend = c("Moody", "Hyper"),
       col = c("blue", "red"),
       lty = 1)

dens(post$b0_1, show.HPDI = 0.95, col=rgb(0,0,1,1/4), xlim=c(-1.0, 4.5),
     main = "Intercept Density Plots with 0.95 HDI")
dens(post$b0_2, show.HPDI = 0.95, col=rgb(1,0,0,1/4), add = TRUE)
legend("topleft", legend = c("Moody", "Hyper"),
       col = c("blue", "red"),lty = 1)

dens(post$b1_1, show.HPDI = 0.95, col=rgb(0,0,1,1/4), xlim=c(0, 2.5),
     main = "Personality Coefficient Density Plots with 0.95 HDI")
dens(post$b1_2, show.HPDI = 0.95, col=rgb(1,0,0,1/4), add = TRUE)
legend("topright", legend = c("Moody", "Hyper"),
       col = c("blue", "red"),lty = 1)
