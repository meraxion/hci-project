# Imports ----
library(dagitty)
library(rethinking)

set.seed(41)

# Causal Model ----
## Simple ----
gs <- dagitty("dag{G -> E
              G [exposure]
              E [outcome]}")
impliedConditionalIndependencies(gs)



## Personality ----
gp <- dagitty("dag{G -> E <- P
              G [exposure]
              E [outcome]}")
impliedConditionalIndependencies(gp)

# Generative Model ----

## Simple
"
E ~ dnorm(mu, sigma),
mu <- b0 + b[G],
b0 ~ dnorm(0, 1),
b[G] ~ dnorm(0, 1),
sigma ~ dexp(1)
"

## Personality
"
E ~ dnorm(mu, sigma),
mu <- b0 + b1[G] + b2[P],
b0 ~ dnorm(0,1),
b1[G] ~ dnorm(0,1),
b2[P] ~ dnorm(0,1),
sigma ~ dexp(1)
"

# Simulation ----
## Simple ----
## Sim 1: Equal enjoyment in both games:
N <- 100 # 100 participants
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
E <- rnorm(N)

dsim1 <- data.frame(N, G, E)

msim1 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 ~ dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim1
)
precis(msim1, depth=2)

post <- extract.samples(msim1)
coeffs <- post$b
dens(coeffs[,1])
dens(coeffs[,2])



## Sim 2: Game 1 is much more fun than game 2:
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(1.5, 0)
E <- rnorm(N, Gbs[G])
  
dsim2 <- data.frame(N, G, E)

msim2 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 ~ dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim2
)
precis(msim2, depth=2)

post <- extract.samples(msim2)
coeffs <- post$b
dens(coeffs[,1])
dens(coeffs[,2])

# So, here it...kind of retrieves the effect? It finds that, correctly, Game 1 has on average 1.5 higher enjoyment
# than Game 2, but doesn't find the "right" values for the two coeffs.
# Something to think about.

## Personality
# No effect sim
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(0,0)
Pbs <- c(0,0)
E <- rnorm(N, Gbs[G] + Pbs[G])

dsim3 <- data.frame(N, G, E)

msim3 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[G],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim3
)
precis(msim3, depth=2)

post <- extract.samples(msim3)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Game type effect sim
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(1.5,0)
Pbs <- c(0,0)
E <- rnorm(N, Gbs[G] + Pbs[G])

dsim4 <- data.frame(N, G, E)

msim4 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[G],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim4
)
precis(msim4, depth=2)

post <- extract.samples(msim4)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Personality effect sim
N = 1000
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(0,0)
P <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 have personality type 1, 50 personality type 2
Pbs <- c(1.5,0)
E <- rnorm(N, Gbs[G] + Pbs[P])

dsim5 <- data.frame(N, G, E, P)

msim5 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[P],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim5
)
precis(msim5, depth=2)

post <- extract.samples(msim5)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Model with covariance


# Dual effect sim




# Statistical Model ----
## Simple
m1 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 <- dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=d
)
precis(m1, depth=2)


## Regression Personality

## Interaction



# Plotting ----