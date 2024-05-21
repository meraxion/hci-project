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

post <- extract.samples(msim1)
coeffs <- post$b
dens(coeffs[,1])
dens(coeffs[,2])

# So, here it...kind of retrieves the effect? It finds that, correctly, Game 1 has on average 1.5 higher enjoyment
# than Game 2, but doesn't find the "right" values for the two coeffs.
# Something to think about.

## Personality





# Statistical Model ----
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




# Plotting ----