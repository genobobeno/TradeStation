
setwd("~/QuantFinance/Cointegration")
source("CointegrateFunctions.R")

#####################################################
###### Start here with your pair to explore.  #######
#####################################################

PAIR = c("JPM","XLF")       #  If you put in more symbols, you'll get them back in DATA
P = c(0.01,1.0,1.0)         #  mu,kappa,sigma
INPUT = c(100,0.1)          #  Days back, step size

# This line will fill the variables you reverse in the next 3 lines col.[sec.A,sec.B,date]
DATA = GetPair(PAIR)  

# Data Step
#sec.A <- rev(col.sec.A.adj.price); sec.B <- rev(col.sec.B.adj.price); ref.date <- rev(col.date)
sec.A <- DATA[,PAIR[1]]; sec.B <- DATA[,PAIR[2]]; ref.date <- DATA$DATE

# Initialize parameters
mu <- P[1]; kappa <- P[2]; sigma <- P[3]

# Initialize estimates
M <- INPUT[1]; step <- INPUT[2]
beta.vec <- seq(from = step,by = step, length.out = M) 
mu.vec <- numeric(M); kappa.vec <- numeric(M) 
sigma.vec <- numeric(M); llk.vec <- numeric(M)

for (i in 1:M) {
    X <- sec.A - beta.vec[i]*sec.B;

    par <- c(mu, kappa, sigma);

    MLE1 <- optim(par, method="L-BFGS-B", fn=fn.ou,
                  lower=c(-Inf, 0.001, 0.001), upper=c(Inf,50.000,10.000), x=X,
                  hessian = FALSE, control = list(maxit = 1000));
    mu.vec[i] <- MLE1$par[1];
    kappa.vec[i] <- MLE1$par[2];
    sigma.vec[i] <- MLE1$par[3];
    llk.vec[i] <- MLE1$value[1];
}    

i0 <- which(llk.vec==min(llk.vec))

beta <- beta.vec[i0]
X <- sec.A - beta*sec.B

mu <- mu.vec[i0]; kappa <- kappa.vec[i0]; sigma <- sigma.vec[i0]; llk <- llk.vec[i0]

s.score <- (X - mu) / (sigma / sqrt(2 * kappa))

#Plot the log likelihood:
plot(llk.vec, type='l')

#Plot the s-score:
plot(s.score, type='l')
