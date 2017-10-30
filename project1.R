#objective function to minimize Rastrigin
set.seed(1234)
DEoptim(fn = Rastrigin,lower = c(-5, -5),upper = c(5, 5),control = list(storepopfrom = 1))

library("quantmod")
tickers <- c("GE", "IBM", "JPM", "MSFT", "WMT")
getSymbols(tickers,
from = "2000-12-01",
to = "2010-12-31")
P <- NULL
for(ticker in tickers) {
tmp <- Cl(to.monthly(eval(parse(text = ticker))))
P <- cbind(P, tmp)
}
colnames(P) <- tickers
R <- diff(log(P))
R <- R[-1,]
mu <- colMeans(R)
sigma <- cov(R)

library("PerformanceAnalytics")
pContribCVaR <- ES(weights = rep(0.2, 5),
                     method = "gaussian",
                     portfolio_method = "component",
                     mu = mu,
                     sigma = sigma)$pct_contrib_ES
rbind(tickers, round(100 * pContribCVaR, 2))

#[,1] [,2] [,3] [,4] [,5]
#tickers "GE" "IBM" "JPM" "MSFT" "WMT"
#"21.61" "18.6" "25.1" "25.39" "9.3"

round(100 * mu , 2)
#GE IBM JPM MSFT WMT
#-0.80 0.46 -0.06 -0.37 0.0
round(100 * diag(sigma)^(1/2), 2)
#GE IBM JPM MSFT WMT
#8.90 7.95 9.65 10.47 5.33

#library(PerformanceAnalytics)
#first define objective function
obj <- function(w) {
if (sum(w) == 0) 
{w <- w + 1e-2}
w <- w / sum(w)
CVaR <- ES(weights = w,
method = 'gaussian',
portfolio_method = 'component',
mu = mu, sigma = sigma)
tmp1 <- CVaR$ES
tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
out <- tmp1 + 1e3 * tmp2
}


set.seed(1234)
out <- DEoptim(fn = obj,lower = rep(0, 5), upper = rep(1, 5))
out$optim$bestval

#[1] 0.1143538
wstar <- out$optim$bestmem
wstar <- wstar / sum(wstar)
rbind(tickers, round(100 * wstar, 2))
#par1 par2 par3 par4 par5
#tickers "GE" "IBM" "JPM" "MSFT" "WMT"
#"18.53" "21.19" "11.61" "13.37" "35.3"
100 * (sum(wstar * mu) - mean(mu))
[1] 0.04827935

#comparing to optim
out <- optim(par = rep(0.2, 5),
               fn = obj,
               method = "L-BFGS-B",
               lower = rep(0, 5),
               upper = rep(1, 5))
out$value
#[1] 0.1255093
out <- nlminb(start = rep(0.2, 5),
                objective = obj,
                lower = rep(0, 5),
                upper = rep(1, 5))
out$objective
#[1] 0.1158250

#if the investor is interested in the most risk diversified portfolio whose expected return is higher than the equal-weight portfolio

obj <- function(w) {
  if(sum(w) == 0) { w <- w + 1e-2 }
  w <- w / sum(w)
  contribCVaR <- ES(weights = w,
             method = "gaussian",
             portfolio_method = "component",
             mu = mu,
            sigma = sigma)$contribution
  tmp1 <- max(contribCVaR)
  tmp2 <- max(mean(mu) - sum(w * mu), 0)
  out <- tmp1 + 1e3 * tmp2
   }
set.seed(1234)
out <- DEoptim(fn = obj,
                 lower = rep(0, 5),
                  upper = rep(1, 5))
wstar <- out$optim$bestmem
wstar <- wstar / sum(wstar)
rbind(tickers, round(100 * wstar, 2))
#par1 par2 par3 par4 par5
#tickers "GE" "IBM" "JPM" "MSFT" "WMT"
"17.38" "19.61" "14.85" "15.19" "32.98"
100 * (sum(wstar * mu) - mean(mu))
[1] 0.04150506