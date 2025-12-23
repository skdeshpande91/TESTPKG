# Test Quarto Article

Test quarto article

We begin by defining the Friedman function.

``` r
friedman_func <- function(df){
  if(ncol(df) < 5 ) stop("df must have at least 5 columns")
  if(!all(abs(df[,1:5]-0.5) <= 1)){
    stop("all entries in the first 5 columns of df must be between 0 and 1")
  } else{
    
    return(10*sin(pi*df[,1] * df[,2]) + 
           20 * (df[,3] - 0.5)^2 + 
           10 * df[,4] + 
           5 * df[,5])
  }
}
```

Although the function depends on only 5 covariates, for this
demonstration, we will create a total of $p = 50$ predictors, each drawn
uniformly from the interval $\lbrack 0,1\rbrack.$ We will also add
$\mathcal{N}\left( 0,2.5^{2} \right)$ noise.

``` r
set.seed(724)
n_train <- 1000
p_cont <- 50
sigma <- 2.5

train_data <- data.frame(Y = rep(NA, times = n_train))
for(j in 1:p_cont) train_data[[paste0("X",j)]] <- runif(n_train, min = 0, max = 1)
mu_train <- friedman_func(train_data[,paste0("X",1:p_cont)])
train_data[,"Y"] <- mu_train + sigma * rnorm(n = n_train, mean = 0, sd = 1)
```

We now can fit a BART model to these data.

``` r
library(BART)
#> Loading required package: nlme
#> Loading required package: survival
fit <- wbart(x.train = train_data[,colnames(train_data) != "Y"],
             y.train = train_data$Y)
```
