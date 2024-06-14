# P101-1 dichotomy
dizero <- function(f, a, b, eps = 1e-6){
  if(f(a)*f(b) > 0)
    list(fail = "finding root is fail!")
  else{
    repeat{
      if(abs(b-a) < eps) break;
      x <- (a+b)/2;
      if(f(a)*f(x) < 0) b <- x else a <- x
    }
    list(root = x, fun = f(x))
  }
}

f1 <- function(x) 2*x^3 - 6*x - 1;
f2 <- function(x) exp(x-2) + x^3 - x;
f3 <- function(x) 1 + 5*x - 6*x^3 - exp(2*x);

u <- seq(-2, 2, 0.05);
v <- f1(u);
plot(u, v, type = "l")
# [-2,-1],[-1,0],[1,2]
uniroot(f1, c(-2,-1), tol = 1e-6)$root
dizero(f1, -2, -1, 1e-6)$root
uniroot(f1, c(-1,0), tol = 1e-6)$root
dizero(f1, -1, 0, 1e-6)$root
uniroot(f1, c(1,2), tol = 1e-6)$root
dizero(f1, 1, 2, 1e-6)$root

u <- seq(-1.5, 1.5, 0.05);
v <- f2(u);
plot(u, v, type = "l")
# [-1.5,-0.5],[-0.5,0.5],[0.5,1.5]
uniroot(f2, c(-1.5,-0.5), tol = 1e-6)$root
dizero(f2, -1.5, -0.5, 1e-6)$root
uniroot(f2, c(-0.5,0.5), tol = 1e-6)$root
dizero(f2, -0.5, 0.5, 1e-6)$root
uniroot(f2, c(0.5,1.5), tol = 1e-6)$root
dizero(f2, 0.5, 1.5, 1e-6)$root

u <- seq(-1, 1, 0.05);
v <- f3(u);
plot(u, v, type = "l")
# [-1.5,-0.5],[-0.6,0.4],[0.5,1.5]
uniroot(f3, c(-1.5,-0.5), tol = 1e-6)$root
dizero(f3, -1.5, -0.5, 1e-6)$root
uniroot(f3, c(-0.6,0.4), tol = 1e-6)$root
dizero(f3, -0.6, 0.4, 1e-6)$root
uniroot(f3, c(0.5,1.5), tol = 1e-6)$root
dizero(f3, 0.5, 1.5, 1e-6)$root



# P101-3 Newton
Newtons <- function(f, x0, eps, max_iter = 50){
  x <- x0
  for (i in 1:max_iter){
    result <- f(x)
    fx <- result$f
    if (max(abs(fx)) < eps){
      return(list(root = x, iter = i)) 
    }
    x <- x - fx%*%solve(result$J)
  }
  return(list(root = x, iter = i))
}
funs <- function(x){
  f <- c(x[1]^2 + x[2]^2 - 1,
         x[1]^3 - x[2])
  J <- matrix(c(2*x[1], 2*x[2],
              3*x[1]^2, -x[2]), nrow = 2, byrow = T)  # derivative
  list(f = f, J = J)
}
Newtons(funs, c(-0.8, 0.6), 1e-3)


# P101-6 binomial distribution
n <- 10; p <- 0.2;
samples <- rbinom(100, n, p);

funs <- function(n, p, moments){
  f <- c(n*p - moments[1],
         n*p*(1-p) - moments[2])  # origin Moment
  J <- matrix(c(p, n,
                p*(1-p), n*(1-2*p)),
              nrow = 2, byrow = T)  # derivative
  list(f = f, J = J)
}
Newtons <- function(f, x0, epsn, epsp, max_iter = 10){
  x <- x0
  for (i in 1:max_iter){
    result <- f(x)
    fx <- result$f
    if (abs(fx[1]) < epsn && abs(fx[2]) < epsp){
      return(list(root = x, iter = i)) 
    }
    x <- x - solve(result$J)%*%fx
  }
  return(list(root = x, iter = i))
}

moments <- c(mean(samples), var(samples));
funs_moment <- function(x) funs(x[1], x[2], moments);
Newtons(funs_moment, c(10, 0.2), 1e-3, 1e-5)


# T4
sum <- 0;
for(i in 10000:99999){
  flag <- FALSE;
  n <- i;
  for(j in 1:5){
    if(n%%10 == 6){
      flag <- TRUE;
      break
    }
    n <- n%/%10;
  }
  if(flag == TRUE){
    if(i%%5 != 0) sum <- sum + 1;
  }
}
sum


# Simulated card fetching
fetch <- function(){
  a <- sample(1:52, 1)
  b <- sample(1:52, 1)
  if((a-1)%/%13 == (b-1)%/%13) return(TRUE)
  else return(FALSE)
}
N <- 10000;
sum <- 0;
for(i in 1:N){
  if(fetch() == TRUE) sum <- sum + 1;
}
p <- sum/N; p
