Nor_constant <- function(Func) {
  if(any(sapply(seq(-100, 100, length.out = 1000), Func) < 0)){
    stop("Functie negativa")
  }
  else {
    tryCatch(const <- integrate(Vectorize(Func), lower = -Inf, upper = Inf)$value,
             error= function(err)
             {
               stop("Integrala e divergenta")
             })
    if(const == 0)
      stop("Nu exista constanta de normalizare pentru functia data")
    const <- 1/const
    return(const)
  }
  
}


# Nor_constant(function(x) {2*x})
# 0 with absolute error < 0

# Nor_constant(function(x) {1/sqrt(2*pi)*exp(1)^((-x^2)/2)})
# [1] "Functie negativa"