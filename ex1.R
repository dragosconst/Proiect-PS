Nor_constant <- function(Func) {
  if(Func(5) < Func(4)){
    print("Functie negativa")
  }
  else {
    const <- integrate(Func, lower = -Inf, upper = Inf)
    return(const)
  }
  
}


# Nor_constant(function(x) {2*x})
# 0 with absolute error < 0

# Nor_constant(function(x) {1/sqrt(2*pi)*exp(1)^((-x^2)/2)})
# [1] "Functie negativa"