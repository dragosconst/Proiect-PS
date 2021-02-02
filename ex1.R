Nor_constant <- function(Func) {
  if(any(sapply(seq(-100, 100, length.out = 1000), Func) < 0)){
    stop("Functie negativa")#daca functia are valori negative nu pot calcula constanta de normalizare
  }
  else {
    tryCatch(integ <- integrate(Vectorize(Func), lower = -Inf, upper = Inf)$value,
             error= function(err)
             {
               stop("Integrala e divergenta")#daca integrala nu poate fi calculata returnez un mesaj de eroare
             })
    if(integ == 0)
      stop("Nu exista constanta de normalizare pentru functia data") # daca integrala = 0 inseamna ca nu exista constanta de normalizare
    const <- 1/integ
    return(const)
  }
  
}


# Nor_constant(function(x) {2*x})
# 0 with absolute error < 0

# Nor_constant(function(x) {1/sqrt(2*pi)*exp(1)^((-x^2)/2)})
# [1] 1"


# Nor_constant(function(x) {exp(1)^((-x^2)/2)})
# [1] 0.3989423