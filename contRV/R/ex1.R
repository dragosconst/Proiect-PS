Nor_constant <- function(Func, sup) {

    sum <- 0

    for (i in sup) {
      tryCatch(
        sum <- sum + integrate(Vectorize(Func), i[1], i[2], abs.tol = 0)$value,
        error= function(err) {
          stop("Integrala e divergenta sau functia nu e integrabila") # daca integrala nu poate fi calculata returnez un mesaj de eroare
        }
      )

      if (i[1] == -Inf && i[2] == Inf) {
        i[1] <- -1000
        i[2] <-  1000
      }
      else if (i[1] == -Inf)
        i[1] <- i[2] - 1000
      else if (i[2] ==  Inf)
        i[2] <- i[1] + 1000

      if(any(sapply(seq(i[1], i[2], length.out = 1000), Func) < 0))
        stop("Functie negativa") # daca functia are valori negative nu pot calcula constanta de normalizare

    }

    if(sum == 0)
      stop("Nu exista constanta de normalizare pentru functia data") # daca integrala = 0 inseamna ca nu exista constanta de normalizare

    const <- 1 / sum
    return (const)

}


# Nor_constant(function(x) {2*x})
# 0 with absolute error < 0

# Nor_constant(function(x) {1/sqrt(2*pi)*exp(1)^((-x^2)/2)})
# [1] 1"


# Nor_constant(function(x) {exp(1)^((-x^2)/2)})
# [1] 0.3989423
