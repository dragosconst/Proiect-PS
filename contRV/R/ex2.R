# Suportul functiei f este reuniunea intervalelor specificate prin parametrul
# sup - o lista de vectori de cate doua elemente, reprezentand extremitati de
# interval. f este densitate de probabilitate - deci dp(f, sup)==TRUE - daca:
# a) f(x) >= 0 pentru orice x din suport,
# b) suma integralelor de f peste fiecare interval din sup este 1.

# Din documentatia pentru integrate: „f must accept a vector of inputs and
# produce a vector of function evaluations at those points”. Considerand ca
# utilizatorul introduce functia dorita in regim scalar -> scalar, aplicand
# Vectorize() se obtine argumentul dorit pentru integrate.

dp <- function(f, sup) {

  sum <- 0
  for (i in sup) {

    tryCatch(
      sum <- sum + integrate(Vectorize(f), i[1], i[2], abs.tol = 0)$value == 1,
      error = function(e) {
        print(sprintf("Integrala divergenta in intervalul [%.2f, %.2f]!", i[1], i[2]))
        return(FALSE)
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

    if (any(sapply(seq(i[1], i[2], length.out = 1000), f) < 0)) {
      print(sprintf("Valoare negativa in intervalul [%.2f, %.2f]!", i[1], i[2]))
      return(FALSE)
    }
  }

  sum == 1
}


# Teste:

# dp(function(x) 3*x^2, list(c(0, 1))) == TRUE

# f <- function(x) if (x < 0) 1 + x else 1 - x
# dp(f, list(c(-1, 1))) == TRUE

# dp(function(x) x, list(c(1, 3), c(-1, 0))) == FALSE

# dp(function(x) x, list(c(0, Inf))) == FALSE
