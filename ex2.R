# Suportul funcției f este reuniunea intervalelor specificate prin parametrul
# sup - o listă de vectori de câte două elemente, reprezentând extremități de
# interval. f este densitate de probabilitate - deci dp(f, sup)==TRUE - dacă:
# a) f(x) >= 0 pentru orice x din suport,
# b) suma integralelor de f peste fiecare interval din sup este 1.

# Din documentația pentru integrate: „f must accept a vector of inputs and
# produce a vector of function evaluations at those points”. Considerând că
# utilizatorul introduce funcția dorită în regim scalar -> scalar, aplicând
# Vectorize() se obține argumentul dorit pentru integrate.

dp <- function(f, sup) {
  
  sum <- 0
  for (i in sup) {
    
    if (any(sapply(seq(i[1], if (i[2] != Inf) i[2] else i[1] + 1000, length.out = 1000), f) < 0)) {
      print(sprintf("Valoare negativă în intervalul [%.2f, %.2f]!", i[1], i[2]))
      return(FALSE)
    }
    
    tryCatch(
      sum <- sum + integrate(Vectorize(f), i[1], i[2], abs.tol = 0)$value == 1,
      error = function(e) {
        print(sprintf("Integrală divergentă în intervalul [%.2f, %.2f]!", i[1], i[2]))
        return(FALSE)
      }
    )
    
  }
  
  sum == 1
}


# Teste:

# dp(function(x) 3*x^2, list(c(0, 1))) == TRUE

# f <- function(x) if (x < 0) 1 + x else 1 - x
# dp(f, list(c(-1, 1))) == TRUE

# dp(function(x) x, list(c(1, 3), c(-1, 0))) == FALSE

# dp(function(x) x, list(c(0, Inf))) == FALSE
