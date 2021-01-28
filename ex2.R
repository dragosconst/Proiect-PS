# Funcția f are suportul [li, ls]; f este densitate de probabilitate
# - deci dp(f, li, ls) returnează TRUE - dacă:
# (a) f(x) >= 0 pentru orice x din [li, ls],
# (b) integrala de la li la ls din f(x), după x, este 1.

# Din documentația pentru integrate: „f must accept a vector of inputs and
# produce a vector of function evaluations at those points”. Considerând că
# utilizatorul introduce funcția dorită în regim scalar -> scalar, aplicând
# Vectorize() se obține argumentul dorit pentru integrate.

dp <- function(f, li, ls) {
  
  tryCatch(
    int <- integrate(Vectorize(f), li, ls, abs.tol = 0)$value == 1,
    error = function(e) {
      int <- FALSE
      print("Integrala este divergentă.")
    }
  )
  
  if (li == -Inf)
    li <- -1000
  if (ls ==  Inf)
    ls <-  1000
  
  int && all(sapply(seq(li, ls, length.out = 1000), f) >= 0)
}

# Teste: dp(function(x) 3*x^2, 0, 1) == TRUE
#        f <- function(x) if (x < -1) 0 else
#                         if (x < 0)  1 + x else
#                         if (x < 1)  1 - x else
#                                     0
#        dp(f, -Inf, Inf) == TRUE
