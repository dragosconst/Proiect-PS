# Funcția f este integrabilă pe [li, ls]; f este densitate de probabilitate
# - deci dp(f, li, ls) returnează TRUE - dacă:
# (a) f(x) >= 0 pentru orice x din [li, ls],
# (b) integrala de la li la ls din f(x), după x, este 1.

dp <- function(f, li, ls) {
  
  int <- integrate(Vectorize(f), li, ls, abs.tol = 0)$value == 1
  
  if (li == -Inf)
    li <- -1000
  if (ls == Inf)
    ls <-  1000
  
  all(sapply(seq(li, ls, length.out = 1000), f) >= 0) && int
}

# Teste: dp(function(x) 3*x^2, 0, 1) == TRUE
#        f <- function(x) if (x < -1) 0 else
#                         if (x < 0)  1 + x else
#                         if (x < 1)  1 - x else
#                                     0
#        dp(f, -Inf, Inf) == TRUE
# Inspirație: math.stackexchange.com/questions/206050/
#             how-do-i-tell-if-this-function-is-a-probability-density-function
# Eroare la integrare: stackoverflow.com/questions/43818574/
#      error-in-integrate-evaluation-of-function-gave-a-result-of-wrong-length