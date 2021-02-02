# Covarianța pentru variabila bidimensională Z, compusă din X și Y
Cov <- function(Z) {
  
  if (!Z@bidimen) {
    print("Variabila nu este bidimensională!")
    NA
  }
  else {
    
    X <- contRV(densitate = Z@densitateX, bidimen = FALSE, suport = Z@suport[[1]])
    Y <- contRV(densitate = Z@densitateY, bidimen = FALSE, suport = Z@suport[[2]])
    
    return (E(Z) - E(X) * E(Y))

  }
}

Cor <- function(Z)
{
  if (!Z@bidimen) {
    print("Variabila nu este bidimensională!")
    NA
  }
  else {
    
    X <- contRV(densitate = Z@densitateX, bidimen = FALSE, suport = Z@suport[[1]])
    Y <- contRV(densitate = Z@densitateY, bidimen = FALSE, suport = Z@suport[[2]])
    
    return (Cov(Z) / sqrt(Var(X) * Var(Y)))
    
  }
}


# Pentru teste:
#Z <- contRV(densitate = function (x, y) (6/7) * (x+y)^2,
#            bidimen = TRUE,
#            suport = list(list(c(0, 1)), list(c(0, 1))),
#            densitateX = function(x) 6/7 * (x^2 + x + 1/3),
#            densitateY = function(y) 6/7 * (y^2 + y + 1/3))

#Cov(Z)
#Cor(Z)