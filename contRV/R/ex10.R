# Covarianța pentru variabila bidimensională Z, compusă din X și Y
Cov <- function(Z) {

  if (!Z@bidimen) {
    print("Variabila nu este bidimensională!")
    NA
  }
  else {

    X <- marginalaX(Z)
    Y <- marginalaY(Z)

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

    X <- marginalaX(Z)
    Y <- marginalaY(Z)

    return (Cov(Z) / sqrt(Var(X) * Var(Y)))

  }
}


# Pentru teste:
# Z <- contRV(densitate = function (x, y) (6/7) * (x+y)^2,
#             bidimen = TRUE,
#             suport = list(list(c(0, 1)), list(c(0, 1))))

#Cov(Z)
#Cor(Z)
