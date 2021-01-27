# definire clasa 
setClass("contRV", slots=list(densitate="function", bidimen="logical", domeniu="numeric"))

# un fel de constructor
contRV <- function(densitate, bidimen, domeniu = c(-Inf, Inf))
{
    
    # aici de verificat daca functia data este densitate de probabilitate
    obj <- new("contRV", densitate = densitate, bidimen = bidimen, domeniu = domeniu)
    
    return (obj)
}


# asta e un inceput pentru ex. 7
if (!isGeneric("P"))
    setGeneric("P", function(object, x) standardGeneric("P"))
    
setMethod("P", "contRV", 
          function (object, x) {
              integrate(f = object@densitate, lower = object@domeniu[1], upper = x)
          })

# supraincarcare functie de afisare
setMethod("show", "contRV",
          function (object) {
              cat("Densitatea de probabilitate: ")
              print(body(fun = object@densitate))
              cat("Este v.a bidimensionala: ", object@bidimen, "\n")
          })

# by Florin
func <- function(x)
{
    if (x < -1)
        0
    else if (x < 0)
        1 + x
    else if (x < 1)
        1 - x
    else
        0
}

# exemple
X <- contRV(densitate = Vectorize(func), bidimen = FALSE)
X
P(X, 5)
#integrate(Vectorize(func), -4, Inf, rel.tol = .Machine$double.eps^0.5)

