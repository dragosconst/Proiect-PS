# definire clasa 
# -val: field care reprezinta ce pun sub integrala inainte de f(x)dx la medie
setClass("contRV", slots=list(densitate="function", val="function", bidimen="logical", domeniu="numeric"))

# un fel de constructor
contRV <- function(densitate, val = function(x) x, bidimen = FALSE, domeniu = c(-Inf, Inf))
{
    
    # aici de verificat daca functia data este densitate de probabilitate
    obj <- new("contRV", densitate = densitate, val = val, bidimen = bidimen, domeniu = domeniu)
    
    return (obj)
}


# asta e un inceput pentru ex. 7
# ar trebui sa calculeze P(X <= x)
if (!isGeneric("P"))
    setGeneric("P", function(object, x) standardGeneric("P"))
if (!isGeneric("E"))
    setGeneric("E", function(object) standardGeneric("E"))
if (!isGeneric("Var"))
    setGeneric("Var", function(object) standardGeneric("Var"))
if (!isGeneric("aplica"))
    setGeneric("aplica", function(object, f) standardGeneric("aplica"))
    
setMethod("P", "contRV", 
          function (object, x) {
              integrate(f = object@densitate, lower = object@domeniu[1], upper = x)
          })
setMethod("E", "contRV",
           function(object){
              media(object)
           })
setMethod("Var", "contRV",
          function(object) dispersia(object))

compunere <- function(f, g)
{
    function(...) f(g(...))
}

# returnez o noua va continua deoarece ar fi greu de lucrat cu pachetul asta daca as modifica direct X
# aici nu sunt 100% sigur daca f trebuie vectorizata dinainte
setMethod("aplica", "contRV",
          function(object, f){
              retval <- contRV(object@densitate, Vectorize(compunere(f, object@val)), object@bidimen, object@domeniu)
          })

# supraincarcare functie de afisare
setMethod("show", "contRV",
          function (object) {
              cat("Densitatea de probabilitate: ")
              print(body(fun = object@densitate))
              cat("Este v.a bidimensionala: ", object@bidimen, "\n")
              cat("Sub integrala o sa avem: ")
              print(body(fun = object@val))
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

