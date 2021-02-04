library(methods)
require(methods)

# definire clasa
# -val: field care reprezinta ce pun sub integrala inainte de f(x)dx la medie

# pt v.a bidimensionala (X, Y) slotul densitate retine densitatea comuna a lui X si Y
# suport[[1]] este suportul densitatii lui X(sau suportul densitatii in cazul v.a unidimensionale)
# suport[[2]] este suportul lui Y
# In cazul in care v.a X este marginala, ref_va_bidimen va fi o referinta catre v.a bidimensionala.
#    Se foloseste pt determinarea densitatii comune la calculul unor probabilitati
setClass("contRV", representation (
    densitate="function",
    val="function",
    bidimen="logical",
    suport="list",
    ref_va_bidimen = "contRV_or_NULL"
))

# Am folosit acest union pentru a permite referintei catre v.a bidimen sa fie nula
setClassUnion("contRV_or_NULL", c("contRV", "NULL"))


# constructor
# Pentru v.a unidimensionala parametrul suport va fi lista intervalelor(nu lista de liste, ca mai jos!)
# Pentru v.a bidimensionala (X, Y) parametrul suport va fi o lista ce contine
    # 2 liste corespunzatoare suportului densitatii lui X, respectiv Y
contRV <- function(densitate, val = function(x) x, bidimen = FALSE, suport = list(c(-Inf, Inf)), ref_va_bidimen = NULL)
{
   if(length(suport) < 2)
        suport <- list(suport, list())
    if (bidimen & missing(val))
        val = function(x, y) x * y

    obj <- new("contRV", densitate = densitate, val = val, bidimen = bidimen,
               suport = suport, ref_va_bidimen = ref_va_bidimen)

    return (obj)
}

# construieste o v.a continua pornind de la densitatea marginala a lui X in v.a bidimen (X, Y)
marginalaX <- function(XY)
{
    return (contRV(densitate = integrala(XY, 2), suport = XY@suport[[1]], bidimen = FALSE, ref_va_bidimen = XY))
}

# construieste o v.a continua pornind de la densitatea marginala a lui Y in v.a bidimen (X, Y)
marginalaY <- function(XY)
{
    return (contRV(densitate = integrala(XY, 1), suport = XY@suport[[2]], bidimen = FALSE, ref_va_bidimen = XY))
}


if (!isGeneric("P"))
    setGeneric("P", function(object) standardGeneric("P"))
if (!isGeneric("E"))
    setGeneric("E", function(object) standardGeneric("E"))
if (!isGeneric("Var"))
    setGeneric("Var", function(object) standardGeneric("Var"))
if (!isGeneric("aplica"))
    setGeneric("aplica", function(object, f) standardGeneric("aplica"))
if (!isGeneric("%AND%"))
    setGeneric("%AND%", function(e1, e2) standardGeneric("%AND%"))
if (!isGeneric("%OR%"))
    setGeneric("%OR%", function(e1, e2) standardGeneric("%OR%"))


# pentru calcularea probabilitatilor la ex.7
setMethod("<", c("contRV", "numeric"), function (e1, e2) {
    comp(e1, e2, "<=") # P(X < x) = P(X <= x)
})
setMethod("<=", c("contRV", "numeric"), function (e1, e2) {
    comp(e1, e2, "<=")
})
setMethod(">", c("contRV", "numeric"), function (e1, e2) {
    comp(e1, e2, ">=") # P(X > x) = P(X >= x)
})
setMethod(">=", c("contRV", "numeric"), function (e1, e2) {
    comp(e1, e2, ">=")
})
setMethod("==", c("contRV", "numeric"), function (e1, e2) {
    comp(e1, e2, "==")
})
setMethod("%AND%", c("contRV", "contRV"), function (e1, e2) {
    op(e1, e2, "&")
})
setMethod("%OR%", c("contRV", "contRV"), function (e1, e2) {
    op(e1, e2, "|")
})
setMethod("|", c("contRV", "contRV"), function (e1, e2) {
    cond(e1, e2)
})
setMethod("P", "contRV",
          function (object) {
              return (integrala(object)) # integreaza pe suport
          })
# functie wrapper pentru rezultatul probabilitatii conditionate
setMethod("P", "numeric",
          function (object) {
              return (object)
          })



setMethod("E", "contRV",
          function(object){
              return(media(object))
          })
setMethod("Var", "contRV",
          function(object) return(dispersia(object)))

compunere <- function(f, g)
{
    function(...) f(g(...))
}

setMethod("aplica", "contRV",
          function(object, f){
              retval <- contRV(object@densitate, Vectorize(compunere(f, object@val)), object@bidimen, object@suport,
                               ref_va_bidimen = object@ref_va_bidimen)
          })

# by Florin
# func <- function(x)
# {
#     if (x < -1)
#         0
#     else if (x < 0)
#         1 + x
#     else if (x < 1)
#         1 - x
#     else
#         0
# }


# exemple
#X <- contRV(densitate = Vectorize(func), bidimen = FALSE, suport = list(c(-1, 1)))
#X

#test
# XY <- contRV(densitate = function (x, y) 6/7(x+y)^2,
#            bidimen = TRUE, suport = list(list(c(0, 1)), list(c(0, 1))))
