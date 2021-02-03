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

setClassUnion("contRV_or_NULL", c("contRV", "NULL"))


# constructor
# Pentru v.a unidimensionala parametrul suport va fi lista intervalelor(nu lista de liste, ca mai jos!)
# Pentru v.a bidimensionala (X, Y) parametrul suport va fi o lista ce contine
    # 2 liste corespunzatoare suportului densitatii lui X, respectiv Y
contRV <- function(densitate, val = function(x) x, bidimen = FALSE, suport = list(c(-Inf, Inf)), ref_va_bidimen = NULL)
{

    # aici de verificat daca functia data este densitate de probabilitate

    # Metoda precedenta era foarte buna, singura problema e ca daca faceam artificii de cod, de ex facem
    # o v.a.c. ca sa calculam o integrala, daca nu era bidimensionala, ii tot imbrica lista de suport si in final dadea eroare la integrala.
    if(length(suport) < 2)
        suport <- list(suport, list())
    if (bidimen && missing(val))
        val = function(x, y) x * y

    obj <- new("contRV", densitate = densitate, val = val, bidimen = bidimen,
               suport = suport, ref_va_bidimen = ref_va_bidimen)

    return (obj)
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
              return (prob(object))
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

# returnez o noua va continua deoarece ar fi greu de lucrat cu pachetul asta daca as modifica direct X
# aici nu sunt 100% sigur daca f trebuie vectorizata dinainte
setMethod("aplica", "contRV",
          function(object, f){
              retval <- contRV(object@densitate, Vectorize(compunere(f, object@val)), object@bidimen, object@suport[[1]],
                               ref_va_bidimen = object@ref_va_bidimen)
          })

# supraincarcare functie de afisare
setMethod("show", "contRV",
          function (object) {
              cat("Densitatea de probabilitate: ")
              print(body(fun = object@densitate))

              cat("Este v.a bidimensionala: ", object@bidimen, "\n")

              cat("Suportul densitatii: ") # de fisat si pt v.a bidimensionale
              for (i in object@suport[[1]]) {
                  cat("[", i[1], ",", i[2], "] ") # de afisat si un simbol de reuniune
              }
              cat("\n")

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
#X <- contRV(densitate = Vectorize(func), bidimen = FALSE, suport = list(c(-1, 1)))
#X

#test
#A <- contRV(densitate = function (x, y) 6/7(x+y)^2,
#            bidimen = TRUE, suport = list(list(c(0, 1)), list(c(0, 1))))

#integrala(A, dt = 1)