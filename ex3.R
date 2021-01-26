# definire clasa 
setClass("contRV", slots=list(densitate="function", bidimen="logical"))

# un fel de constructor
contRV <- function(densitate, bidimen)
{
    
    # aici de verificat daca functia data este densitate de probabilitate
    obj <- new("contRV", densitate = densitate, bidimen = bidimen)
    
    return (obj)
}

# declararea functiei de verificare v.a bidimensionala
setGeneric("verifbidimen", function(object) standardGeneric("verifbidimen"))

# definirea functiei pentru clasa "contRV"
setMethod("verifbidimen", "contRV", 
          function (object) {
              if (object@bidimen == TRUE)
                  print("Este bidimensionala!")
              else
                  print ("Nu este bidimensionala!")
          })

# supraincarcare functie de afisare
setMethod("show", "contRV",
          function (object) {
              cat("Denistatea de probabilitate: ")
              print(body(fun = object@densitate))
              cat("Este v.a bidimensionala: ", object@bidimen, "\n")
          })

# exemple
X <- contRV(densitate = function(x, y) 2*x + 3*y, bidimen = TRUE)
verifbidimen(X)
X
