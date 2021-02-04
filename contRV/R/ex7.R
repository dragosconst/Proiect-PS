# Determina suportul pentru expresii de tipul X <= x, X >= x
comp <- function(X, x, c)
{

   if (X@bidimen)
     stop("Nu se poate compara o v.a bidimensionala cu un numar real!")

   suportNou <- list()
   nr <- 1

   # Presupunem ca intervalele sunt in ordine crescatoare dupa capatul inferior
   # si nu se intersecteaza!

   if (c == "==")
   {
      for (i in X@suport[[1]])
      {
        a <- i[1]
        b <- i[2]

        if (x < a)
          break # nu mai are rost sa cautam

        if (a <= x & b >= x) # daca x se afla in intervalul [a, b]
        {
          suportNou[[nr]] <- c(x, x) # suportul va fi intervalul [x, x]
          break
        }
      }
   }
   else if (c == "<=")
   {
      # Exemplu: Daca suportul densitatii este format din [0, 2] U [4, 7] U [9, 11]
      # Noul suport pentru X <= 5 va fi [0, 2] U [4, 5]

      for (i in X@suport[[1]])
      {
          a <- i[1]
          b <- i[2]

          if (x < a)  # daca x este mai mic decat capatul inferior al intervalului
              break # am terminat de construit suportul

          if (a <= x & b >= x) # daca x se afla in intervalul [a, b]
          {
             suportNou[[nr]] <- c(a, x) # adaugam ultimul interval din noul suport, adica [a, x]
             break
          }

          # altfel, n-am ajuns la un interval care sa-l contina pe x, deci il adaugam in suportul nou
          suportNou[[nr]] <- c(a, b)
          nr <- nr + 1
      }
   }
   else # ">="
   {
     # Exemplu: Daca suportul densitatii este format din [0, 2] U [4, 7] U [9, 11]
     # Noul suport pentru X >= 5 va fi [5, 7] U [9, 11]

      # parcurgem intervalele in ordine descrescatoare dupa capatul inferior
      for (i in rev(X@suport[[1]]))
      {
        a <- i[1]
        b <- i[2]

        if (x > b)
          break

        if (a <= x & b >= x) # daca x se afla in intervalul [a, b]
        {
          suportNou[[nr]] <- c(x, b) # adaugam ultimul interval din noul suport, adica [x, b]
          break
        }

        # altfel, inca n-am ajuns la un interval care sa-l contina pe x, deci il adaugam in suportul nou
        suportNou[[nr]] <- c(a, b)
        nr <- nr + 1
      }

      # inversam ordinea din noul suport, intrucat am parcurs intervalele din suport in ordine inversa
      suportNou <- rev(suportNou)
   }

   # atentie! rezultatul obtinut nu mai este o v.a! se foloseste doar pt a calcula probabilitati!
   return (contRV(densitate = X@densitate, val = X@val, bidimen = X@bidimen, suport = suportNou,
                  ref_va_bidimen = X@ref_va_bidimen))
}

interval_intersect <- function (A, B)
{
  # intersectia intervalelor [a, b] si [c, d]

  a <- A[1]
  b <- A[2]
  c <- B[1]
  d <- B[2]


  if (b < c | d < a)
    return (NULL)

  if (c <= a & d >= b) # [a, b] se afla in interiorul lui [c, d]
    return (A)

  if (a <= c & b >= d) # [c, d] se afla in interiorul lui [a, b]
    return (B)

  if (a <= c & b >= c)
    return (c(c, b))

  if (c <= a & d >= a)
    return (c(a, d))

}


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

# Reuniune si intersectie de variabile aleatoare
# Functioneaza doar daca X si Y au aceeasi densitate
op <- function(X, Y, o)
{

  suportNou <- list()
  nr <- 1

  if (o == "&") # intersectie
  {

    if (!identical(X@densitate, Y@densitate))
    {

      XY <- NULL

      if (!is.null(X@ref_va_bidimen) & identical(X@ref_va_bidimen, Y@ref_va_bidimen))
      {
        # aici se face o copie
        XY <- X@ref_va_bidimen
      }
      else
      {
        # consideram ca sunt independente
        XY <- contRV(densitate = function(x, y) {X@densitate(x) * Y@densitate(y)}, bidimen = TRUE)
      }

      XY@suport[[1]] <- X@suport[[1]]
      XY@suport[[2]] <- Y@suport[[1]]

      return (XY)
    }

    for (i in X@suport[[1]])
    {
      for (j in Y@suport[[1]])
      {
         # reuniuni de intersectii ale intervalelor din suport

         A <- interval_intersect(i, j)
         if (!is.null(A))
         {
           suportNou[[nr]] <- A
           nr <- nr + 1
         }
      }
    }

    return (contRV(densitate = X@densitate, val = X@val, bidimen = X@bidimen, suport = suportNou,
                   ref_va_bidimen = X@ref_va_bidimen)) # intoarce contRV pt a integra suportul ramas
  }
  else # reuniune
  {
    return (P(X) + P(Y) - P(X %AND% Y)) # aici intoarce deja probabilitatea calculata
    # problema este ca nu se mai pot aplica alte operatii pe v.a
  }
}


# Calculeaza probabilitatea conditionata
# X si Y pot fi expresii de tipul Z <= x, Z %AND% W etc.
cond <- function (X, Y)
{
  if (!is.null(X@ref_va_bidimen) & identical(X@ref_va_bidimen, Y@ref_va_bidimen))
  {
     # aici probabil ar trebui verificat daca X si Y referintiaza aceeasi v.a bidimensionala
     XY <- X@ref_va_bidimen
     fx_cond_y <- dens_condit_x_de_y(XY)

     if (length(Y@suport[[1]]) == 0) # inseamna ca nu a fost gasit y in suportul lui Y
     {
       return (0)
     }

     if (length(Y@suport[[1]]) != 1  || Y@suport[[1]][[1]][[1]] != Y@suport[[1]][[1]][[2]])
     {
        stop("Nu pot calcula asa ceva! Suportul lui Y trebuie sa fie un singur punct!!")
     }


     yfixat <- Y@suport[[1]][[1]][[1]]

     sum <- 0
     for (i in X@suport[[1]]) {
       tryCatch(sum <- sum + integrate(fx_cond_y, i[1], i[2], y = yfixat, abs.tol = 1.0e-13)$value,
                error= function(err)
                {
                  stop("Integrala a esuat.")
                })
     }
     return (sum)

  }
  else
  {
    # P(X | Y) = P(X), X si Y independente
    # Daca X si Y sunt independente, atunci calculul de mai jos este egal cu P(X)
    return (integrala(X %AND% Y) / integrala(Y)) # P(X ∩ Y) / P(Y)
  }
}

# Teste
# Y nu este o v.a, se foloseste pt a testa operatiile de tip intersectie si reuniune de intervale
# Y <- contRV(densitate = function(x) x, bidimen = FALSE, suport = list(c(0, 2), c(4, 7), c(9, 11)))
# Y > 5  -- suportul va fi [5, 7] U [9, 11]
# ((Y <= 1) %OR% (Y >= 5)) -- suportul va fi [0, 1] U [5, 7] U [9, 11]
# ((Y <= 5) %AND% (Y >= 1.5)) -- suportul va fi [1.5, 2] U [4, 5]


# Z <- contRV(densitate = Vectorize(func), bidimen = FALSE, suport = list(c(-1, 1)))
# P(Z <= 0) == 0.5
# P((Z <= 0.5) %AND% (Z >= -0.7)) == 0.83
# P(((Z <= 0.5) %AND% (Z >= -0.7)) %OR% (Z <= 1)) == 1
# P(Z >= 0.1 | Z <= 0.8) == 0.3928572

# XY <- contRV(densitate = function (x, y) (6/7) * (x+y)^2,
#             bidimen = TRUE,
#             suport = list(list(c(0, 1)), list(c(0, 1))))
#
# X <- marginalaX(XY)
# Y <- marginalaY(XY)

# P((X <= 0.5) %AND% (X >= 0.2) | Y == 0.2) == 0.1622093
#

# P((X <= 0.7) %AND% (Y >= 0.5))
#
# func2 <- function(x)
# {
#   if (x < 2 & x > 0)
#     3/8 * (4 * x - 2 * x^2)
#   else
#     0
# }
#
# func3 <- function(x)
# {
#   if (x >= 0)
#   {
#     1/96 * (x^3 * exp(-x/2))
#   }
#   else
#     0
# }

# X <- contRV(densitate = Vectorize(func3), bidimen = FALSE, suport = list(c(0, Inf)))
# Y <- contRV(densitate = Vectorize(func2), bidimen = FALSE, suport = list(c(0, 2)))
# P((X <= 3) %AND% (Y >= 1))
