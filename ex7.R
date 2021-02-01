# Determina suportul pentru expresii de tipul X <= x, X >= x
comp <- function(X, x, c)
{
   suportNou <- list()
   nr <- 1
  
   # Presupunem ca intervalele sunt in ordine crescatoare dupa capatul inferior
   # si nu se intersecteaza!
   
   if (c == "<=")
   {
      # Exemplu: Daca suportul densitatii este format din [0, 2] U [4, 7] U [9, 11]
      # Noul suport pentru X <= 5 va fi [0, 2] U [4, 5]
     
      for (i in X@suport)
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
      for (i in rev(X@suport))
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
   return (contRV(densitate = X@densitate, val = X@val, bidimen = X@bidimen, suport = suportNou))
}


prob <- function(X)
{
  return(integrala(X))
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
    
    for (i in X@suport)
    {
      for (j in Y@suport)
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
  }
  else # reuniune
  {
    i <- 1
    j <- 1
    while (i <= length(X@suport) && j <= length(Y@suport))
    {
       # Formeaza intervale disjuncte in urma reuniunii (se vor intersecta totusi cate doua 
       # in cel mult un punct, dar acest lucru se neglijeaza la calculul integralei).
       # Exemplu: [1, 5] U [3, 9] = [1, 3] U [3, 5] U [5, 9]
      
       A <- X@suport[[i]]
       B <- Y@suport[[j]]
       C <- interval_intersect(A, B)
       
       if (is.null(C)) # daca sunt disjuncte
       {
         # ordonam crescator dupa capatul inferior
         if (A[1] < B[1])
         {
           suportNou[[nr]] <- A
           suportNou[[nr+1]] <- B
         }
         else
         {
           suportNou[[nr]] <- B
           suportNou[[nr+1]] <- A
         }
         
         nr <- nr + 2
       }
       else
       {
         if (A[1] < B[1])
         {
           suportNou[[nr]] <- c(A[1], B[1])
           
           if (A[2] < B[2])
           {
             suportNou[[nr+1]] <- c(B[1], A[2])
             suportNou[[nr+2]] <- c(A[2], B[2])
           }
           else
           {
             suportNou[[nr+1]] <- c(B[1], B[2])
             suportNou[[nr+2]] <- c(B[2], A[2])
           }
         }
         else
         {
           suportNou[[nr]] <- c(B[1], A[1])
           
           if (A[2] < B[2])
           {
             suportNou[[nr+1]] <- c(A[1], A[2])
             suportNou[[nr+2]] <- c(A[2], B[2])
           }
           else
           {
             suportNou[[nr+1]] <- c(A[1], B[2])
             suportNou[[nr+2]] <- c(B[2], A[2])
           }
         }
         nr <- nr + 3
       }
       
       i <- i + 1
       j <- j + 1
    }
    
    while (i <= length(X@suport))
    {
       suportNou[[nr]] <- X@suport[[i]]
       nr <- nr + 1
       i <- i + 1
    }
    
    while (j <= length(Y@suport))
    {
      suportNou[[nr]] <- Y@suport[[j]]
      nr <- nr + 1
      j <- j + 1
    }
  }
  
  return (contRV(densitate = X@densitate, val = X@val, bidimen = X@bidimen, suport = suportNou))
}


# Calculeaza probabilitatea conditionata
# X si Y pot fi expresii de tipul Z <= x, Z %AND% W etc.
cond <- function (X, Y)
{
  return (integrala(X %AND% Y) / integrala(Y)) # P(X ∩ Y) / P(Y)
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
# P((Z <= 0.5) %OR% (Z >= -0.7)) == 1
# P(Z >= 0.1 | Z <= 0.8) == 0.3928572