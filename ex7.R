# Determina suportul pentru expresii de tipul X <= x, X >= x
comp <- function(X, x, c)
{
   suportNou <- list()
   nr <- 1
  
   # Presupunem ca intervalele sunt in ordine crescatoare dupa capatul superior
   # si nu se intersecteaza!
   
   if (c == "<=")
   {
      # Exemplu: Daca suportul densitatii este format din [0, 2] U [4, 7] U [9, 11]
      # Noul suport pentru X <= 5 va fi [0, 2] U [4, 5]
     
      for (i in X@domeniu)
      {
          a <- i[1]
          b <- i[2]
          
          if (x < a)  # daca x este mai mic decat capatul inferior al intervalului
              break # am terminat de construit suportul
          
          if (a <= x && b >= x) # daca x se afla in intervalul [a, b]
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
      
      # parcurgem intervalele in ordine descrescatoare dupa capatul superior
      for (i in rev(X@domeniu))
      {
        a <- i[1]
        b <- i[2]
        
        if (x > b)
          break
       
        if (a <= x && b >= x) # daca x se afla in intervalul [a, b]
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
   return (contRV(densitate = X@densitate, val = X@val, bidimen = X@bidimen, domeniu = suportNou))
}


# Teste
# Y <- contRV(densitate = function(x) x, bidimen = FALSE, domeniu = list(c(0, 2), c(4, 7), c(9, 11)))
# Y > 5