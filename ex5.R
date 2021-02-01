
# Un mic hack cu functia asta e ca daca vreti sa calculati integrala unei functii care nu e explicit densitate,
# puteti face un obiect contRv cu densitatea functia pe care vreti sa o integrati si val = f(...) = 1 (functia care duce tot in 1).
integrala <- function(X)
{
  if(X@bidimen)
  {
    retval <- 0
    #...
  }
  else
  {
    sum <- 0
    for (i in X@suport[[1]]) {
      tryCatch(sum <- sum + integrate(Vectorize(X@densitate), i[1], i[2], abs.tol = 1.0e-13)$value,
               error= function(err)
               {
                 stop("Integrala a esuat.")  
               })
    }
    # retval <- sum # return
    return (sum)
  }
}


# -hackish function
# -asa pot ridica o functie la puterea y si ca retval sa am tot o functie
# -f useful pt momente
# O mica problema cu approach-ul asta e ca se pare ca R trimite ca argument
# functiile ca referinte, nu cu shallow copy.
# Implicatia e ca daca schimbi f in afara lui powF, o sa fie schimbat si cand e
# aplicat de powF.
# Daca faci ceva de genu myFunc <- powF(myFunc,2), obtii o recursie infinita.
# Solutia este sa faci raisedFunc <- powF(myFunc, 2)
powF <- function(f, y)
{
  ret <- function(...)
  {
    f(...) ^ y
  }
}

# returneaza valoarea mediei sau o eroare
media <- function(X)
{
  
  # -valoarea de sub integrala trebuie trimisa
  # ca o functie, cel mai lejer de scris e asa
  subIntegrala <- function(...)
  {
    X@val(...) * X@densitate(...)
  }
  
  # more evil R magic
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport[[1]],
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error=function(err){
             stop("Media nu exista")
           }
  )
}

dispersia <- function(X)
{
  # -deoarece momentan lucrez doar cu functii de densitate, voi folosi
  # formula clasica cu integrala
  # -cand e gata clasa de variabile, putem incerca si formula aia mai rapida
  
  tryCatch(m <- media(X), warning=function(wr)
  {
    stop("Calcularea dispersiei a esuat, media nu exista")
  }) 
  
  xCoef <- function(...)
  {
    
    X@val(...) - m
  }
  
  coefRaised <- powF(xCoef, 2)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = function(...) 1, bidimen = X@bidimen, suport = X@suport[[1]],
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop("Dispersia nu exista.")
           })
}


# -referitor la momente, nu sunt sigur daca o strategie mai buna
# ar fi sa scriem o singura functie moment, in care sa se indice cu un
# parametru daca e centrat sau initial, ca sa mai economisim cod
# -dezavantajul e evident ca functia generala de moment ar arata mai urat
moment_centrat <- function(X, ordin)
{
  # cazurile triviale
  if(ordin == 0)
    return(1)
  else if(ordin == 1)
    tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
      media(X)
      retval <- 0
    }, error= function(err)
    {
      stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat, nu exista media."))
    })
  else if(ordin == 2)
    tryCatch({  # X dispersia nu exista, vrem un mesaj specific pt momente
      dispersia(X)
    }, error= function(err)
    {
      stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat, nu exista dispersie."))
    })
  
  tryCatch(m <- media(X), warning=function(wr)
  {
    stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat"))
  }) 
  
  xCoef <- function(...)
  {
    
    X@val(...) - m
  }
  
  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = function(...) 1, bidimen = X@bidimen, suport = X@suport[[1]],
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop(paste("Momentul centrat de ordin ", ordin, " nu exista."))
           })
}

moment_initial <- function(X, ordin)
{
  # cazurile triviale
  if(ordin == 0)
    return(1)
  else if(ordin == 1)
    tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
      media(X)
    }, error= function(err)
    {
      stop(paste("Calcularea momentului initial de ordin ", ordin, " a esuat."))
    })
  
  
  xCoef <- function(...)
  {
    
    X@val(...)
  }
  
  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = function(...) 1, bidimen = X@bidimen, suport = X@suport[[1]],
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop(paste("Momentul initial de ordin ", ordin, " nu exista."))
           })
}