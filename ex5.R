 
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
  ret <- function(x)
  {
    f(x) ^ y
  }
}

# returneaza valoarea mediei sau o eroare
media <- function(fdens)
{
  # evil R magic
  fdens <- Vectorize(fdens) # needed ? putem presupune ca functiile trimise ca parametrii sunt deja vectorizate
  
  # -valoarea de sub integrala trebuie trimisa
  # ca o functie, cel mai lejer de scris e asa
  subIntegrala <- function(x)
  {
    x * fdens(x)
  }
  
  # more evil R magic
  subIntegrala <- Vectorize(subIntegrala)
  
  tryCatch(
    integrate(subIntegrala, -Inf, Inf)$value,
    error=function(err){
      stop("Media este divergenta")
    }
  )
}

dispersia <- function(fdens)
{
  # -deoarece momentan lucrez doar cu functii de densitate, voi folosi
  # formula clasica cu integrala
  # -cand e gata clasa de variabile, putem incerca si formula aia mai rapida
  
  tryCatch(m <- media(fdens), warning=function(wr)
    {
      stop("Calcularea dispersiei a esuat")
    }) 
  
  xCoef <- function(x)
  {
    x - m
  }
    
  coefRaised <- powF(xCoef, 2)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(x)
  {
    coefRaised(x) * fdens(x)
  }
  subIntegrala <- Vectorize(subIntegrala)
  
  tryCatch(retval <- integrate(subIntegrala, -Inf, Inf)$value,
           error= function(err)
           {
             stop("Dispersia este divergenta")
           })
}


# -referitor la momente, nu sunt sigur daca o strategie mai buna
# ar fi sa scriem o singura functie moment, in care sa se indice cu un
# parametru daca e centrat sau initial, ca sa mai economisim cod
# -dezavantajul e evident ca functia generala de moment ar arata mai urat
moment_centrat <- function(fdens, ordin)
{
  # cazurile triviale
  if(ordin == 0)
      return(1)
  else if(ordin == 1)
      tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
        media(fdens)
        retval <- 0
        }, error= function(err)
          {
          stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat"))
        })
  else if(ordin == 2)
      tryCatch({  # daca dispersia nu exista, vrem un mesaj specific pt momente
        dispersia(fdens)
      }, error= function(err)
        {
        stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat"))
      })
  
  tryCatch(m <- media(fdens), warning=function(wr)
  {
    stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat"))
  }) 
  
  xCoef <- function(x)
  {
    x - m
  }
  
  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(x)
  {
    coefRaised(x) * fdens(x)
  }
  subIntegrala <- Vectorize(subIntegrala)
  
  tryCatch(retval <- integrate(subIntegrala, -Inf, Inf)$value,
           error= function(err)
           {
             stop(paste("Momentul centrat de ordin ", ordin, " nu exista"))
           })
}

moment_initial <- function(fdens, ordin)
{
  # cazurile triviale
  if(ordin == 0)
    return(1)
  else if(ordin == 1)
    tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
      media(fdens)
    }, error= function(err)
    {
      stop(paste("Calcularea momentului initial de ordin ", ordin, " a esuat"))
    })
  
  tryCatch(m <- media(fdens), warning=function(wr)
  {
    stop(paste("Calcularea momentului initial de ordin ", ordin, " a esuat"))
  }) 
  
  xCoef <- function(x)
  {
    x
  }
  
  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)
  
  subIntegrala <- function(x)
  {
    coefRaised(x) * fdens(x)
  }
  subIntegrala <- Vectorize(subIntegrala)
  
  tryCatch(retval <- integrate(subIntegrala, -Inf, Inf)$value,
           error= function(err)
           {
             stop(paste("Momentul initial de ordin ", ordin, " nu exista"))
           })
}