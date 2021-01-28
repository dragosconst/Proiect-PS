#am pus 4 spatii ca sa apara indentat ca un subtitlu
rep_uniforma <- c("    Definitie:","O v.a. U este repartizata uniform pe intervalul (a,b) daca admite densitatea de repartitie f(x)=1/(b-1), x ??? (a,b).",
                  "    Utilizare:","Se foloseste atunci cand numarul cazurilor posibile tinde la infinit",
                  "    Notatie:","U ~ U((a,b)), unde (a,b) este intervalul pe care este repartizata variabila aleatoare",
                  "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx",
                  "    Dispersia:","E[U^2] = integrala de la -Inf la Inf din (x^2)*f(x) dx",
                  "    Variatia:","Var(U) = E[U^2]-E[U]^2",
                  "    Sursa:","http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")

rep_exponentiala <- c("    Definitie:","O v.a. X este repartizata exponential de parametru lambda > 0 daca densitatea de repartitie a lui X este de forma f(x) = lambda * e^(-lambda*x), x > 0",
                      "    Utilizare:","Modeleaza timpul de asteptare pana la aparitia unui eveniment de interes.",
                      "    Notatie:","X ~ Exp(lambda)",
                      "    Media:","E[X] = integrala de la -Inf la Inf din x*f(x) dx = 1/lambda",
                      "    Dispersia:","E[X^2] = integrala de la -Inf la Inf din (x^2)*f(x) dx = 2/(lambda^2)",
                      "    Variatia:","Var(X) = E[X^2] - (E[x])^2 = 1/(lambda^2)",
                      "    Sursa:","Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")


rep_normala <- c("    Definitie:","Spunem ca o v.a. X este repartizata normal (sau Gaussian) de parametru miu si sigma^2 daca admite densitatea f(x) = (1/(sqrt(2pi)*sigma))*e^(-(x-miu)^2/2*sigma^2), unde x ??? R)",
                 "    Notatie:","X ~ N(miu,sigma^2)","In cazul in care miu = 0 si sigma = 1 spunem ca v.a. X este repartizata normal standard si notam X ~ N(0,1) si putem calcula:",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = 0",
                 "    Dispersia:","E[U^2] = integrala de la -Inf la Inf din (x^2)*f(x) dx = 1",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = 1 - 0 = 1",
                 "    Sursa:","Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")


#rep_gamma
#rep_beta
#rep_X^2
#rep_student
#o sa completez pe masura ce ajung la ele la cursuri

Fisa_sinteza <- function(Rep){
  for (i in Rep)
  {
    print(i)
  }
  
}
