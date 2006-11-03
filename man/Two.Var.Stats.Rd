\name{Two.Var.Stats}

\alias{Two.Var.Stats}

\title{ Statistiche a 2 variabili }

\description{ \bold{Two.Var.Stats} calcola media, somma, somma dei quadrati, somma dei prodotti misti, deviazione standard, deviazione standard della popolazione, numero di osservazioni, minimo e massimo.
}

\usage{
Two.Var.Stats(XList,YList,Freq=1)
}

\arguments{
  \item{ XList }{ nome dell'elenco che contiene i dati per la variabile x }
  \item{ YList }{ nome dell'elenco che contiene i dati per la variabile y } 
  \item{ Freq }{ nome dell'elenco che contiene i valori di frequenza per i dati in \emph{XList} e \emph{YList}.   Valore predefinito uguale ad 1. Tutti gli elementi devono essere interi \eqn{\ge} 0 }
}

\value{
  \item{ xmean }{ media di x }
  \item{ sumx }{ somma di x }
  \item{ sumx2 }{ somma dei quadrati di x }
  \item{ Sx }{ deviazione standard di x }
  \item{ sigmax }{ deviazione standard della popolazione x }
  \item{ n }{ dimensione campionaria }
  \item{ ymean }{ media di y }
  \item{ sumy }{ somma di y }
  \item{ sumy2 }{ somma dei quadrati di y }
  \item{ Sy }{ deviazione standard di y }
  \item{ sigmay }{ deviazione standard della popolazione y }
  \item{ sumxy }{ somma dei prodotti misti }
  \item{ minX }{ minimo di x }
  \item{ maxX }{ massimo di x }
  \item{ minY }{ minimo di y }
  \item{ maxY }{ massimo di y }  
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{One.Var.Stats}} }

\examples{
 x<-c(1.2,3.4,5.6,7.8,8.9,8.7)  
 y<-c(1,2,3.5,6.7,8.9,8.8) 
 Two.Var.Stats(XList=x,YList=y,Freq=1)
}

\keyword{ univar }
