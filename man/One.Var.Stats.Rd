\name{One.Var.Stats}

\alias{One.Var.Stats}

\title{ Statistiche ad 1 variabile }

\description{ \bold{One.Var.Stats} calcola media, somma, somma dei quadrati, deviazione standard, deviazione standard della popolazione, numero di osservazioni, minimo, primo quartile, mediana, terzo quartile e massimo.
}

\usage{
One.Var.Stats(List,Freq=1)
}

\arguments{
  \item{ List }{ nome dell'elenco che contiene i dati }
  \item{ Freq }{ nome dell'elenco che contiene i valori di frequenza per i dati in \emph{List}.   Valore predefinito uguale ad 1. Tutti gli elementi devono essere interi \eqn{\ge} 0 }
}

\value{
  \item{ xmean }{ media  }
  \item{ sumx }{ somma  }
  \item{ sumx2 }{ somma dei quadrati  }
  \item{ Sx }{ deviazione standard  }
  \item{ sigmax }{ deviazione standard della popolazione }
  \item{ n }{ dimensione campionaria }
  \item{ minX }{ minimo }
  \item{ Q1 }{ primo quartile }
  \item{ Med }{ mediana }
  \item{ Q3 }{ terzo quartile }
  \item{ maxX }{ massimo }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Two.Var.Stats}} }

\examples{
 x<-c(1.2,3.4,5.6,7.8,8.9,8.7)  
 One.Var.Stats(List=x,Freq=1)
}

\keyword{ univar }
