\name{Two.SampTTest}

\alias{Two.SampTTest}

\title{ Verifica di confronto di 2 mu, sigma non note }

\description{ \bold{Two.SampTTest} (verifica \emph{t} su due campioni) verifica l'uguaglianza delle medie di due popolazioni basate su campioni indipendenti quando entrambe le deviazioni standard delle popolazioni non sono note.  
}

\usage{
Two.SampTTest(xmean1,Sx1,n1,xmean2,Sx2,n2,mu="!=",Pooled="Yes",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ xmean1 }{ media per il primo campione }
  \item{ Sx1 }{ deviazione standard per il primo campione }
  \item{ n1 }{ dimensione per il primo campione }
  \item{ xmean2 }{ media per il secondo campione }
  \item{ Sx2 }{ deviazione standard per il secondo campione }
  \item{ n2 }{ dimensione per il secondo campione }
  \item{ mu }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Pooled }{ varianza aggregata }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ t }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ df }{ gradi di libertà }
  \item{ xbar1 }{ media campionaria di x valori per il campione 1 }
  \item{ xbar2 }{ media campionaria di x valori per il campione 2 }
  \item{ Sx1 }{ deviazione standard campionaria di x valori per il campione 1 }
  \item{ Sx2 }{ deviazione standard campionaria di x valori per il campione 2 }
  \item{ Sxp }{ deviazione standard campionaria aggregata }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{T.Test}}, \code{\link{TInterval}}, \code{\link{Two.SampTInt}} }

\examples{
Two.SampTTest(xmean1=5.9,Sx1=1.7,n1=5,xmean2=1.7,Sx2=1.3,n2=5,mu="<",Pooled="Yes")

}

\keyword{ htest }

