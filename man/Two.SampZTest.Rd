\name{Two.SampZTest}

\alias{Two.SampZTest}

\title{ Verifica di confronto di 2 mu, sigma note }

\description{ \bold{Two.SampZTest} (verifica \emph{z} su due campioni) verifica l'uguaglianza delle medie di due popolazioni basate su campioni indipendenti quando entrambe le deviazioni standard delle popolazioni sono note.  
}

\usage{
Two.SampZTest(sigma1,sigma2,xmean1,n1,xmean2,n2,mu="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ sigma1 }{ deviazione standard nota della prima popolazione }
  \item{ sigma2 }{ deviazione standard nota della seconda popolazione }
  \item{ xmean1 }{ media per il primo campione }
  \item{ n1 }{ dimensione per il primo campione }
  \item{ xmean2 }{ media per il secondo campione }
  \item{ n2 }{ dimensione per il secondo campione }
  \item{ mu }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ z }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ xbar1 }{ media campionaria di x valori per il campione 1 }
  \item{ xbar2 }{ media campionaria di x valori per il campione 2 }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Z.Test}}, \code{\link{ZInterval}}, \code{\link{Two.SampZInt}} }

\examples{
 Two.SampZTest(sigma1=1.5,sigma2=3.5,xmean1=3.1,n1=5,xmean2=1.7,n2=5,mu="!=")

}

\keyword{ htest }

