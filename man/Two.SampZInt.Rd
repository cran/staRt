\name{Two.SampZInt}

\alias{Two.SampZInt}

\title{ Intervallo di confidenza per la differenza di 2 mu, sigma note }

\description{ \bold{Two.SampZInt} (intervallo di confidenza \emph{z} su due campioni) calcola un intervallo di confidenza per la differenza tra le medie di due popolazioni quando entrambe le deviazioni standard delle popolazioni sono note. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}

\usage{
Two.SampZInt(sigma1,sigma2,xmean1,n1,xmean2,n2,Clevel=0.95)
}

\arguments{
  \item{ sigma1 }{ la deviazione standard nota della prima popolazione per gli intervalli su due campioni }
  \item{ sigma2 }{ la deviazione standard nota della seconda popolazione per gli intervalli su due campioni }
  \item{ xmean1 }{ media per il primo campione }
  \item{ n1 }{ dimensione del primo campione }
\item{ xmean2 }{ media per il secondo campione }
  \item{ n2 }{ dimensione del secondo campione }
  \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }
}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ xbar1 }{ media campionaria di x valori per il campione 1 }
  \item{ xbar2 }{ media campionaria di x valori per il campione 2 }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
} 

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Z.Test}}, \code{\link{Two.SampZTest}}, \code{\link{ZInterval}} }

\examples{
 Two.SampZInt(sigma1=15.5,sigma2=13.5,xmean1=131,n1=5,xmean2=117.4,n2=5,Clevel=0.99)
}

\keyword{ htest }

