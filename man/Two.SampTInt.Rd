\name{Two.SampTInt}

\alias{Two.SampTInt}

\title{ Intervallo di confidenza per la differenza di 2 mu, sigma non note }

\description{ \bold{Two.SampTInt} (intervallo di confidenza \emph{t} su due campioni) calcola un intervallo di confidenza per la differenza tra le medie di due popolazioni quando entrambe le deviazioni standard delle popolazioni non sono note. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}

\usage{
Two.SampTInt(xmean1,Sx1,n1,xmean2,Sx2,n2,Clevel=0.95,Pooled="Yes")
}

\arguments{
  \item{ xmean1 }{ media per il primo campione }
  \item{ Sx1 }{ deviazione standard per il primo campione }
  \item{ n1 }{ dimensione del primo campione }
  \item{ xmean2 }{ media per il secondo campione }
  \item{ Sx2 }{ deviazione standard per il secondo campione }  
  \item{ n2 }{ dimensione del secondo campione }
  \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }
  \item{ Pooled }{ ipotesi di ugual varianza }
}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ df }{ gradi di libertà }
  \item{ xbar1 }{ media campionaria di x valori per il campione 1 }
  \item{ xbar2 }{ media campionaria di x valori per il campione 2 }
  \item{ Sx1 }{ deviazione standard campionaria di x valori per il campione 1 }
  \item{ Sx2 }{ deviazione standard campionaria di x valori per il campione 2 }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
} 

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{TInterval}}, \code{\link{T.Test}}, \code{\link{Two.SampTTest}} }

\examples{
Two.SampTInt(xmean1=15.9,Sx1=6.7,n1=6,xmean2=9.4,Sx2=1.9,n2=6,Clevel=0.99,Pooled="Yes")
}

\keyword{ htest }


