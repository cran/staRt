\name{TInterval}

\alias{TInterval}

\title{ Intervallo di confidenza di 1 mu, sigma non nota }

\description{ \bold{TInterval} (intervallo di confidenza \emph{t} su un unico campione) calcola un intervallo di confidenza per la media mu non nota di una popolazione quando la 
  deviazione standard sigma della popolazione non è nota. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}

\usage{
TInterval(xmean,Sx,n,Clevel=0.95)
}

\arguments{
  \item{ xmean }{ media campionaria }
  \item{ Sx }{ deviazione standard }
  \item{ n }{ dimensione campionaria }
  \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }
}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ xbar }{ media di x valori }
  \item{ Sx }{ deviazione standard del campione di x }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
} 

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{T.Test}}, \code{\link{Two.SampTTest}}, \code{\link{Two.SampTInt}} }

\examples{
 TInterval(xmean=1.75,Sx=0.1291,n=4,Clevel=0.95)
}

\keyword{ htest }


