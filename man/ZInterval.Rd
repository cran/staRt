\name{ZInterval}

\alias{ZInterval}

\title{ Intervallo di confidenza di 1 mu, sigma nota }

\description{ \bold{ZInterval} (intervallo di confidenza \emph{z} su un unico campione) calcola un intervallo di confidenza per la media mu non nota di una popolazione quando la 
  deviazione standard sigma della popolazione è nota. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}
 
\usage{
 ZInterval(sigma,xmean,n,Clevel=0.95)
}

\arguments{
  \item{ sigma }{ deviazione standard nota della popolazione }
  \item{ xmean }{ media campionaria }
  \item{ n }{ dimensione campionaria }
  \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }
}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ xbar }{ media di x valori }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Z.Test}}, \code{\link{Two.SampZTest}}, \code{\link{Two.SampZInt}} }

\examples{
 ZInterval(sigma=3,xmean=299.0333,n=6,Clevel=0.95)
}

\keyword{ htest }

