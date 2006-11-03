\name{Z.Test}

\alias{Z.Test}

\title{ Verifica di un singolo mu, sigma nota }

\description{ \bold{Z.Test} (verifica \emph{z} su un unico campione) esegue una
  verifica dell'ipotesi sull'unica media mu non nota di una popolazione quando la 
  deviazione standard sigma della popolazione è nota.  
}

\usage{
 Z.Test(mu0,sigma,xmean,n,mu="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ mu0 }{ valore della media non nota sotto ipotesi nulla }
  \item{ sigma }{ deviazione standard nota della popolazione; deve essere un numero reale positivo }
  \item{ xmean }{ media campionaria }
  \item{ n }{ dimensione del campione }
  \item{ mu }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ z }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ xbar }{ media di x valori }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Two.SampZTest}}, \code{\link{ZInterval}}, \code{\link{Two.SampZInt}} }

\examples{
 Z.Test(mu0=300,sigma=3,xmean=299.0333,n=6,mu="!=",Calculate=TRUE,Draw=FALSE)
}

\keyword{ htest }

