\name{T.Test}

\alias{T.Test}


\title{ Verifica di un singolo mu, sigma non nota }

\description{ \bold{T.Test} (verifica \emph{t} su un unico campione) esegue una
  verifica dell'ipotesi sull'unica media mu non nota di una popolazione quando la 
  deviazione standard sigma della popolazione non è nota.  
}

\usage{
T.Test(mu0,xmean,Sx,n,mu="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ mu0 }{ valore della media non nota sotto ipotesi nulla }
  \item{ xmean }{ media campionaria }
  \item{ Sx }{ deviazione standard }
  \item{ n }{ dimensione campionaria }
  \item{ mu }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ t }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ xbar }{ media di x valori }
  \item{ Sx }{ deviazione standard del campione di x }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Two.SampTTest}}, \code{\link{TInterval}}, \code{\link{Two.SampTInt}} }

\examples{
 T.Test(mu0=300,xmean=103.9667,Sx=11.4669,n=6,mu="!=",Calculate=TRUE,Draw=FALSE)
	
}

\keyword{ htest }

