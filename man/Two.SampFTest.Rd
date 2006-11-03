\name{Two.SampFTest}

\alias{Two.SampFTest}

\title{ Verifica di confronto di 2 sigma }

\description{ \bold{Two.SampFTest} (verifica \emph{F} su due campioni) esegue un test \emph{F} per confrontare le deviazioni standard di una popolazione normale. Le medie e le deviazioni standard non sono note.
}

\usage{
Two.SampFTest(Sx1,n1,Sx2,n2,sigma="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ Sx1 }{ deviazione standard per il primo campione }
  \item{ n1 }{ dimensione del primo campione }
  \item{ Sx2 }{ deviazione standard per il secondo campione }
  \item{ n2 }{ dimensione del secondo campione }
  \item{ sigma}{ ipotesi alternativa minore (<1), maggiore (>1) o diversa   (!=1)}
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ F }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ Sx1 }{ deviazione standard campionaria di x valori per il campione 1 }
  \item{ Sx2 }{ deviazione standard campionaria di x valori per il campione 2 }
  \item{ n1 }{ numero di dati per il campione 1 }  
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Two.SampZTest}}, \code{\link{Two.SampTTest}} }

\examples{
 Two.SampFTest(Sx1=8.7433,n1=10,Sx2=5.9007,n2=11,sigma="!=",Calculate=TRUE,Draw=FALSE)
}

\keyword{ htest }


