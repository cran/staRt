\name{Two.PropZTest}

\alias{Two.PropZTest}

\title{ Verifica di confronto di 2 proporzioni }

\description{ \bold{Two.PropZTest} (verifica \emph{z} di due proporzioni) esegue una verifica per confrontare le proporzioni di casi favorevoli in due popolazioni. Come input, viene preso il numero di casi favorevoli in ciascun campione e il numero di osservazioni in ciascun campione.
}

\usage{
Two.PropZTest(x1,n1,x2,n2,prop="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ x1 }{ numero di casi favorevoli dal primo campione }
  \item{ n1 }{ numero di osservazioni nel primo campione }
  \item{ x2 }{ numero di casi favorevoli dal secondo campione }
  \item{ n2 }{ numero di osservazioni nel secondo campione }
  \item{ prop }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ z }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ phat1 }{ proporzione stimata del campione per la popolazione 1 }
  \item{ phat2 }{ proporzione stimata del campione per la popolazione 2 }
  \item{ phat }{ proporzione pooled }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{One.PropZTest}}, \code{\link{One.PropZInt}}, \code{\link{Two.PropZInt}} }

\examples{
 Two.PropZTest(x1=45,n1=61,x2=38,n2=62,prop="!=",Calculate=TRUE,Draw=FALSE)

}

\keyword{ htest }

