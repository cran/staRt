\name{One.PropZTest}

\alias{One.PropZTest}

\title{ Verifica di una proporzione }

\description{ \bold{One.PropZTest} (verifica \emph{z} di una proporzione) esegue una verifica di una proporzione non nota di casi favorevoli (prop). Come input, viene preso il numero di casi favorevoli nel campione \emph{x} e il numero di osservazioni nel campione \emph{n}.
}

\usage{
One.PropZTest(prop0=0.5,x,n,prop="!=",Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ prop0 }{ valore di prop sotto ipotesi nulla }
  \item{ x }{ numero di realizzazioni favorevoli nel campione }
  \item{ n }{ numero di osservazioni nel campione }
  \item{ prop }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ z }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ phat }{ proporzione stimata nel campione }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{One.PropZInt}}, \code{\link{Two.PropZInt}}, \code{\link{Two.PropZTest}} }

\examples{

 One.PropZTest(prop0=0.5,x=2048,n=4040,prop="!=",Calculate=TRUE,Draw=FALSE)

}

\keyword{ htest }

