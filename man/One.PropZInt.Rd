\name{One.PropZInt}

\alias{One.PropZInt}

\title{ Intervallo di confidenza di 1 proporzione }

\description{ \bold{One.PropZInt} (intervallo di confidenza \emph{z} per una proporzione) calcola un intervallo di confidenza per una proporzione non nota di casi favorevoli. Come input, viene preso il numero di casi favorevoli nel campione \emph{x} e il numero di osservazioni nel campione \emph{n}. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}

\usage{
One.PropZInt(x,n,Clevel=0.95)
}

\arguments{
  \item{ x }{ numero di realizzazioni favorevoli nel campione }
  \item{ n }{ numero di osservazioni del campione }
    \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }

}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ phat }{ proporzione stimata del campione }
  \item{ n }{ numero di dati }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{One.PropZTest}}, \code{\link{Two.PropZInt}}, \code{\link{Two.PropZTest}} }

\examples{
 One.PropZInt(x=2048,n=4040,Clevel=0.99)
}

\keyword{ htest }

