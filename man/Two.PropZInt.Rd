\name{Two.PropZInt}

\alias{Two.PropZInt}

\title{ Intervallo di confidenza per la differenza tra 2 proporzioni }

\description{ \bold{Two.PropZInt} (intervallo di confidenza \emph{z} per due proporzioni) calcola un intervallo di confidenza per la differenza tra la proporzione di casi favorevoli in due popolazioni. Come input, viene preso il numero di casi favorevoli in ciascun campione e il numero di osservazioni in ciascun campione. L'intervallo di confidenza calcolato dipende dal livello di confidenza specificato dall'utente.
}

\usage{
Two.PropZInt(x1,n1,x2,n2,Clevel=0.95)
}

\arguments{
  \item{ x1 }{ numero di casi favorevoli dal primo campione }
  \item{ n1 }{ numero di osservazioni nel primo campione }
  \item{ x2 }{ numero di casi favorevoli dal secondo campione }
  \item{ n2 }{ numero di osservazioni nel secondo campione }
  \item{ Clevel }{ Il livello di confidenza per le istruzioni di intervallo. Deve essere \eqn{\ge} 0 e < 100. Se il valore è \eqn{\ge} 1, si presume che venga dato come percentuale e diviso per 100. Valore predefinito uguale a 0.95 }

}

\value{
  \item{ lwr,upr }{ coppia dell'intervallo di confidenza }
  \item{ phat1 }{ proporzione stimata del campione per la popolazione 1 }
  \item{ n1 }{ numero di dati per il campione 1 }
  \item{ phat2 }{ proporzione stimata del campione per la popolazione 2 }
  \item{ n2 }{ numero di dati per il campione 2 }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{One.PropZInt}}, \code{\link{One.PropZTest}}, \code{\link{Two.PropZTest}} }

\examples{
 Two.PropZInt(x1=49,n1=61,x2=38,n2=62,Clevel=0.95)
}

\keyword{ htest }

