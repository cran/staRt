\name{ANOVA}

\alias{ANOVA}

\title{ Analisi della varianza ad una variabile }

\description{ \bold{ANOVA} (analisi della varianza ad una dimensione) calcola l'analisi della varianza ad una variabile per confrontare le medie di popolazione. La procedura ANOVA per confrontare queste medie utilizza l'analisi della varianza dei dati del campione. L'ipotesi nulla di uguaglianza tra le medie viene verificata in contrapposizione all'alternativa che non tutte le medie sono uguali.  
}

\usage{
ANOVA(...)
}

\arguments{
  \item{\dots}{ nomi degli elenchi che contengono i dati che si stanno verificando }
}

\value{
  \item{ F }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ Factor df }{ gradi di libertà fattore }
  \item{ Factor SS }{ somma dei quadrati fattore }
  \item{ Factor MS }{ quadrati medi fattore }
  \item{ Error df }{ gradi di libertà errore }
  \item{ Error SS }{ somma dei quadrati errore }
  \item{ Error MS }{ quadrati medi errore }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
} 

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\seealso{ \code{\link{Two.SampTTest}} }

\examples{
 x<-c(7,4,6,6,5)
 y<-c(6,5,5,8,7)
 z<-c(4,7,6,7,6)
 ANOVA(x,y,z)
}

\keyword{ htest }


