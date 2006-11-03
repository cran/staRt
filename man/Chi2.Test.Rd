\name{Chi2.Test}

\alias{Chi2.Test}

\title{ Verifica chi quadrato per tabelle a 2 variabili }

\description{ \bold{Chi2.Test} (verifica chi quadrato) esegue un test chi quadrato dell'associazione tra il numero di realizzazioni nella tabella a due variabili della matrice \emph{Observed} (delle osservazioni) specificata. L'ipotesi nulla per una tabella a due variabili è: non esiste alcuna associazione tra la variabile di riga e la variabile di colonna. L'ipotesi alternativa è: le variabili sono correlate.
}

\usage{
Chi2.Test(Observed,Calculate=TRUE,Draw=TRUE)
}

\arguments{
  \item{ Observed }{ nome della matrice che rappresenta le colonne e le righe per i valori osservati di una tabella a due dimensioni }
  \item{ Calculate }{ risultati calcolati }
  \item{ Draw }{ risultati grafici }
}

\value{
  \item{ chi2 }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ df }{ gradi di libertà }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
}

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\examples{
 
 A<-matrix(c(5,19,8,16,11,13),nrow=3,ncol=2)
 Chi2.Test(Observed=A,Calculate=TRUE,Draw=FALSE)

}

\keyword{ htest }


