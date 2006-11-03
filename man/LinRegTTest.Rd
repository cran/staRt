\name{LinRegTTest}

\alias{LinRegTTest}

\title{ Verifica t della pendenza della regressione }

\description{ \bold{LinRegTTest} (test \emph{t} sulla regressione lineare) esegue una regressione lineare sui dati assegnati ed un test \emph{t} sul valore della pendenza e sul coefficiente di correlazione  per l'equazione di regressione. Viene verificata l'ipotesi nulla che il coefficiente angolare sia uguale a 0 (in modo equivalente che il coefficiente di correlazione sia uguale a 0).
}

\usage{
LinRegTTest(XList,YList,Freq=1,beta="!=0")
}

\arguments{
  \item{ XList }{ nome dell'elenco che contiene i dati per la variabile indipendente }
  \item{ YList }{ nome dell'elenco che contiene i dati per la variabile dipendente }
  \item{ Freq }{ nome dell'elenco che contiene i valori di frequenza per i dati in \emph{XList} e \emph{YList}.   Valore predefinito uguale ad 1. Tutti gli elementi devono essere interi \eqn{\ge} 0 }
  \item{ beta }{ ipotesi alternativa minore (<), maggiore (>) o diversa (!=) }
}

\value{
  \item{ t }{ statistica di verifica }
  \item{ p }{ valore p }
  \item{ df }{ gradi di libertà }
  \item{ a }{ intercetta }
  \item{ b }{ coefficiente angolare }
  \item{ s }{ errore standard della retta }
  \item{ r2 }{ indice di determinazione }
  \item{ r }{ coefficiente di correlazione }
}

\references{
  Texas Instruments, (1999) \emph{TI-83 Plus CALCOLATORE GRAFICO MANUALE}
} 

\author{ Fabio Frascati <fabiofrascati@yahoo.it>}

\examples{
 x<-c(38,56,59,64,70)
 y<-c(41,63,70,72,84) 
 LinRegTTest(XList=x,YList=y,Freq=1,beta="!=0")
}

\keyword{ htest }

