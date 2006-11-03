.First.lib<-
function(lib,pkg)
{
    if(!require("stats",quietly=TRUE))
		requite(ts)
    mylib <- dirname(system.file(package = "staRt"))
    ver <- packageDescription("staRt", lib = mylib)["Version"]
    txt <- c("\n",
             paste("Package",sQuote("staRt"),"versione", ver,"caricato correttamente.",sep=" "),
             "\n",
             paste(sQuote("staRt"),
                   "Emulatore dei comandi di statistica inferenziale",
                   "presenti sulla calcolatrice grafica",				   
                   "'TI 83-Plus'."),
             "\n",
             paste("Si veda",
                   sQuote("library(help=\"staRt\")"),"oppure",sQuote("help(package=\"staRt\")"),
                   "per i comandi disponibili."),
             "\n")
    if(interactive() || getOption("verbose"))
        writeLines(strwrap(txt, indent = 1, exdent = 1))
}



