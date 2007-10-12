.onLoad<-function(libname,pkgname){
require("tools",quietly=TRUE)  
}

.onAttach<-function(libname,pkgname){
  #library.dynam(pkgname,pkgname,lib.loc=libname)
  options(warn=-1)
  cat("\n")
  cat("---------------------------------------------------------------------------------\n")
  pkg.info<-drop(read.dcf(file=file.path(.find.package(package=pkgname,lib.loc=libname),"DESCRIPTION")))
  ndescr<-length(pkg.info)-2
  for(i in 1:ndescr)
  cat(paste(names(pkg.info[i]),":",sep=""),pkg.info[i],"\n")
  cat("=================================================================================\n")
  if(packageHasNamespace(package=pkgname,package.lib=libname))
  cat("NAMESPACE:","yes","\n")
  else
  cat("NAMESPACE:","no","\n")
  if(any(names(getLoadedDLLs())==pkgname))
  cat("C/Fortran:","yes","\n")
  else
  cat("C/Fortran:","no","\n")
  cat("=================================================================================\n")
  filename<-file.path(.find.package(package=pkgname,lib.loc=libname),"doc")
  result<-list_files_with_type(filename,"vignette")
  if(length(result)==0){
  cat(paste(pkg.info["Package"]," not contains Vignettes.",sep=""))
  cat("\n")
  }
  if(length(result)>0){
  cat(paste(pkg.info["Package"]," contains Vignettes.",sep=""))
  cat("\nType",sQuote(paste("vignette(package=\"",pkg.info["Package"],"\")",sep="")),
  "to list the Vignettes.")
  cat("\nType",sQuote(paste("vignette(topic=\"name\",package=\"",
  pkg.info["Package"],"\")",sep="")),"to view the Vignette.")
  cat("\nYou can choose name in:\n")
  filename2<-file.path(.find.package(package=as.character(pkgname),lib.loc=libname),"doc")
  allv<-list.files(filename2,"pdf")
  number<-length(allv)
  for(i in 1:number){
  myfile<-paste(filename2,list.files(filename2,"pdf")[i],sep="/")
  miastringa<-unlist(strsplit(myfile,split="/"))[length(unlist(strsplit(myfile,split="/")))]
  miasottostringa<-substr(miastringa,1,nchar(miastringa)-4)
  cat(miasottostringa,"\n")
  }
  }
  cat("=================================================================================\n")
  cat("Type",sQuote(paste("help(package=\"",pkg.info["Package"],"\")",sep="")),
  "or",sQuote(paste("package?",pkg.info["Package"],sep="")),"to get started.\n")
  cat("Type",sQuote(paste("data(package=\"",pkg.info["Package"],"\")",sep="")),
  "to view a list of data frames.\n")
  cat("Type",sQuote(paste("ls(\"package:",pkg.info["Package"],"\")",sep="")),
  "to view a list of functions.\n")
  cat("Type",sQuote(paste("citation(package=\"",pkg.info["Package"],"\")",sep="")),
  "to view the bibiography.\n")
  cat("Type",sQuote(paste("detach(package:",pkg.info["Package"],")",sep="")),
  "to remove it from the search() path.\n")
  cat("---------------------------------------------------------------------------------\n")
  cat("\n")
  options(warn=0)
  return(invisible(0))  
  
}

  

.Last.lib<-function(libname){
  stringa<-unlist(strsplit(x=libname,split="/"))
  pkgname<-stringa[length(stringa)]
  #library.dynam.unload(pkgname,libpath=libname)
  txt<-paste("Thank you to use",sQuote(pkgname),"package. See you.")
  writeLines(txt)
}
