`LinRegTTest` <-
function(XList,YList,Freq=1,beta="!=0"){
x<-rep(XList,Freq)
y<-rep(YList,Freq)
n<-length(XList)
modello<-lm(YList~XList)
erre<-cor(XList,YList)
a<-coef(modello)[1]
b<-coef(modello)[2]
tcritic<-b/sqrt(vcov(modello)[2,2])
pvalue<-if(beta=="!=0") 2*pt(-abs(tcritic),df=n-2) else if(beta=="<0") pt(tcritic,df=n-2) else 1-pt(tcritic,df=n-2)
sigma<-summary.lm(modello)$sigma
nomi<-c("t=","p=","df=","a=","b=","s=","r2=","r=")
valori<-round(c(tcritic,pvalue,n-2,a,b,sigma,erre**2,erre),4)
cat("\n","LinRegTTest","\n")
cat("  ","y=a+bx",sep="","\n")
cat(" ",paste("beta",eval(beta),sep="")," and ",paste("rho",eval(beta),sep=""),"\n")
for(i in 1:8){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

