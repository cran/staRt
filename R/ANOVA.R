`ANOVA` <-
function(...){
k<-nargs(...)
lista<-list(...)
dime<-unlist(lapply(lista,length))
stddev<-unlist(lapply(lista,sd))
n<-sum(unlist(lapply(lista,length)))
y<-unlist(list(...))
devy<-(n-1)*var(y)
devwit<-sum((dime-1)*stddev**2)
devbet<-devy-devwit
Fvalue<-(devbet/(k-1))/(devwit/(n-k))
pvalue<-1-pf(Fvalue,df1=k-1,df2=n-k)
cat("\n","One-way ANOVA","\n")
for(i in 1:2){
cat("  ",c("F=","p=")[i],round(c(Fvalue,pvalue),4)[i],sep="","\n")}
cat("  ","Factor",sep="","\n")
for(i in 1:3){
cat("   ",c("df=","SS=","MS=")[i],round(c(k-1,devbet,devbet/(k-1)),4)[i],sep="","\n")}
cat("  ","Error",sep="","\n")
for(i in 1:3){
cat("   ",c("df=","SS=","MS=")[i],round(c(n-k,devwit,devwit/(n-k)),4)[i],sep="","\n")}
cat("  ","Sxp=",round(sqrt(devwit/(n-k)),4),sep="","\n")
cat("\n")
}

