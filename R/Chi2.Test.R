`Chi2.Test` <-
function(Observed,Calculate=TRUE,Draw=TRUE){
A<-Observed
statistic<-chisq.test(A)$statistic
gradiliberta<-chisq.test(A)$parameter
pvalue<-chisq.test(A)$p.value

if(Calculate==TRUE){
nomi<-c("chi2=","p=","df=")
valori<-round(c(statistic,pvalue,gradiliberta),4)
cat("\n","Chi2-Test","\n")
for(i in 1:3){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

if(Draw==TRUE){
par(mfrow=c(1,1))
lim.sx<-0
lim.dx<-10
xx<-seq(lim.sx,lim.dx,by=0.01)
plot(xx,dchisq(xx,df=gradiliberta),xlim=c(0,10),ylim=c(-0.1,0.25),xlab="",ylab="",type="n",axes=F)
points(xx,dchisq(xx,df=gradiliberta),type="l",lwd=2)
abline(h=0,lwd=2)
box(lwd=2)
if(statistic>10){
text(-0.2,-0.09,as.expression(substitute(chi^2==cc,list(cc=round(statistic,4)))),adj=0)
text(3.3,-0.09,paste("p =",round(pvalue,4)),adj=0)
}
if(statistic<10){
xa<-seq(lim.dx,statistic,by=-0.01)
ya<-c(dchisq(xa,df=gradiliberta),0,0)
xa<-c(xa,statistic,lim.dx)
polygon(xa,ya,col="black")
text(-0.2,-0.09,as.expression(substitute(chi^2==cc,list(cc=round(statistic,4)))),adj=0)
text(3.3,-0.09,paste("p =",round(pvalue,4)),adj=0)
}
}

}
