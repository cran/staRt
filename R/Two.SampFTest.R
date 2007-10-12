`Two.SampFTest` <-
function(Sx1,n1,Sx2,n2,sigma="!=",Calculate=TRUE,Draw=TRUE){
s1<-Sx1
s2<-Sx2
fcritic<-(s1/s2)**2
pvalue<-if(sigma=="!=") 2*min(pf(fcritic,df1=n1-1,df2=n2-1),1-pf(fcritic,df1=n1-1,df2=n2-1)) else if(sigma=="<") pf(fcritic,df1=n1-1,df2=n2-1) else 1-pf(fcritic,df1=n1-1,df2=n2-1)

if(Calculate==TRUE){
nomi<-c("F=","p=","Sx1=","Sx2=","n1=","n2=")
valori<-round(c(fcritic,pvalue,s1,s2,n1,n2),4)
cat("\n","2-SampFTest","\n")
cat("  ",paste("sigma1",eval(sigma),"sigma2",sep=""),sep="","\n")
for(i in 1:6){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")}

if(Draw==TRUE){
par(mfrow=c(1,1))
lim.sx<-0
lim.dx<-10
xx<-seq(lim.sx,lim.dx,by=0.01)
plot(xx,df(xx,df1=n1-1,df2=n2-1),xlim=c(0,10),ylim=c(-0.06,1.2),xlab="",ylab="",type="n",axes=FALSE)
points(xx,df(xx,df1=n1-1,df2=n2-1),type="l",lwd=2)
abline(h=0,lwd=2)
box(lwd=2)

if(sigma=="!="){
fcritic1<-fcritic
ppvalue<-pf(fcritic1,df1=n1-1,df2=n2-1)
if(ppvalue<0.5) fcritic2<-qf(p=ppvalue,df1=n1-1,df2=n2-1,lower.tail=FALSE)
else
{
fcritic2<-fcritic1
fcritic1<-qf(p=1-ppvalue,df1=n1-1,df2=n2-1,lower.tail=TRUE)
}

if(fcritic2>=10){
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)}

if(fcritic2<10){
xa<-seq(lim.sx,fcritic1,by=0.01)
ya<-c(df(xa,df1=n1-1,df2=n2-1),0,0)
xa<-c(xa,fcritic1,lim.sx)
polygon(xa,ya,col="black")
xa<-seq(lim.dx,fcritic2,by=-0.01)
ya<-c(df(xa,df1=n1-1,df2=n2-1),0,0)
xa<-c(xa,fcritic2,lim.dx)
polygon(xa,ya,col="black")
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}
}

if(fcritic>=10 & sigma=="<"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(df(xa,df1=n1-1,df2=n2-1),0,0)
xa<-c(xa,lim.dx,lim.sx)
polygon(xa,ya,col="black")
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(fcritic<10 & sigma=="<"){
xa<-seq(lim.sx,fcritic,by=0.01)
ya<-c(df(xa,df1=n1-1,df2=n2-1),0,0)
xa<-c(xa,fcritic,lim.sx)
polygon(xa,ya,col="black")
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}


if(fcritic>10 & sigma==">"){
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(fcritic<10 & sigma==">"){
xa<-seq(lim.dx,fcritic,by=-0.01)
ya<-c(df(xa,df1=n1-1,df2=n2-1),0,0)
xa<-c(xa,fcritic,lim.dx)
polygon(xa,ya,col="black")
text(-0.2,-0.05,paste("F =",round(fcritic,4)),adj=0)
text(3.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

}


}

