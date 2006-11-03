`Two.SampTTest` <-
function(xmean1,Sx1,n1,xmean2,Sx2,n2,mu="!=",Pooled="Yes",Calculate=TRUE,Draw=TRUE){
m1<-xmean1
m2<-xmean2
s1<-Sx1
s2<-Sx2
gradiliberta<-if(Pooled=="Yes") n1+n2-2 else (s1**2/n1+s2**2/n2)**2/(s1**4/(n1**2*(n1-1))+s2**4/(n2**2*(n2-1)))
stderr<-if(Pooled=="Yes") sqrt(((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2))*sqrt(1/n1+1/n2) else sqrt(s1**2/n1+s2**2/n2)
tcritic<-(m1-m2)/stderr
pvalue<-if(mu=="!=") 2*pt(-abs(tcritic),df=gradiliberta) else if(mu=="<") pt(tcritic,df=gradiliberta) else 1-pt(tcritic,df=gradiliberta)

if(Calculate==TRUE){
nomi<-c("t=","p=","df=","xmean1=","xmean2=","Sx1=","Sx2=","n1=","n2=")
valori<-round(c(tcritic,pvalue,gradiliberta,m1,m2,s1,s2,n1,n2),4)
cat("\n","2-SampTTest","\n")
cat("  ",paste("mu1",eval(mu),"mu2",sep=""),sep="","\n")
for(i in 1:7){
cat("  ",nomi[i],valori[i],sep="","\n")
}
if(Pooled=="Yes"){cat("  ","Sxp=",sqrt(((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2)),sep="","\n")}
for(i in 8:9){
cat("  ",nomi[i],valori[i],sep="","\n")
}
cat("\n")}

if(Draw==TRUE){
par(mfrow=c(1,1))
lim.sx<--10
lim.dx<-10
xx<-seq(lim.sx,lim.dx,by=0.01)
plot(xx,dt(xx,df=gradiliberta),xlim=c(-10,10),ylim=c(-0.06,0.5),xlab="",ylab="",type="n",axes=FALSE)
points(xx,dt(xx,df=gradiliberta),type="l",lwd=2)
abline(v=0,h=0,lwd=2)
box(lwd=2)

if(abs(tcritic)>=10 & mu=="!="){
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(tcritic)<10 & mu=="!="){
xa<-seq(lim.sx,-abs(tcritic),by=0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,-abs(tcritic),lim.sx)
polygon(xa,ya,col="black")
xa<-seq(lim.dx,abs(tcritic),by=-0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,abs(tcritic),lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic>=10 & mu=="<"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,lim.dx,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic<=-10 & mu=="<"){
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(tcritic)<10 & mu=="<"){
xa<-seq(lim.sx,tcritic,by=0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,tcritic,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic<=-10 & mu==">"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,lim.dx,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic>10 & mu==">"){
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(tcritic)<10 & mu==">"){
xa<-seq(lim.dx,tcritic,by=-0.01)
ya<-c(dt(xa,df=gradiliberta),0,0)
xa<-c(xa,tcritic,lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}
}


}

