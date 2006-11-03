`T.Test` <-
function(mu0,xmean,Sx,n,mu="!=",Calculate=TRUE,Draw=TRUE){
mx<-xmean
sx<-Sx
tcritic<-(mx-mu0)/(sx/sqrt(n))
pvalue<-if(mu=="!=") 2*pt(-abs(tcritic),df=n-1) else if(mu=="<") pt(tcritic,df=n-1) else 1-pt(tcritic,df=n-1)

if(Calculate==TRUE){
nomi<-c("t=","p=","xmean=","Sx=","n=")
valori<-round(c(tcritic,pvalue,mx,sx,n),4)
cat("\n","T-Test","\n")
cat("  ",paste("mu",eval(mu),mu0,sep=""),sep="","\n")
for(i in 1:5){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")}

if(Draw==TRUE){
par(mfrow=c(1,1))
lim.sx<--10
lim.dx<-10
xx<-seq(lim.sx,lim.dx,by=0.01)
plot(xx,dt(xx,df=n-1),xlim=c(-10,10),ylim=c(-0.06,0.5),xlab="",ylab="",type="n",axes=FALSE)
points(xx,dt(xx,df=n-1),type="l",lwd=2)
abline(v=0,h=0,lwd=2)
box(lwd=2)

if(abs(tcritic)>=10 & mu=="!="){
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(tcritic)<10 & mu=="!="){
xa<-seq(lim.sx,-abs(tcritic),by=0.01)
ya<-c(dt(xa,df=n-1),0,0)
xa<-c(xa,-abs(tcritic),lim.sx)
polygon(xa,ya,col="black")
xa<-seq(lim.dx,abs(tcritic),by=-0.01)
ya<-c(dt(xa,df=n-1),0,0)
xa<-c(xa,abs(tcritic),lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic>=10 & mu=="<"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dt(xa,df=n-1),0,0)
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
ya<-c(dt(xa,df=n-1),0,0)
xa<-c(xa,tcritic,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(tcritic<=-10 & mu==">"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dt(xa,df=n-1),0,0)
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
ya<-c(dt(xa,df=n-1),0,0)
xa<-c(xa,tcritic,lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("t =",round(tcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}
}


}

