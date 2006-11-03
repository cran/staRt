`Two.PropZTest` <-
function(x1,n1,x2,n2,prop="!=",Calculate=TRUE,Draw=TRUE){
phat1<-x1/n1
phat2<-x2/n2
phat<-(x1+x2)/(n1+n2)
zcritic<-(phat1-phat2)/sqrt(((x1+x2)/(n1+n2))*(1-(x1+x2)/(n1+n2))*(1/n1+1/n2))
pvalue<-if(prop=="!=") 2*pnorm(-abs(zcritic)) else if(prop=="<") pnorm(zcritic) else 1-pnorm(zcritic)

if(Calculate==TRUE){
nomi<-c("z=","p=","phat1=","phat2=","phat=","n1=","n2=")
valori<-round(c(zcritic,pvalue,phat1,phat2,phat,n1,n2),4)
cat("\n","2-PropZTest","\n")
cat("  ",paste("prop1",eval(prop),"prop2",sep=""),sep="","\n")
for(i in 1:7){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

if(Draw==TRUE){
par(mfrow=c(1,1))
lim.sx<--10
lim.dx<-10
xx<-seq(lim.sx,lim.dx,by=0.01)
plot(xx,dnorm(xx,mean=0,sd=1),xlim=c(-10,10),ylim=c(-0.06,0.5),xlab="",ylab="",type="n",axes=FALSE)
points(xx,dnorm(xx,mean=0,sd=1),type="l",lwd=2)
abline(v=0,h=0,lwd=2)
box(lwd=2)

if(abs(zcritic)>=10 & prop=="!="){
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(zcritic)<10 & prop=="!="){
xa<-seq(lim.sx,-abs(zcritic),by=0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,-abs(zcritic),lim.sx)
polygon(xa,ya,col="black")
xa<-seq(lim.dx,abs(zcritic),by=-0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,abs(zcritic),lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(zcritic>=10 & prop=="<"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,lim.dx,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(zcritic<=-10 & prop=="<"){
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(zcritic)<10 & prop=="<"){
xa<-seq(lim.sx,zcritic,by=0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,zcritic,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(zcritic<=-10 & prop==">"){
xa<-seq(lim.sx,lim.dx,by=0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,lim.dx,lim.sx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(zcritic>10 & prop==">"){
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

if(abs(zcritic)<10 & prop==">"){
xa<-seq(lim.dx,zcritic,by=-0.01)
ya<-c(dnorm(xa,mean=0,sd=1),0,0)
xa<-c(xa,zcritic,lim.dx)
polygon(xa,ya,col="black")
text(-10.2,-0.05,paste("z =",round(zcritic,4)),adj=0)
text(0.3,-0.05,paste("p =",round(pvalue,4)),adj=0)
}

}



}

