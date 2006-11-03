`One.PropZTest` <-
function(prop0=0.5,x,n,prop="!=",Calculate=TRUE,Draw=TRUE){
phat<-x/n
zcritic<-(phat-prop0)/sqrt(prop0*(1-prop0)/n)
pvalue<-if(prop=="!=") 2*pnorm(-abs(zcritic)) else if(prop=="<") pnorm(zcritic) else 1-pnorm(zcritic)

if(Calculate==TRUE){
nomi<-c("z=","p=","phat=","n=")
valori<-round(c(zcritic,pvalue,phat,n),4)
cat("\n","1-PropZTest","\n")
cat("  ",paste("prop",eval(prop),prop0,sep=""),sep="","\n")
for(i in 1:4){
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

