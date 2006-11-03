`Two.Var.Stats` <-
function(XList,YList,Freq=1){
x<-rep(XList,Freq)
y<-rep(YList,Freq)
n<-length(x)
mx<-mean(x)
my<-mean(y)
sx<-sd(x)
sy<-sd(y)
sigmax<-sqrt((n-1)*var(x)/n)
sigmay<-sqrt((n-1)*var(y)/n)
nomi<-c("xmean=","sumx=","sumx2=","Sx=","sigmax=","n=","ymean=","sumy=","sumy2=","Sy=","sigmay=","sumxy=","minX=","maxX=","minY=","maxY=")
valori<-round(c(mx,sum(x),sum(x**2),sx,sigmax,n,my,sum(y),sum(y**2),sy,sigmay,sum(x*y),min(x),max(x),min(y),max(y)),4)
cat("\n","2-Var Stats","\n")
for(i in 1:16){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

