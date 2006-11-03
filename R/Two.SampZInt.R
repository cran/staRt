`Two.SampZInt` <-
function(sigma1,sigma2,xmean1,n1,xmean2,n2,Clevel=0.95){
m1<-xmean1
m2<-xmean2
intervallo<-round((m1-m2)+c(-1,1)*qnorm(1-(1-Clevel)/2)*sqrt(sigma1**2/n1+sigma2**2/n2),4)
nomi<-c("xmean1=","xmean2=","n1=","n2=")
valori<-round(c(m1,m2,n1,n2),4)
cat("\n","2-SampZInt","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:4){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

