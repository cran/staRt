`TInterval` <-
function(xmean,Sx,n,Clevel=0.95){
mx<-xmean
sx<-Sx
intervallo<-round(mx+c(-1,1)*qt(1-(1-Clevel)/2,df=n-1)*sx/sqrt(n),4)
nomi<-c("xmean=","Sx=","n=")
valori<-round(c(mx,sx,n),4)
cat("\n","TInterval","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:3){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

