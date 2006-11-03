`ZInterval` <-
function(sigma,xmean,n,Clevel=0.95){
mx<-xmean
intervallo<-round(mx+c(-1,1)*qnorm(1-(1-Clevel)/2)*sigma/sqrt(n),4)
nomi<-c("xmean=","n=")
valori<-round(c(mx,n),4)
cat("\n","ZInterval","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:2){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

