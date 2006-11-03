`One.PropZInt` <-
function(x,n,Clevel=0.95){
phat<-x/n
intervallo<-round(phat+c(-1,1)*qnorm(1-(1-Clevel)/2)*sqrt(phat*(1-phat)/n),4)
nomi<-c("phat=","n=")
valori<-round(c(phat,n),4)
cat("\n","1-PropZInt","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:2){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

