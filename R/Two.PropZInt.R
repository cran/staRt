`Two.PropZInt` <-
function(x1,n1,x2,n2,Clevel=0.95){
phat1<-x1/n1
phat2<-x2/n2
intervallo<-round((phat1-phat2)+c(-1,1)*qnorm(1-(1-Clevel)/2)*sqrt(phat1*(1-phat1)/n1+phat2*(1-phat2)/n2),4)
nomi<-c("phat1=","phat2=","n1=","n2=")
valori<-round(c(phat1,phat2,n1,n2),4)
cat("\n","2-PropZInt","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:4){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

