`Two.SampTInt` <-
function(xmean1,Sx1,n1,xmean2,Sx2,n2,Clevel=0.95,Pooled="Yes"){
m1<-xmean1
m2<-xmean2
s1<-Sx1
s2<-Sx2
gradiliberta<-if(Pooled=="Yes") n1+n2-2 else (s1**2/n1+s2**2/n2)**2/(s1**4/(n1**2*(n1-1))+s2**4/(n2**2*(n2-1)))
stderr<-if(Pooled=="Yes") qt(1-(1-Clevel)/2,df=gradiliberta)*sqrt(((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2))*sqrt(1/n1+1/n2) else qt(1-(1-Clevel)/2,df=gradiliberta)*sqrt(s1**2/n1+s2**2/n2) 
intervallo<-round((m1-m2)+c(-1,1)*stderr,4)
nomi<-c("df=","xmean1=","xmean2=","Sx1=","Sx2=","n1=","n2=")
valori<-round(c(gradiliberta,m1,m2,s1,s2,n1,n2),4)
cat("\n","2-SampTInt","\n")
cat("  (",intervallo[1],",",intervallo[2],")",sep="","\n")
for(i in 1:5){
cat("  ",nomi[i],valori[i],sep="","\n")}
if(Pooled=="Yes"){cat("  ","Sxp=",sqrt(((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2)),sep="","\n")}
for(i in 6:7){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

