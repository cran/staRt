`One.Var.Stats` <-
function(List,Freq=1){
x<-rep(List,Freq)
x<-sort(x)
n<-length(x)
m<-floor((n+1)/2)
mx<-mean(x)
sx<-sd(x)
sigmax<-sqrt((n-1)*var(x)/n)
nomi<-c("xmean=","sumx=","sumx2=","Sx=","sigmax=","n=","minX=","Q1=","Med=","Q3=","maxX=")
valori<-round(c(mx,sum(x),sum(x**2),sx,sigmax,n,min(x),if(n %% 2 == 0) median(x[1:m]) else median(x[1:(m-1)]),median(x),median(x[(m+1):n]),max(x)),4)
cat("\n","1-Var Stats","\n")
for(i in 1:11){
cat("  ",nomi[i],valori[i],sep="","\n")}
cat("\n")
}

