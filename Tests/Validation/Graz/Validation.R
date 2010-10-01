# NB. cedar (observed) files are missing.

f1<- "annual.out"
f2<- "cedar_c4past/cedar_annual.txt"

f3<- "daily.out"
f4<- "cedar_c4past/cedar_daily.txt"

read.apsim <- function (fname) {
   apsim <- read.table(fname,skip=4,header=F)
   names(apsim) <- names(read.table(fname,skip=2,header=T,nrows=1))
   return(apsim)
}

d1<-read.apsim(f1); 
d2<- read.table(f2,header=T)

pdf("summary.pdf",width=8,height=10,paper="a4")
layout(matrix(1:6, 3, 2, byrow=TRUE))
                                               what<-
z<-merge(d1, d2, by="year", suffixes=c(".apsim", ".cedar"))

for (what in names(d1)[-1]) {
   x<-paste(what,"apsim",sep=".")
   y<-paste(what,"cedar",sep=".")
   plot(z[[x]], z[[y]], xlab="apsim", ylab="cedar", main=what)
   abline(a=0, b=1)
   boxplot(d1[[what]], d2[[what]], ylab="", names=c("apsim", "cedar"), main=what)
}

d3<-read.apsim(f3) 
d3$Date<-as.Date(d3$Date,format="%d/%m/%Y")

d4<- read.table(f4,header=T)
d4$Date<-as.Date(as.character(d4$Date),format="%Y%m%d")
timespan<-1:400

layout(matrix(1:3, 3, 1, byrow=TRUE))

for (what in names(d3)[-1]) {
   ylim<- range(c(d3[[what]],d4[[what]]))
   plot(d3$Date[timespan], d3[[what]][timespan], ylim=ylim, xlab="", ylab="", main=what, type="l", col="red")
   lines(d4$Date[timespan], d4[[what]][timespan], col="blue")
   legend("topleft", legend=c("apsim", "cedar"), fill=c("red", "blue")) 
}


dev.off()
