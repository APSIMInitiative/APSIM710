## Start reading this from the end...
#######################Housekeeping############################
##	this is underhanded
# Read an "apsim" file into a named R table.
read.apsim <- function (file) {
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }

    lines <- .Internal(readTableHead(file, 4, "!", FALSE))
    if(all(nchar(lines) == 0)) stop("empty header")

    summaryfile<- lines[1]
    title<- lines[2]
    col.names <- unlist(strsplit(lines[3], " +"))
    col.units <- unlist(strsplit(lines[4], " +"))
    data <- read.table(file, skip = 0, row.names=NULL, na.strings="????")
    names(data)<-col.names[-1]
    #attr(data, "units") <- col.units[-1]
    #attr(data, "summaryfile") <- summaryfile
    #attr(data, "title") <- title
    data
}

# pred:obs plotting.
plotexp <- function(pred, obs, vars, trials, scatterplot=TRUE, diffplot = TRUE) {
  for (var in vars) {
    pm<-vector(); om<-vector()
    for (i in 1:length(trials)) {
      trial <- trials[i]
      for (das in obs$das[obs$trial==trial]) {
        o<-obs[[var]][obs$trial==trial&obs$das==das]
        if (!is.na(o)) {
           p<-pred[[var]][pred$trial==trial&pred$das==das]
           if (length(p)>0) {
              pm <- c(pm, p)
              om <- c(om, o)
           }
        }
      }
    }

    if (length(om) > 0) {
      if (scatterplot) {
        mx<-max(c(pm, om))
        plot(pm, om, ylim=c(0,mx), xlim=c(0,mx),
          xlab="Pred",ylab="Obs", main=var)
        lines(c(0,mx), c(0,mx))
        lm<-pm~om

      }
      if (diffplot) {
        dm <- rev(sort(om-pm))
        mx<-max(dm); mn<-min(dm)
        plot(NA, ylim=c(mn,mx), xlim=c(0,length(dm)),
          xlab="",ylab="Obs-Pred", main=var)
        for (i in 1:length(dm)) {
          lines(c(i,i), c(0,dm[i]))
        }
      }
    }
  }
}
#######################End Housekeeping############################

# Read Observed data
obs<-read.csv("obsdata.csv", header=T)
trials <- as.character(unique(obs$trial))

##Predicted files in this dir:
#dir <- "c:/apswork/apsim21/sunflower/valid/new"
#dir <- "c:/apswork/test2.2/sunflower/new"
dir <- "new"

## Read modelled data into 1 big table
if (exists("pred")) { rm("pred") }
for (i in 1:length(trials)) {
   trial <- trials[i]
   predfile <- paste(dir, "/", trial, ".out", sep="")
   data <- read.apsim(predfile)
   if (exists("pred")) {
      pred <- rbind(pred, cbind(trial=rep(trial,dim(data)[1]),data))
   } else {
      pred <- cbind(trial=rep(trial,dim(data)[1]),data)
   }
}


## All experiments as XY scatter plots
pdf(file="valid.pdf", width=6, height=8, horizontal=F)
##-- page 1
layout(matrix(c(1,1:7), 4,2, byrow = TRUE))
plot(NA, ylim=c(0,1), xlim=c(0,1), xlab="", ylab="", axes=F, type="n", main="All Experiments")
plotexp(pred, obs, c("yield", "biomass", "lai"), trials)

layout(matrix(1:8, 4,2, byrow = TRUE))
for (var in c("lai", "biomass", "yield")) {
   all <- na.omit(c(pred[[var]], obs[[var]]))
   ylim <- c(min(all), max(all))
   for (i in 1:length(trials)) {
       trial <- trials[i]
       p<-data.frame(x=pred$das[pred$trial==trial],
                     y=pred[[var]][pred$trial==trial])
       o<-na.omit(data.frame(x=obs$das[obs$trial==trial],
                             y=obs[[var]][obs$trial==trial]))
       if (length(o$x) > 0) {
         plot(p,type="l", xlab="das", ylab=var,
              main=paste(trial,var), xlim=c(min(c(p$x,o$x)),max(c(p$x,o$x))),
              ylim=ylim)
         points(na.omit(o))
       }
   }
}

##-- extra stuff ?

dev.off()
