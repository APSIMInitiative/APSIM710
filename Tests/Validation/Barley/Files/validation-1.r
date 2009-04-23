# Validation runs #1.
# Produce 1 page for each treatment. Just predicted & observed data.

library(RODBC)

# The "latest" runs (ie your development tree)
indir<-"c:/development/apsim/Barley/Validation/Files"

# Where to write output
outDir<-".."

Crop<-"Barley"

# "Aggregate" lists
groups<-list(c("biomass_wt", "grain_wt", "head_wt"),
             c("deadleaf_wt", "leaf_wt", "stem_wt"),
             c("lai", "tiller_no"),
             c("biomass_n", "grain_n"),
             c("leaf_n", "stem_n"),
             c("stage"),
             c("swdef_photo", "swdef_expan"),
             c("nfact_photo", "nfact_expan"))

headings<-list(c("Biomass"),
             c("Partitioning"),
             c("Canopy"),
             c("Biomass_N"),
             c("N_Partitioning"),
             c("Phenology"),
             c("WaterStress"),
             c("NitrogenStress"))


report <- function(outDir, obs,pred,Groups,Headings, Crop,expName,tName) {
   fnames <- vector()
   colours <- c("red", "blue", "green")
   obs$date <- as.Date(obs$date,"%d/%m/%Y")
   pred$date <- as.Date(as.character(pred$date),"%d/%m/%Y")
cat("e=",expName,"t=",tName,"\n")
cat("pred=",names(pred),"\n")
cat("obs=",names(obs),"\n")
   for (group in Groups) {
      xlims <- range(obs$date,pred$date,finite=T)
      y <- vector()
      for (series in unlist(group)) {
         y <- c(y,pred[[series]],obs[[series]])
      }
      ylims <- range(y,finite=T)
      if (sum(is.finite(ylims)) == 2) {
        fname <- paste(Headings[length(fnames)+1],"png",sep=".")
        fnames <- c(fnames, fname)
        dir.create(paste(outDir, expName, sep="/"))
        dir.create(paste(outDir, expName, tName, sep="/"))
        png(file=paste(outDir, expName, tName, fname, sep="/"), bg=rgb(243,243,243,max=255), width=350, height=350)
        plot(NA, xlim=xlims, ylim=ylims, cex.axis=.8, ylab="", xaxt="n", xlab="")
        axis.Date(1,c(pred$date,obs$date))
        for (series in unlist(group)) {
           colour <- colours[match(series,unlist(group))]

           if (sum(!is.na(pred[[series]])) > 0) {
              matplot(pred$date,pred[[series]],type="l",col=colour,add=T)
           }
           if (sum(!is.na(obs[[series]])) > 0) {
              points(obs$date,obs[[series]],pch=15,col=colour)
           }
        }
        legend(xlims[1],ylims[2],unlist(group),col=colours,pch=15)
        dev.off()
     }
   }
   return(fnames)
}

read.apsim<-function(filename) {
  s<-file.info(filename)
  data <- NULL
  if (!is.na(s$size) && s$size > 0) {
    conn <- file(filename)
    hdr <- readLines(conn, 4)[3]
    hdr<-sub(" +", "", hdr)
    data <- read.table(conn,header=F,skip=4,col.names=unlist(strsplit(hdr, " +")))
    names(data)<-tolower(names(data))
  }
  return(data)
}

squash<-function(pred, obs) {
  ids<- match(as.character(obs$x), as.character(pred$x))
  return (data.frame(x=obs$y, y=pred$y[ids]))
}

mdbFilename<-paste(indir, "/", Crop, ".mdb",sep="")
db <- odbcConnectAccess(mdbFilename)

crops <- sqlQuery(db, "SELECT Crops.CropID, Crops.Crop FROM Crops;")

experiments<-sqlQuery(db, "SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;")
traits<-tolower(as.character(sqlQuery(db, "SELECT Traits.Trait FROM Traits;")$Trait))

allTreatments<-vector()
allData<-list()


for (e in 1:dim(experiments)[1]) {
  expID<-experiments$ExpID[e]
  expName<-as.character(experiments$Experiment[e])
  treatments<-sqlQuery(db,paste(
    "TRANSFORM First(Levels.Level) AS FirstOfLevel \
     SELECT Experiments.ExpID, Designs.TreatmentID \
     FROM (Experiments INNER JOIN Treatments ON Experiments.ExpID = \
     Treatments.ExpID) INNER JOIN ((Factors INNER JOIN Levels ON \
     Factors.FactorID = Levels.FactorID) INNER JOIN Designs ON Levels.LevelID = \
     Designs.LevelID) ON Treatments.TreatmentID = Designs.TreatmentID \
     WHERE (((Experiments.ExpID)= ", expID, ")) \
     GROUP BY Experiments.ExpID, Designs.TreatmentID \
     ORDER BY Designs.TreatmentID \
     PIVOT Factors.Factor;", sep=""))

  for (t in 1:dim(treatments)[1]) {
    tID<-treatments$TreatmentID[t]
    tName<- as.character(sqlQuery(db, paste("SELECT Treatments.TreatmentName \
         FROM Treatments WHERE (Treatments.TreatmentID = ", tID, ");"))$TreatmentName)

    allTreatments<-append(allTreatments, paste(expName, "_", tName, sep=""))

    pred<-read.apsim(paste(indir, "/", Crop, "_", expName, "_", tName, ".out", sep=""))

    obs<-sqlQuery(db,paste(
         "TRANSFORM Avg(PlotData.Value) AS AvgOfValue \
          SELECT Plots.TreatmentID, PlotData.Date \
          FROM Plots INNER JOIN (Traits INNER JOIN PlotData ON Traits.TraitID = \
          PlotData.TraitID) ON Plots.PlotID = PlotData.PlotID \
          WHERE (((Plots.TreatmentID)=", tID, ")) \
          GROUP BY Plots.TreatmentID, PlotData.Date \
          ORDER BY PlotData.Date PIVOT Traits.Trait;"));
    names(obs) <- tolower(names(obs))
    images<-report(outDir, obs, pred, groups, headings, Crop, expName, tName)

  }
}
