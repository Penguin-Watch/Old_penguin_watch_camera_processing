
rm(list=ls())
ls()


library(spatstat)
install.packages("spatstat")

###Concatenate 

for (camera in c('PETEb', 'PETEc')){

cameraname<- camera

path<-"C:/Users/Fiona Jones/Dropbox/Penguin_DPhil/Penguins_Greg/"
workdir<-paste(path, cameraname, "/", sep = "")
setwd(workdir)

filenames<-list.files()

allimages<-NULL
for (i in 1:length(filenames)){
  x<-read.table(filenames[i],header=FALSE,sep=",", skip=1,
                col.names=c("penguin_index","x_center", "y_center", "probability_of_adult", "probability_of_chick", "probability_of_egg", "probability_of_true_positive", "num_markings"))
  
  ifelse(length(x$penguin_index)<1, x[1,]<-NA, x$penguin_index[1]<-x$penguin_index[1])

  x$name<-filenames[i]
    allimages<-rbind(allimages,x,deparse.level = 1)
}

savepath<-paste(path, cameraname, "zooconc.csv", sep = "")
write.table(allimages, savepath, sep=",", col.names=TRUE, row.names=FALSE) }

###############

imagenames<-levels(as.factor(allimages$name))
for (i in 1:length(imagenames)){
  
egnnd<-allimages[allimages[,'name'] == 'AITCb2015a_005216.csv',]
egnnd2<-data.frame(egnnd$x_center, egnnd$y_center)
chickndout<-nndist(egnnd2)

}

hist(allimages$probability_of_adult)
