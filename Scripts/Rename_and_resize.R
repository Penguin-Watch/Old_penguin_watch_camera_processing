#thanks to baptiste from stack overflow
#http://stackoverflow.com/users/471093/baptiste
#Tom Hart, with original code from Robin Freeman

rm(list=ls())

library(jpeg)
library(grid)


#test

#rm(list=ls())
#cameras<-c("LOCKa2016a", "LOCKa2016b", "LOCKb2016a", "LOCKb2016b")

#for (j in 1:length(cameras)){
#cameraname<-cameras[j]

cameraname<-"YALOa2016a"
path<-"E:/Cameras 2016/"
#step 1: create a new directory to backup images and to put the zooniverse reduced images into.
renamed<-paste(path, "Renamed images/", cameraname, "/", sep = "")
dir.create(path=renamed, showWarnings = TRUE, recursive = FALSE, mode = "0777")

zooniversed<-paste(path, "Zooniverse images/", cameraname, "/", sep = "")
dir.create(path=zooniversed, showWarnings = TRUE, recursive = FALSE, mode = "0777")

timelapse<-paste(path, "Timelapse images/", cameraname, "/", sep = "")
dir.create(path=timelapse, showWarnings = TRUE, recursive = FALSE, mode = "0777")

origindir<-paste(path, "Raw images/", cameraname, "/", sep = "")
copydir <-paste(path, "Renamed images/", cameraname, "/", sep = "")
zoodir<-paste(path, "Zooniverse images/", cameraname, "/", sep = "")

#create a list of files to copy and copy them across to copydir

filestocopy <- list.files(origindir)
#optional remove stuff
#filestocopy<-filestocopy[2:788]

movelist <- paste(origindir, filestocopy, sep='')
file.copy(from=movelist, to=copydir, copy.mode = TRUE)

#step 2 run the rename script, which should automatically rename to the camera unique id.
files<-list.files(copydir)
l<-length(files)

a <- paste(copydir, files, sep='')
nameslist<-paste0(cameraname, sprintf("_%06d", 1:l, sep = ""))
#nameslist<-gsub("\\s+","",nameslist)
b<-paste(copydir, nameslist, ".JPG", sep = "") 
file.rename(a, b)

##resize each of these images.
c<-paste(zoodir, nameslist, ".JPG", sep = "") 


for(i in 1:length(nameslist)) {
currpic<-readJPEG(paste(b[i]), native=TRUE)

#currpic<-readJPEG(paste(b[10]), native=TRUE)
dims<-dim(currpic)          ## calculate the dimensions of currpic      
aspect<-dims[1]/dims[2]     ## calculate the aspect ratio of currpic

## if the aspect ratio is 0.75 (as usual), proceed with resize to 1000x750. 
## if the aspect ratio is 0.5625, send a notification, then resize to 1000x562.5
## if the aspect ratio does not equal either, print an error message.

if(aspect == 0.75) {
  
  dw<-1000
  dh<-750
  
  jpeg(paste(c[i]), width = dw, height = dh) 
  grid.raster(currpic, width=unit(1,"npc"), height=unit(1,"npc"))
  dev.off() 
  
} else if (aspect == 0.5625) {
  
  dw<-1000
  dh<-562.5
  
  jpeg(paste(c[i]), width = dw, height = dh) 
  grid.raster(currpic, width=unit(1,"npc"), height=unit(1,"npc"))
  dev.off() 
  
} else {
  
  print("error - unrecognised aspect ratio")
  
}

}


alarm()

#extract the metadata from each image
workdir<-paste(path, "Renamed images/", cameraname, "/", sep = "")
setwd(workdir)
expath<-paste(path, "Renamed images/", cameraname, "/", sep = "")

#read in the EXIF data file for processing and renaming images
#paste(path, cameraname, sep="")

#try to add camera name to data out
my_cmd <- sprintf(paste0("C:\\exiftool\\exiftool.exe -T \"%s*\" >", cameraname, "data.txt"), paste(expath, cameraname, sep=""))
shell(my_cmd)

datafile<-read.table(paste0(cameraname, "data.txt"), sep="\t")
head(datafile)



datafile2<-NULL
datafile2$imageid<-datafile$V2
datafile2$datetime<-datafile$V20
datafile2<-as.data.frame(datafile2)
datafile2$moon<-datafile$V30
datafile2$tempf<-datafile$V31
datafile2$tempc<-datafile$V32

levels(datafile2$moon)[levels(datafile2$moon)=="Full"] <- "full"
levels(datafile2$moon)[levels(datafile2$moon)=="New"] <- "new"
levels(datafile2$moon)[levels(datafile2$moon)=="New Crescent"] <- "newcres"
levels(datafile2$moon)[levels(datafile2$moon)=="First Quarter"] <- "firstq"
levels(datafile2$moon)[levels(datafile2$moon)=="Waxing Gibbous"] <- "waxinggib"
levels(datafile2$moon)[levels(datafile2$moon)=="Waning Gibbous"] <- "waninggib"
levels(datafile2$moon)[levels(datafile2$moon)=="Last Quarter"] <- "lastq"
levels(datafile2$moon)[levels(datafile2$moon)=="Old Crescent"] <- "oldcres"

#convert f to c
#Deduct 32, then multiply by 5, then divide by 9
#refdata<-head(datafile)
datafile2<-data.frame(datafile2)
head(datafile2)

savepath<-paste(zooniversed, cameraname, " zoo.csv", sep = "")

write.table(datafile2, savepath, sep=",", col.names=TRUE, row.names=FALSE)

#nameslist<-paste(cameraname, sprintf("_%06d", 1:l, sep = ""))
#nameslist<-gsub("\\s+","",nameslist)
#gsub("([ab])", "\\1_\\1_", "abc and ABC")

##automatically select midday images
midday<-grep(pattern="12:00:00", datafile2$datetime)
b<-paste(copydir, nameslist, ".JPG", sep = "") 
middayimages<-b[midday]
middaymove<-gsub(pattern="Zooniverse images/", replacement="Timelapse images/" , x=middayimages)

file.copy(from=middayimages, to=timelapse, copy.mode = TRUE)


rm(a)
rm(b)
rm(c)
rm(datafile)
rm(l)
rm(files)
rm(my_cmd)
rm(path)
rm(cameraname)
rm(copydir)
rm(currpic)
rm(datafile2)
rm(expath)
rm(filestocopy)
rm(midday)
rm(middayimages)
rm(middaymove)
rm(movelist)
rm(nameslist)
rm(origindir)
rm(renamed)
rm(savepath)
rm(timelapse)
rm(workdir)
rm(zoodir)  
rm(zooniversed)

