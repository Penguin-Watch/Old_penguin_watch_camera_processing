#take zooniverse consensus data
#grep one site at a time
#calculate NND
#use that period of time in the methods_script


rm(list=ls())
ls()

### Load in the spatstat library

library(spatstat)

### Read in the data table and assign the site name
### Assign the column headers 

aitcb<-read.table("C:/Users/Fiona Jones/Dropbox/Penguin_DPhil/Penguins_Greg/AITCb2015zooconc.csv", sep=",", header=TRUE)
names(aitcb)<- c("penguin_index", "x_centre", "y_centre", "probability_of_adult", "probability_of_chick", "probability_of_egg", "probability_of_true_positive", "num_markings", "name")

### Assign the imageid

imageid<-levels(as.factor(aitcb$name))

adultndout<-NULL                  ## adult nearest neighbour
chickndout<-NULL                  ## chick nearest neighbour
chicksdndout<-NULL                ## chick std dev nearest neighbour

### Assuming a threshold probability level of 0.5


for(i in 1:length(imageid)){
#for(i in 1:100){
  
    x<-imageid[i]
    current<-aitcb[which(aitcb$name==paste(x, sep=",")),]
    currentadult<-current[which(current$probability_of_adult>0.5),]
    currentadultxy<-data.frame(currentadult$x_centre, currentadult$y_centre)
    currentchick<-current[which(current$probability_of_chick>0.5),]
    currentchickxy<-data.frame(currentchick$x_centre, currentchick$y_centre)
    
    adultndout[i]<-mean(nndist(currentadultxy))
    chickndout[i]<-mean(nndist(currentchickxy))
    chicksdndout[i]<-sd(nndist(currentchickxy))
    
    remove(currentadult)
    remove(currentchick)
}

plot(adultndout, type="l")
lines(chickndout, col="green")
lines(chicksdndout, col="blue")


plot(current$x_centre, 500-current$y_centre, pch=19)

#####

current$user_name<-factor(current$user_name)
nameslist<-paste(cameraname, sprintf("_%06d", 1:l, sep = ""))
nameslist<-gsub("\\s+","",nameslist)
b<-paste(copydir, nameslist, ".JPG", sep = "") 
