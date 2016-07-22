

install.packages("jpeg", dependencies=TRUE)
install.packages("moments", dependencies=TRUE)
install.packages("tools", dependencies=TRUE)

rm(list=ls(all=TRUE))

library(jpeg)
library(moments)
library(tools)


path = "C:/Users/Tom/Pictures/South 2015/Camera data to check/SPIGacop/"
maskpath = "G:/Cameras 2015/aYALOa2015a"

files <- list.files(path)

#if necessary, remove the csv files in this folder.
files<-files[9:length(files)]
files<-files[1:8503]

files2<-gsub(".JPG", ".jpg", files)

#setwd("C:/Users/Tom/Dropbox/Kittiwake remote camera/02-07-2013/")
j<-1

nestmask<-c(nestmask1, nestmask2)
mask1<-readJPEG(paste(maskpath, [j], sep=""), native = FALSE)

mask1<-readJPEG("C:/Users/Tom/Dropbox/Kittiwake remote camera/masknest1.JPG")
mask<-mask1[, , 2]
#should the mask actually be 1, NA?
#if so, this will speed computing.
kittiwakeeg<-readJPEG("C:/Users/Tom/Dropbox/Kittiwake remote camera/02-07-2013/IMAG1643.JPG")
rkitti<-as.matrix(kittiwakeeg[, , 3])

masked<-as.matrix(rkitti*mask)
#when you read in, cut out all but the relevant polygon from the image.

rmatrix<-mask1[, , 3]
results = data.frame(name=as.character(), redmean=as.numeric(), redkurtosis=as.numeric(), redskew=as.numeric(), redsd=as.numeric(), greenmean=as.numeric(), greenkurtosis=as.numeric(), greenskew=as.numeric(), greensd=as.numeric(), bluemean=as.numeric(), bluekurtosis=as.numeric(), blueskew=as.numeric(), bluesd=as.numeric())

#take the mean, skew, kurtosis, sd

i<-10


for (i in 1:length(files2)) {
  if (tolower(file_ext(files2[i])) == 'jpg') {
    paste(path, files2[i])
    sali1<-readJPEG(paste(path,files2[i], sep=""))
    d<-dim(sali1)
    v<-d[1]
    h<-d[2]
  #  file_names <- dir("C:/Users/Tom/Dropbox/Kittiwake remote camera/01-07-2013/aldkitt_", pattern = "^stat[[:digit:]]+_pwg[[:digit:]]+\\.JPG$")
  #sali1<-readJPEG(paste(path, files[i]))
    mean_r<-mean(sali1[1:v, 1:h, 1])
    kr1<-sali1[1:v, 1:h, 1]
    kr2<-as.vector(kr1)
    kurtosis_r<-kurtosis(kr2)
    skew_r<-skewness(kr2)
    redsd<-sd(kr2)
    
    mean_g<-mean(sali1[1:v, 1:h, 2])
    kg1<-sali1[1:v, 1:h, 2]
    kg2<-as.vector(kg1)
    kurtosis_g<-kurtosis(kg2)
    skew_g<-skewness(kg2)
    greensd<-sd(kg2)
    
    mean_b<-mean(sali1[1:v, 1:h, 3])
    kb1<-sali1[1:v, 1:h, 3]
    kb2<-as.vector(kb1)
    kurtosis_b<-kurtosis(kb2)
    skew_b<-skewness(kb2)
    bluesd<-sd(kb2)
    
    #plot(sali1)
    cat(files[i], "\n")
    results <- rbind(results, data.frame(name=files[i], redmean=mean_r, redkurtosis=kurtosis_r, redskew=skew_r, greenmean=mean_g, bluemean=mean_b, redkurt=kurtosis_r, greenkurt=kurtosis_g, bluekurt=kurtosis_b))
  }
}


name=as.character(), redmean=as.numeric(), redkurtosis=as.numeric(), redskew=as.numeric(), redsd(), greenmean=as.numeric(), greenkurtosis=as.numeric(), greenskew=as.numeric(), greensd(), bluemean=as.numeric(), bluekurtosis=as.numeric(), blueskew=as.numeric(), bluesd())


head(results)

write.table(results, "C:/Users/Tom/Documents/Penguin image analysis/RGB paper/SPIGbrgbout.csv", row.names=F, sep=",")

            
            )
k<-sali1[1:1536, 1:2048, 1]
k<-as.vector(k)
kurtosis(k)

kurtosis(sali1[1:1536, 1:2048, 1], na.rm = FALSE)
require(reshape2)
require(ggplot2)
melted <- melt(results, "name")
ggplot(melted, aes(x=name, y=value, group=variable, color=variable)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


