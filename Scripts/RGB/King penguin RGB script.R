



if('pacman' %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pacman)
}

pacman::p_load(jpeg, moments, tools)



# Clear environment -------------------------------------------------------

rm(list = ls())
dev.off()



# Set WD ------------------------------------------------------------------

osx <- '/Users/caseyyoungflesh/Google Drive/R/pwatch/'

if(Sys.info()[['sysname']] == 'Windows')
{
  dir <- win
}
if(Sys.info()[['sysname']] == 'Darwin')
{
  dir <- osx
}


setwd(paste0(dir, 'Images/GOLDa2016a'))

images <- list.files()

#not needed
#files2<-gsub(".JPG", ".jpg", files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#|
#V
#Mask

j<-1
nestmask<-c(nestmask1, nestmask2)
mask1<-readJPEG(paste(maskpath[j], sep=""), native = FALSE)

mask1<-readJPEG("C:/Users/Tom/Dropbox/Kittiwake remote camera/masknest1.JPG")
mask<-mask1[, , 2]
#should the mask actually be 1, NA?
#if so, this will speed computing.

#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

img_jpeg <- readJPEG(images[1])
img_ch3 <- as.matrix(img_jpeg[, , 3])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#|
#V
#Mask
#to mask
#masked <- as.matrix(img*mask)

rmatrix<-mask1[, , 3]
#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



results <- data.frame(name = as.character(), red_mean = as.numeric(), red_kurtosis = as.numeric(), 
                      red_skew = as.numeric(), red_sd = as.numeric(), green_mean = as.numeric(), 
                      green_kurtosis = as.numeric(), green_skew = as.numeric(), green_sd = as.numeric(),
                      blue_mean = as.numeric(), blue_kurtosis = as.numeric(), blue_skew = as.numeric(), 
                      blue_sd = as.numeric())


#take the mean, skew, kurtosis, sd
for (i in 1:length(images)) 
{
  i <- 1  
  
  
  if (tolower(file_ext(images[i])) == 'jpg') 
  {
    paste(path, files2[i])
    sali1 <- readJPEG(paste(path,files2[i], sep=""))
    d <- dim(sali1)
    v <- d[1]
    h <- d[2]
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


