##################
#To check consensus against actual image
#
#check for appropriate probability and accuracy of consensus algorithm
##################




# Packages and WD ---------------------------------------------------------


if('pacman' %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pacman)
}

pacman::p_load(jpeg, MASS, SDMTools, parallel, deldir, sp, dplyr, data.table)



osx <- '/Users/caseyyoungflesh/Google Drive/R/pwatch/'
win <- 'C:/Users/Lynch Lab 7/Google Drive/R/pwatch/'

if(Sys.info()[['sysname']] == 'Windows')
{
  dir <- win
}
if(Sys.info()[['sysname']] == 'Darwin')
{
  dir <- osx
}







# Functions ---------------------------------------------------------------


#functions to plot points and images
#to plot camera images
plot_jpeg = function(path, add=FALSE)
{
  par(mar=  c(0,0,0,0))
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[1:2] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[2]),ylim=c(1,res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[2],res[1])
  par(mar= c(5, 4, 4, 2))
}

cam_trans <- function(input, INVERT = FALSE)
{
  
  temp_x <- input[,1] * 2.048
  
  if(INVERT == TRUE)
  {
    temp_y <- 1536 - (input[,2] * 2.048)
  }else{
    temp_y <- input[,2] * 2.048
  }
  
  OUT <- cbind(temp_x, temp_y)
  return(OUT)
}



#instead of 1000 x 750 images are 1920 x 1080?????
head(test)


#consensus click input basically in 1000 x 750
hist(test[,2])
range(test[,2], na.rm=TRUE)
hist(test[,3])
range(test[,3], na.rm=TRUE)




# Load/process data -------------------------------------------------------


#for clicks
setwd(paste0(dir, 'Data'))
test <- read.csv('ADPE/YALOazooconc.csv')

click_images <- unique(test$name)

#for images
setwd(paste0(dir, 'Images/YALOa'))
images <- list.files()


for (i in 1:length(click_images))
{
  #i <- 1
  

  #filter for just clicks from one image name
  clicks <- filter(test, name == paste0(click_images[i]))
  #transform clicks to 2048 x 1536 to plot
  trans_clicks <- cam_trans(clicks[,2:3], INVERT = TRUE)
  
  #name of image with clicks without extension
  cl_name <- base::strsplit(toString(click_images[i]), split='[.]')[[1]][1]
  
  #print name of image in console
  print(cl_name)
  
  #position in image vector
  pos_img <- grep(cl_name[1], images)
  
  #plot image
  plot_jpeg(images[pos_img])
  #plot consensus clicks on image
  points(trans_clicks, col='green', pch=19)
  
  #stop point to go to next image
  readline(prompt="Press [enter] to continue")

}



