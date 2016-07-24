###############
#Script to calculate mean, sd, skew, kurtosis of King Penguin images
#
#GOLDa 2016a as a test data set for the code
###############




# Install/load packages --------------------------------------------------------

if('pacman' %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pacman)
}

pacman::p_load(jpeg, moments, tools, ggplot2, reshape2)




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

#Mask
#to mask
#masked <- as.matrix(img*mask)

rmatrix<-mask1[, , 3]

#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#





# Function to calc mean, sd, skew, kurtosis -------------------------------


#PATH is folder with images
#WHICH specifies which images - ALL = all images in folder, otherwise specify (e.g., 1:100)
img_fun <- function(PATH, WHICH = 'ALL')
{
  
  #set wd
  setwd(paste0(PATH))
  
  if(WHICH[1] == 'ALL')
  {
    #files in target location
    images <- list.files()
  }else{
    images <- list.files()[WHICH]
  }
  
  
  
  #create empty matrix
  results <- matrix(nrow = length(INPUT), ncol= 13)

  #progress bar
  pb <- txtProgressBar(min = 1, max = length(INPUT), style = 3)

  #loop to calc mean, sd, skew, kurtosis
  for (i in 1:length(INPUT)) 
  {

    #check to make sure jpg
    if (tolower(file_ext(INPUT[i])) == 'jpg') 
    {

      #read in JPEG
      temp_jpeg <- readJPEG(INPUT[i])

      d <- dim(temp_jpeg)
      vert <- d[1]
      horiz <- d[2]


      #name of image in first column of empty matrix
      results[i,1] <- INPUT[i]

      #RED
      temp_r1 <- temp_jpeg[1:vert, 1:horiz, 1]
      temp_r2 <- as.vector(temp_r1)
      results[i,2] <- mean(temp_r2)
      results[i,3] <- sd(temp_r2)
      results[i,4] <- skewness(temp_r2)
      results[i,5] <- kurtosis(temp_r2)


      #GREEN
      temp_g1 <- temp_jpeg[1:vert, 1:horiz, 2]
      temp_g2 <- as.vector(temp_g1)
      results[i,6] <- mean(temp_g2)
      results[i,7] <- sd(temp_g2)
      results[i,8] <- skewness(temp_g2)
      results[i,9] <- kurtosis(temp_g2)


      #BLUE
      temp_b1 <- temp_jpeg[1:vert, 1:horiz, 3]
      temp_b2 <- as.vector(temp_b1)
      results[i,10] <- mean(temp_b2)
      results[i,11] <- sd(temp_b2)
      results[i,12] <- skewness(temp_b2)
      results[i,13] <- kurtosis(temp_b2)

      #plot(temp_jpeg)
      #cat(files[i], "\n")

    }

    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  
  OUT <- data.frame(name = results[,1], 
                    red_mean = results[,2], red_sd = results[,3], 
                    red_skew = results[,4], red_kurtosis = results[,5], 
                    green_mean = results[,6], green_sd = results[,7], 
                    green_skew = results[,8], green_kurtosis = results[,9], 
                    blue_mean = results[,10], blue_sd = results[,11], 
                    blue_skew = results[,12], blue_kurtosis = results[,13])
  
  
  return(OUT)
}




# Run function ------------------------------------------------------------

path <- paste0(dir, 'Images/GOLDa2016a')

#52 minutes projected for all images
ptm <- proc.time()
img_results <- img_fun(PATH = path, WHICH = 1:100)
proc.time() - ptm




# Write results to file ---------------------------------------------------

setwd(paste0(dir, 'Scripts/RGB'))

#write.table(img_results, "King_RGB.csv", row.names = FALSE, sep = ",")

#read csv in
#img_results <- read.csv('King_RGB.csv')




# Process metrics -----------------------------------------------------

#standardize 
st_results <- apply(img_results[,-1], 2, function(X){scale(X, scale = TRUE)})


#moving avg function
ma_fun <- function(x, n= 5, SIDES= 2)
{
  OUT <- filter(x, rep(1/n, n), sides= SIDES)
  
  return(OUT)
}

#5 year moving average
ma_results <- apply(st_results, 2, function(X){ma_fun(X, n = 5)})

#make data frame with image names and standardized, moving average values
st_res_df <- data.frame(name = img_results[,1], ma_results)






# Plot summary metrics ----------------------------------------------------

#all metrics
m_st_results <- melt(st_res_df, 'name')

ggplot(m_st_results, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL red
m_st_red <- melt(st_res_df[,1:5], 'name')

ggplot(m_st_red, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL green
m_st_green <- melt(st_res_df[,c(1, 6:9)], 'name')

ggplot(m_st_green, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL blue
m_st_blue <- melt(st_res_df[,c(1, 10:13)], 'name')

ggplot(m_st_blue, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())



#ALL mean
m_st_mean <- melt(st_res_df[,c(1:2,6,10)], 'name')

ggplot(m_st_mean, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL sd
m_st_sd <- melt(st_res_df[,c(1,3,7,11)], 'name')

ggplot(m_st_sd, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL skew
m_st_skew <- melt(st_res_df[,c(1,4,8,12)], 'name')

ggplot(m_st_skew, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())


#ALL kurtosis
m_st_kurtosis <- melt(st_res_df[,c(1,5,9,13)], 'name')

ggplot(m_st_kurtosis, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line(alpha=0.5) + 
  theme(axis.text.x = element_blank())












# Plot jpeg function from pwatch script -----------------------------------

setwd(paste0(dir, 'Images/GOLDa2016a'))

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


i <- 2
plot_jpeg(images[i])



