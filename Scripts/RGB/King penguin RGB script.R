###############
#Script to calculate mean, sd, skew, kurtosis of King Penguin images
#
#GOLDa 2016a as a test data set for the code
###############

#need to filter out night images and snow images
#above or below a certain threshold --> need to remove
#less light is a problem in winter months


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





# Function to calc mean, sd, skew, kurtosis -------------------------------


#PATH is folder with images
#MASK is path to mask - MASK should be black where not applicable, white in areas of interest
#WHICH specifies which images - ALL = all images in folder, otherwise specify (e.g., 1:100)

img_fun <- function(PATH, MASK = NULL, WHICH = 'ALL')
{

  #PATH <- paste0(dir, 'Images/GOLDa2016a')
  #MASK <- paste0('/Users/caseyyoungflesh/Google Drive/R/pwatch/Images/mask_test.jpg')
  #MASK <- NULL
  #WHICH <- 1:100
  
  
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
  results <- matrix(nrow = length(images), ncol= 12)
  name_vec <- rep(NA, length(images))
  
  #progress bar
  pb <- txtProgressBar(min = 1, max = length(images), style = 3)

  #loop to calc mean, sd, skew, kurtosis
  for (i in 1:length(images)) 
  {

    #i <- 1
    
    #check to make sure jpg
    if (tolower(file_ext(images[i])) == 'jpg') 
    {

      #read in JPEG
      temp_img <- readJPEG(images[i])


      #apply mask if applicable
      #all 1s RGB is white, all 0s is black
      if (is.null(MASK))
      {
        temp_jpeg <- temp_img
      }else{
        mask <- readJPEG(MASK)
        temp_jpeg <- as.array(temp_img*mask)
      }

      
      d <- dim(temp_jpeg)
      vert <- d[1]
      horiz <- d[2]


      #name of image in first column of empty matrix
      name_vec[i] <- images[i]

      
      #RED
      temp_r1 <- as.vector(temp_jpeg[1:vert, 1:horiz, 1])
      results[i,1] <- mean(temp_r1)
      results[i,2] <- sd(temp_r1)
      results[i,3] <- skewness(temp_r1)
      results[i,4] <- kurtosis(temp_r1)


      #GREEN
      temp_g1 <- as.vector(temp_jpeg[1:vert, 1:horiz, 2])
      results[i,5] <- mean(temp_g1)
      results[i,6] <- sd(temp_g1)
      results[i,7] <- skewness(temp_g1)
      results[i,8] <- kurtosis(temp_g1)


      #BLUE
      temp_b1 <- as.vector(temp_jpeg[1:vert, 1:horiz, 3])
      results[i,9] <- mean(temp_b1)
      results[i,10] <- sd(temp_b1)
      results[i,11] <- skewness(temp_b1)
      results[i,12] <- kurtosis(temp_b1)

    }

    setTxtProgressBar(pb, i)
  }
  close(pb)
  

  OUT <- data.frame(name = name_vec, 
                    red_mean = results[,1], red_sd = results[,2], 
                    red_skew = results[,3], red_kurtosis = results[,4], 
                    green_mean = results[,5], green_sd = results[,6], 
                    green_skew = results[,7], green_kurtosis = results[,8], 
                    blue_mean = results[,9], blue_sd = results[,10], 
                    blue_skew = results[,11], blue_kurtosis = results[,12])
  
  
  return(OUT)
}




# Run function ------------------------------------------------------------

path <- paste0(dir, 'Images/GOLDa2016a')
mask <- paste0('/Users/caseyyoungflesh/Google Drive/R/pwatch/Scripts/RGB/mask_test.jpg') 

#52 minutes processing time
ptm <- proc.time()
img_results <- img_fun(PATH = path, WHICH = 'ALL')
proc.time() - ptm

#slightly longer with masking
ptm <- proc.time()
img_results <- img_fun(PATH = path, MASK = mask, WHICH = 'ALL')
proc.time() - ptm



# Write results to file ---------------------------------------------------

setwd(paste0(dir, 'Output'))

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



