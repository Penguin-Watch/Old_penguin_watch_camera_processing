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



# Set up  -----------------------------------------------------------------

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

#Mask
#to mask
#masked <- as.matrix(img*mask)

rmatrix<-mask1[, , 3]

#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#






# Create empty data frame -------------------------------------------------


results <- data.frame(name = as.character(), red_mean = as.numeric(), red_sd = as.numeric(),
                      red_skew = as.numeric(), red_kurtosis = as.numeric(), 
                      green_mean = as.numeric(), green_sd = as.numeric(), green_skew = as.numeric(),
                      green_kurtosis = as.numeric(), blue_mean = as.numeric(), blue_sd = as.numeric(),
                      blue_skew = as.numeric(), blue_kurtosis = as.numeric())





# Loop for mean, sd, skew, kurtosis of each image ---------------------------------------


pb <- txtProgressBar(min = 1, max = length(images), style = 3)
for (i in 1:length(images)) 
{
  #i <- 1  
  
  if (tolower(file_ext(images[i])) == 'jpg') 
  {
    #paste name of image being processed
    #paste(images[i])
    
    #read in JPEG
    temp_jpeg <- readJPEG(images[i])
    
    d <- dim(temp_jpeg)
    vert <- d[1]
    horiz <- d[2]
    
    #don't know what the next line of code does
    #file_names <- dir("C:/Users/Tom/Dropbox/Kittiwake remote camera/01-07-2013/aldkitt_", pattern = "^stat[[:digit:]]+_pwg[[:digit:]]+\\.JPG$")
    
    #RED
    temp_r1 <- temp_jpeg[1:vert, 1:horiz, 1]
    temp_r2 <- as.vector(temp_r1)
    mean_r <- mean(temp_r2)
    sd_r <- sd(temp_r2)
    skew_r <- skewness(temp_r2)
    kurtosis_r <- kurtosis(temp_r2)
    
    
    #GREEN
    temp_g1 <- temp_jpeg[1:vert, 1:horiz, 2]
    temp_g2 <- as.vector(temp_g1)
    mean_g <- mean(temp_g2)
    sd_g <- sd(temp_g2)
    skew_g <- skewness(temp_g2)
    kurtosis_g <- kurtosis(temp_g2)
    
    
    #BLUE
    temp_b1 <- temp_jpeg[1:vert, 1:horiz, 3]
    temp_b2 <- as.vector(temp_b1)
    mean_b <- mean(temp_b2)
    sd_b <- sd(temp_b2)
    skew_b <- skewness(temp_b2)
    kurtosis_b <- kurtosis(temp_b2)
    
    
    #plot(temp_jpeg)
    #cat(files[i], "\n")
    
    results <- rbind(results, data.frame(name = images[i], 
                                         red_mean = mean_r, red_sd = sd_r, red_skew = skew_r, 
                                         red_kurtosis = kurtosis_r, green_mean = mean_g, 
                                         green_sd = sd_g, green_skew = skew_g,
                                         green_kurtosis = kurtosis_g, blue_mean = mean_b, 
                                         blue_sd = sd_b, blue_skew = skew_b, 
                                         blue_kurtosis = kurtosis_b))
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)




# Write results to file ---------------------------------------------------

setwd(paste0(dir, 'Scripts/RGB'))

write.table(results, "King_RGB.csv", row.names = FALSE, sep = ",")



# Standardize metrics -----------------------------------------------------

st_results <- apply(results[,-1], 2, function(X){scale(X, scale = FALSE)})

st_res_df <- data.frame(name = results[,1], st_results)




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

#need to apply smoothing window and determine when derivative is maximized



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



