

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




# Create empty data frame -------------------------------------------------


results <- data.frame(name = as.character(), red_mean = as.numeric(), red_sd = as.numeric(),
                      red_skew = as.numeric(), red_kurtosis = as.numeric(), 
                      green_mean = as.numeric(), green_sd = as.numeric(), green_skew = as.numeric(),
                      green_kurtosis = as.numeric(), blue_mean = as.numeric(), blue_sd = as.numeric(),
                      blue_skew = as.numeric(), blue_kurtosis = as.numeric())





# Loop for mean, sd, skew, kurtosis ---------------------------------------


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
    #  file_names <- dir("C:/Users/Tom/Dropbox/Kittiwake remote camera/01-07-2013/aldkitt_", pattern = "^stat[[:digit:]]+_pwg[[:digit:]]+\\.JPG$")
    
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



head(results)



m_results <- melt(results, 'name')
ggplot(m_results, aes(x = name, y = value, group = variable, color = variable)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#write.table(results, "C:/Users/Tom/Documents/Penguin image analysis/RGB paper/SPIGbrgbout.csv", row.names=F, sep=",")



#don't know what this bit is...
k <- sali1[1:1536, 1:2048, 1]
k <- as.vector(k)
kurtosis(k)

kurtosis(sali1[1:1536, 1:2048, 1], na.rm = FALSE)



