###############
#Script to calculate mean, sd, skew, kurtosis of King Penguin images
#
#Input path with images
#Output metrics for each image
###############

#TO DOS
#pick out dark and snowy images
#find where change point happens in time series

#In order to:
#need to filter out night images and snow images
#above or below a certain threshold --> need to remove
#less light is a problem in winter months


# Install/load packages --------------------------------------------------------

if('pacman' %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pacman)
}

pacman::p_load(jpeg, moments, tools, ggplot2, reshape2, parallel)




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
#CORES is number of cores if want to run parallelized
#NOTE: no progress bar when parallelized; CANNOT RUN IN PARALLEL IN WINDOWS

img_fun <- function(PATH, MASK = NULL, WHICH = 'ALL', CORES = 1)
{

  #PATH <- paste0(dir, 'Images/GOLDa2016a')
  #MASK <- paste0('/Users/caseyyoungflesh/Google Drive/R/pwatch/Images/mask_test.jpg')
  #MASK <- NULL
  #WHICH <- 1:100
  
  ncore <- detectCores()
  
  if(CORES > ncore)
  {
    stop("You don't have that many cores on your machine, dummy!")
  }
  
  
  #set wd
  setwd(paste0(PATH))

  if(WHICH[1] == 'ALL')
  {
    #files in target location
    images <- list.files()
  }else{
    images <- list.files()[WHICH]
  }

  
  
  #function to calculate metrics - needed for mclapply
  int_fun <- function(IMG_IN)
  {
    
    tMASK <- MASK
    
    #create empty matrix
    results <- matrix(nrow = length(IMG_IN), ncol= 12)
    name_vec <- rep(NA, length(IMG_IN))
  
    #progress bar
    pb <- txtProgressBar(min = 1, max = length(IMG_IN), style = 3)

    #loop to calc mean, sd, skew, kurtosis
    for (i in 1:length(IMG_IN)) 
    {

      #i <- 1
    
      #check to make sure jpg
      if (tolower(file_ext(IMG_IN[i])) == 'jpg') 
      {

        #read in JPEG
        temp_img <- readJPEG(IMG_IN[i])


        #apply mask if applicable
        #all 1s RGB is white, all 0s is black
        if (is.null(tMASK))
        {
          temp_jpeg <- temp_img
        }else{
          mask <- readJPEG(tMASK)
          temp_jpeg <- as.array(temp_img*mask)
          #make sure this is transformed to NA
        }

      
        d <- dim(temp_jpeg)
        vert <- d[1]
        horiz <- d[2]


        #name of image in first column of empty matrix
        name_vec[i] <- IMG_IN[i]

      
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
  
  
  
  #if cores is 1, run in serial
  #if > 1, run using mclappy - doesn't have progres bar when parallelized
  if (CORES == 1)
  {
    
    M_OUT <- int_fun(images)
    
  }else{
    
    #length of sements
    l_segs <- ceiling(length(images)/CORES)
    
    sp <- seq(from= 1, to= length(images), by= l_segs)
    
    #add first sequence to list
    n_ol <- images[sp[1]:sp[2]]
    TL <- list(n_ol)
    #run loop for rest of sequences in list
    for (j in 2:CORES)
    {
      #j <- 3
      if(j < CORES)
      {
        n_temp <- images[(sp[j]+1):sp[j+1]]
      }else{
        #needed if number of images not evenly divisable by CORES
        n_temp <- images[(sp[j]+1):length(images)]
      }
      TL <- c(TL, list(n_temp))
    }
  
    #run function parallelized
    res <- mclapply(TL, FUN = int_fun)
    
    #bind results from different cores together
    M_OUT <- do.call(rbind, res)
    
  }
  
  
  return(M_OUT)
}





# Run function ------------------------------------------------------------


#SALIa2015
path_SALIa2015 <- paste0(dir, 'Images/King_imagery/SALIa2015')

ptm <- proc.time()
SALIa2015 <- img_fun(PATH = path_SALIa2015, WHICH = 'ALL', CORES = 2)
proc.time() - ptm


#SALIa2016a
path_SALIa2016a <- paste0(dir, 'Images/King_imagery/SALIa2016a')

ptm <- proc.time()
SALIa2016a <- img_fun(PATH = path_SALIa2016a, WHICH = 'ALL', CORES = 2)
proc.time() - ptm


#SALIb2015
path_SALIb2015 <- paste0(dir, 'Images/King_imagery/SALIb2015')

ptm <- proc.time()
SALIb2015 <- img_fun(PATH = path_SALIb2015, WHICH = 'ALL', CORES = 2)
proc.time() - ptm



# Write results to file ---------------------------------------------------

setwd(paste0(dir, 'Output'))

write.table(SALIa2015, "SALIa2015_RGB.csv", row.names = FALSE, sep = ",")
write.table(SALIa2016a, "SALIa2016a_RGB.csv", row.names = FALSE, sep = ",")
write.table(SALIb2015, "SALIb2015_RGB.csv", row.names = FALSE, sep = ",")




#NOTES ON IMAGERY

#SALIa2015
#img_results <- SALIa2015
#black images

#night white
#1206
#1214
#1222
#1230
#1238 (x8)
#1325, 1326
#1390, 1406, 1510 #particularly good

#SUN images
#1479
#1511
#1519
#1527

#stormy
#1583-1589


#transition
#000001 - chicks
#000626 - no chicks
#001050 - start of chicks




#filter images only from 10AM-2PM

to.rm <- grep('[(]', SALIa2015[,1])
n_SALIa2015 <- SALIa2015[-to.rm,]

len <- nrow(n_SALIa2015) #number of images
vec <- seq(from= 1, to= len, by= 8)

OUT <- c()
for (i in 0:2)
{
  #i <- 3
  OUT <- c(OUT, vec+i)
}

#only images from 10AM-2PM
f_img <- n_SALIa2015[sort(OUT),]






#img_results <- f_img
#img_results <- SALIb2015

# Process metrics -----------------------------------------------------

#standardize 
st_results <- apply(img_results[,-1], 2, function(X){scale(X, scale = TRUE)})


#moving avg function
ma_fun <- function(x, n= 5, SIDES= 2)
{
  OUT <- stats::filter(x, rep(1/n, n), sides= SIDES)
  
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



