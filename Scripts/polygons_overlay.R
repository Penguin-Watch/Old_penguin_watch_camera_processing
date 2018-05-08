###Script for dividing an image into individual nests, and calculating how many chicks are in each
###particular nest over time.


###Plotting polygons over JPEGs 


#1) input nest locations

#2) tesselate using nest locations

#3) arrange data in time series

#4) determine which polygon (i.e., nest zone) the points are in using the tesselation polygons (static)

#5) aggregate to create time series



###Clear the working environment

rm(list=ls())

###Install the required packages.

#install.packages("deldir", dependencies = TRUE)
#install.packages("SDMTools", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
##install.packages("tidyr", dependencies = TRUE)
##install.packages("jpeg", dependencies = TRUE)


### Load in the required libraries.

require(deldir)
require(SDMTools)
require(dplyr)
require(tidyr)
library(data.table)
library(jpeg)
library(stringr)


###INPUT NEST LOCATION DATA using the rows below OR upload csv file

#data <- read.csv("C:/Users/lady3793/Dropbox/Penguin_DPhil/Survival_paper/LOCKb2014/LOCKb2014_nestcoords.csv", header = TRUE, sep = ",")
#data_user <- read.csv("C:/Users/lady3793/Dropbox/Penguin_DPhil/Survival_paper/Filtered_clusters/LOCKb_filtered2.csv", header = TRUE, sep = ",")

dir <- '~/Google_Drive/R/Project_archive/Old_penguin_watch_camera_processing/Scripts/'
#NEST COORDINATES
nest_coords_p <- read.csv(paste0(dir, 'LOCKb2014_nestcoords.csv'))
#CONSENSUS CLICKS
consensus_p <- read.csv(paste0(dir, 'LOCKb2014_data_user.csv'))
#IMAGE DIR
path <- paste0(dir, 'LOCKb2014b_jpeg/')

#transform y coordinates as image origin is top left, rather than bottom left
nest_coords <- data.frame(x = nest_coords_p$x, 
                          y = 750 - nest_coords_p$y)
consensus <- data.frame(name = consensus_p$name, 
                        x = consensus_p$x, 
                        y = 750 - consensus_p$y)


#Tests each click location to determine which nest it is located in

#function to tesselate
#INPUT IS MATRIX WITH TWO COLUMNS (col 1 is x coordinates, col 2 is y coordinates of nests)

poly_fun <- function(KM_REV_ORTHO)
{
  #KM_REV_ORTHO <- km_rev_ortho
  width <- 1000
  height <- 750
  
  #Voronoi tesselation using specified nest sites - deldir makes tesselation
  vt <- suppressWarnings(deldir(KM_REV_ORTHO[,1], KM_REV_ORTHO[,2], 
                                rw= c(0, width, 0, height)))
  
  w <- tile.list(vt) #polygon coordinates
  
  polys <- vector(mode= 'list', length= length(w))
  for (i in seq(along= polys))
  {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- pcrds #arrange polygon coordinates
  }
  return(polys)
}

#determine polygons from nest coordinates
polys <- poly_fun(nest_coords)

#change wd to dir with jpg images
setwd(path) 
jpeg_files <- list.files()

#ggplot colors function
gg_color_hue <- function(n, OUT = 'HEX')
{
  #n = 6
  hues = seq(15, 375, length=n+1)
  tmp <- hcl(h=hues, l=65, c=100)[1:n]
  gg_cols <- col2rgb(tmp)/255
  
  if (OUT == 'HEX')
  {
    return(tmp)
  }
  if (OUT == 'RGB')
  {
    return(gg_cols)
  }
  if (OUT != 'HEX' & OUT != 'RGB')
  {
    stop('"OUT" argument must be "HEX" or "RGB"')
  }
}

#plot jpeg function
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

#determine colors to use (as many as there are nests)
cols <- gg_color_hue(NROW(nest_coords))

#vector of numbers to plot for nests
let <- str_pad(1:99, 2, pad = '0')

#set dir to out location for jpeg files
#loop to plot camera image with nest 'zones', nest numbers, and consensus chick clicks
for (i in 1:length(jpeg_files))
{
  #create jpeg
  jpeg(filename = paste0('../Output_jpeg/pts_', jpeg_files[i]), width = 1000, height = 750)
  #plot jpeg camera image
  plot_jpeg(jpeg_files[i])
  #filter for consensus clicks from a single image
  jpg_name <- strsplit(jpeg_files[i], split = '.', fixed = TRUE)[[1]][1]
  
  filt_clicks <- filter(consensus, name == jpg_name)
  for (j in 1:length(polys))
  {
    #plot polygons
    lines(polys[[j]], lwd = 5)
    #nests numbers on plot
    text(nest_coords$x[j], nest_coords$y[j], labels = let[j], col = cols[j], lwd = 12)
  }
  #consensus clicks
  points(filt_clicks$x, filt_clicks$y, pch = 19, col = 'red', lwd = 4)
  dev.off()
}






#PLOT DATA

plot(data, pch = 19)


#PLOT POLYGONS FROM TESSELATION

for (i in 1: length(polys))
  
{
  
  lines(polys[[i]])
  
}


## Plot volunteer consensus clicks over the top, for error-checking

##points(data_user$x, data_user$y, pch = "19")


# Point in poly -----------------------------------------------------------













#input is arg 1: polygons (output from previous function), arg 2: consensus clicks

point_fun <- function(POLY, ORDER_OUT)
  
{
  
  POLY <- polys
  
  ORDER_OUT <- data_user
  
  
  
  #CHANGED TO ORDER_OUT[,1], ORDER_OUT[,2] from ORDER_OUT[,2], ORDER_OUT[,3] because col 1 was image name (used in function below)
  
  DATA <- cbind(ORDER_OUT[,2], ORDER_OUT[,3])
  
  
  
  #determine which points are in which polygons
  
  out <- c()
  
  temp.names <- c()
  
  for (j in 1:length(POLY))
    
  {
    
    #j <- 1
    
    temp <- pnt.in.poly(DATA, POLY[[j]])$pip
    
    out <- cbind(out, temp)
    
    temp.names[j] <- paste0("nest",j)  
    
  }
  
  
  
  #create data frame
  
  colnames(out) <- temp.names
  
  t_series <- data.frame(ORDER_OUT, out)
  
  
  
  return(t_series)
  
}



#RUN FUNCTION WITH USER DATA

points_nests <- point_fun(polys, data_user)


# Count the number of chicks in each nest per image and write to a data table

imageid <- data.table(points_nests)

pingu <- imageid[, lapply(.SD, sum), by = imageid$name]

counts_table <- pingu[, c("x","y"):=NULL]

write.csv(counts_table, "C:/Users/lady3793/Dropbox/Penguin_DPhil/Survival_paper/ORNEa2014/ORNEa2014_V2_chicks_nest.csv", row.names = FALSE)


### Metadata can be added using 'merge'(use image id).






