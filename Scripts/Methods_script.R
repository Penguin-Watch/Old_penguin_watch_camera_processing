##################
#Example script for NEKOc 2013
#
#
#Code can be generalized into package
#
#Script uses consensus zooniverse clicks
#-Defines nests based on density thresholds
#-Creates time series for each nest
#
##################


#post-zooniverse:
#1) zoo out consensus data
#2) grep one site at a time
#3) calculate NND to determine correct time to look at nests
#4) subset point of interest to just that specified in Step 3
#5) orthorectify image (with scalar)
#6) density threshold
#7) filter for points only in high density areas
#8) click tool - human specifies nest with high density clicks overlaid on image
#9) run tesselation on nests specified in Step 7
#10) create time series for each nest using consensus data


#turn into function with 3 inputs (obliqueness, density threshold, # nests)

#methods script:
#as before
#remove all extraneous functions/plotting
#set up so that points that have been density thresholded are transformed to non-ortho to 
#...overlay on image for human nest detection

#long term script:
#change so that only points in high density areas are shown and then overlaid on plot
#develop tool so that user can then click on nests to specify location with high click density overlaid
#nest locations will be used to create tesselation and create time series for nests


# Load packages -----------------------------------------------------------

#installs packages if need and loads required packages

if('pacman' %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pacman)
}

pacman::p_load(jpeg, MASS, SDMTools, parallel, deldir, sp, dplyr)



# Clear environment -------------------------------------------------------

rm(list = ls())
dev.off()



# Set WD ------------------------------------------------------------------

osx <- '/Users/caseyyoungflesh/Google Drive/R/pwatch/'
win <- 'C:/Users/Lynch Lab 7/Google Drive/R/Camera trap - mark recapture/'

if(Sys.info()[['sysname']] == 'Windows')
{
  dir <- win
}
if(Sys.info()[['sysname']] == 'Darwin')
{
  dir <- osx
}



# Define functions --------------------------------------------------------

#to plot camera images
#2048 x 1536
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


#function to scale points from zooniverse dimensions (1000 x 750) to camera image dimension (2048 x 1536)
#zooniverse uses scaling factor of 2.048
#first column must be x coords, second column y coords
cam_trans <- function(input)
{
  temp_x <- input[,1] * 2.048
  temp_y <- 1536 - (input[,2] * 2.048)

  OUT <- cbind(temp_x, temp_y)
  return(OUT)
}




#TOP FUNCTION 
#function(DATA_IN, SITE, DEN, OBL)
#DATA_IN is zoo consensus
#SITE is site to grep
#DEN is density threshold
#OBL is obliqueness of camera (how much to ortho)

#grep -> NND -> grep -> ortho_fun -> den_fun


# Load/process data -------------------------------------------------------

#generalize to read in zoo consensus data
#grep one site at a time
#run NND analysis to determine appropriate time


setwd(paste0(dir, 'Data'))

#import NEKOc data
NEKO_con_data_imp <- read.csv('NEKOc_consensus.csv', header=TRUE) 


#just 2013
NEKO_con_data <- NEKO_con_data_imp[grep('NEKOc2013', NEKO_con_data_imp$path),c(1:5)]
colnames(NEKO_con_data) <- c('ID', 'ZOOID', 'path', 'x', 'y')



# Orthorectification ------------------------------------------------------

#due to oblique angle of camera, image needs to be altered to correctly determine click density
#essentially normalizing area


ortho_fun <- function(IN, OBL)
{
  #IN <- NEKO_con_data
  #OBL <- 150
  
  #remove erroneous clicks outside of defined region
  to_rm <- which(IN$x > 1000 | IN$x < 0 | IN$y < 0 | IN$y > 750)

  #remove erroneous clicks
  if(length(to_rm) > 0)
  {
    TEMP_x <- scale(IN$x[-to_rm], scale=FALSE)
    TEMP_y <- 750 - IN$y[-to_rm]
  }else{
    TEMP_x <- scale(IN$x, scale=FALSE)
    TEMP_y <- 750 - IN$y
  }

  #transform data
  x_val <- TEMP_x * (TEMP_y + OBL)
  y_val <- TEMP_y * (TEMP_y + OBL)
  
  SCALE <- attributes(TEMP_x)$'scaled:center'
  
  #out object
  OUT <- data.frame(x= x_val, y= y_val, x_scale = SCALE, OBL = OBL) #TEMP_x is used in reverse ortho function
  
  return(OUT)
}


post_ortho <- ortho_fun(NEKO_con_data, 0)
plot(post_ortho$x, post_ortho$y, pch='.')

plot(NEKO_con_data$x, -NEKO_con_data$y, pch='.')


#----------------------------------------------#
#Function to reverse transform for orthorectified to original
#does the reverse of above for given set of points

rev_orthro_fun <- function(IN, POST_ORTHO)
{
  
  #IN is df of points to be transformed - column 1 must be x, column 2 must be y
  #POST_ORTHO output from ortho_fun -> scaled x data used to find centers and reverse ortho
  
  #get centers of scaled data from ortho
  x_center <- POST_ORTHO$x_scale

  #points to be transformed
  XVAL <- IN[,1]
  YVAL <- IN[,2]
  
  #ortho adjuster used in original ortho
  OBL <- POST_ORTHO$OBL
  
  #quadratic equation to backsolve for y
  ay <- 1
  by <- OBL
  cy <- -YVAL
  DEL_y <- by^2 - (4*ay*cy)
  out1_y <- (-by + sqrt(DEL_y))/(2*ay)
  
  #use y to solve for x
  out1_x <- XVAL/(out1_y + OBL)
  
  #transform back to original coords
  orig_y <- 750 - (YVAL/(out1_y + OBL))
  orig_x <- (XVAL/(out1_y + OBL)) + x_center
  
  #out object
  OUT <- data.frame(orig_x= orig_x, orig_y= orig_y)
  
  return(OUT)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#JUST PLOTTING
#|
#v

#NEKO data merged with transformed x and transformed y
tNEKO_con_data <- data.frame(NEKO_con_data, xtr= x_val, ytr= y_val)

#unique images that we have consensus data for
unique_images <- unique(tNEKO_con_data$path)

#Plots of normal vs transformed clicks to see effect of transformation
#first plot camera image

#unique images needs to be defined first
setwd(paste0(dir, 'Images/NEKOc'))
i <- 23
img_to_plot <- paste0(substr(unique_images[i], 7,27), '.JPG')
plot_jpeg(img_to_plot)

#plot all consensus clicks for NEKOc_2013
#transform clicks to camera dimensions then plot
tp <- cam_trans(cbind(NEKO_con_data$x, NEKO_con_data$y))
points(tp[,1], tp[,2], pch='.', col=rgb(.3,.8,.3, alpha=.3))

#plot all consensus clicks for NEKOc_2013 orthorectified
plot(x_val, y_val, pch='.')

#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#NOT NEEDED IF GREP AFTER NND AT BEGINNING
#|
#v


#just 380 of images - about when creche happens 
#this was determined here manually but can be automated with NND
series <- unique_images[1:380]

temp_image <- c()
for (i in 1:length(series))
{
  #i<-1
  temp_lp <- tNEKO_con_data[grep(series[i], tNEKO_con_data$path),]
  temp_image <- rbind(temp_image, temp_lp)
}

<- temp_image
head(temp_image)
#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Click Density -----------------------------------------------------------

den_fun <- function(IN)
{
  #IN <- post_ortho
  #dimensions for kernel density estimation
  dimx <- 2048
  dimy <- 1536

  #dimx <- 1000
  #dimy <- 750
  
  #kernel density estimation - calculates clicks density over continuous interval
  #bandwidth calculated using width.SJ function
  f2 <- kde2d(IN$x, IN$y, n=c(dimx, dimy),
              h= c(width.SJ(IN$x), width.SJ(IN$y)))

  #scale density to 0,1 to better interpret
  sc_z <- apply(f2$z, scale, MARGIN= c(1, 2), center= 0, scale= max(f2$z))
  s2_s <- list(x= f2$x, y= f2$y, z= sc_z)
  
  OUT <- s2_s
  
  return(OUT)
}

den_out <- den_fun(post_ortho)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#JUST PLOTTING
#|
#v
# Density plots -----------------------------------------------------------

#contour plot
#Takes time
contour(den_out)
#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




# Contours ----------------------------------------------------------------

#specifies density threshold, determines polygons of that density
#then calculates which points are inside those density thresholds


#DENSITY THRESHOLD - can be changed but this works well here
THR <- 0.25

#contour lines - 0.25 contour
c_lines <- contourLines(s2_s, levels = THR)

#number of polygons for 0.25 contour
n_poly <- length(c_lines)

#all clicks inside 0.25 density contour polygons
X <- temp_image[,6:7]
HD_clicks <- c()
for (k in 1:n_poly)
{
  #k <- 1
  tmat <- cbind(c_lines[[k]]$x, c_lines[[k]]$y)
  temp <- which(pnt.in.poly(X, tmat)$pip > 0)
  TT <- X[temp,]
  HD_clicks <- rbind(HD_clicks, TT)
}

#plot filtered points (only points in areas where scaled density is > 0.25)
plot(HD_clicks[,1], HD_clicks[,2], pch='.')
#reverse ortho and lay over image - then have humans click on image to define nests


# K-means -----------------------------------------------------------------

#possibly remove
#Number of nests has to be fed into code - don't think there is a way around this
#There are 26 nests in this image - determined manually
#This is the time limiting step. Many starting points must be run to make
#...sure cluster centers are determined correctly. 2million seems alright here
#It will be easy to run this on AWS or the supercoputer here at SBU

#for benchmarking - run between #~~~~~~~~~~~~#

#~~~~~~~~~~~~#
ptm <- proc.time()

#run in parallel with 2 cores

par.function <- function(i)
{
  kmeans(out, 26, nstart= i, iter.max= 1000000000, algorithm = 'Hartigan-Wong')
}

#2 cores
res <- mclapply(c(1000000, 1000000), FUN = par.function)

#4 cores
#res <- mclapply(c(500000, 500000, 500000, 500000), FUN = par.function)

temp.vec <- sapply(res, function(nests) {nests$tot.withinss})
nests <- res[[which.min(temp.vec)]]

#1 core
#nests <- kmeans(out, 26, nstart= 2000000, iter.max = 1000000000, algorithm = 'Hartigan-Wong')

proc.time() - ptm
#~~~~~~~~~~~~#


#-----------#
#benchmark results for MBP

#1 core - 95 minutes for 2 million iterations
#2 cores - 50 minutes for 2 million iterations (mclapply)
#4 cores - CRASHED
#-----------#



# Plot and process --------------------------------------------------------

#need to reverse orthorectify and transform to camera image dimensions to visualize
#reverse orthorectification



####fix
btrans_pts <- rev_ortho_fun(nests$center, post_ortho)
nest_cam_bt <- cam_trans(btrans_pts)

#filtered clicks in high density area with nest centers
plot(HD_clicks[,1], HD_clicks[,2], pch='.')
points(nests$centers, col= gg_color_hue(26), pch= 19)

#camera image with nest centers
plot_jpeg(img_to_plot)
#pts.fun(NEKO_con_data[,4:5]) # plots all consensus click points
points(nest_cam_bt, col= gg_color_hue(26), pch= 19)



# Data for time series creation --------------------------------------------

#Arrange data by image # (time)
#get jpg number from path name and sort
data_order <- order(substr(NEKO_con_data$path, start=18, stop=23))
image_names <- NEKO_con_data$path[data_order]

x.pt <- (NEKO_con_data$x[data_order] * 2.048)
y.pt <- (1536 - (NEKO_con_data$y[data_order]*2.048))

#zooniverse consensus click data - sorted in chronological order
click.dat <- cbind(x.pt, y.pt)

#nest centers from output of define nest script
nest.dat <- cbind(bt_x, bt_y)


# Tesselation/time series creation ----------------------------------------

#Tests each click location to determine which nest it is located in

width <- 2048
height <- 1536

vt <- deldir(nest.dat[,1],nest.dat[,2], rw= c(0,width,0,height)) #Voronoi tesselation using specified nest sites
w <- tile.list(vt) #polygon coordinates

polys <- vector(mode= 'list', length= length(w))
for (i in seq(along= polys))
{
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- pcrds #arrange polygon coordinates
}



#-----------------------------------------#
#PLOT all consensus clicks, nest centers, polygons, and nest numbers

plot_jpeg(img_to_plot)
points(click.dat, col=rgb(.3,.6,.3, alpha=.5), pch='.')
points(nest.dat, col= gg_color_hue(26), pch= 19)

#number nests on image
text(nest.dat, labels=paste(1:26), col= 'white', cex = 1.2)

#Plot polygons on image
for (i in 1:length(polys))
{
  polygon(polys[[i]], lwd=3)
}
#-----------------------------------------#


#determine which points are in which polygons
out <- c()
for (j in 1:length(polys))
{
  #j <- 1
  temp <- pnt.in.poly(click.dat, polys[[j]])$pip
  out <- cbind(out, temp)
  temp.names[j] <- paste0("nest",j)  
}

#create data frame
colnames(out) <- temp.names
t_series <- data.frame(IMAGE = image_names, out)

#unique images
u_images <- unique(image_names)


#progress bar
pb <- txtProgressBar(min = 1, max = NROW(u_images), style = 3)

#create empty matrix
OUT <- matrix(nrow= NROW(u_images), ncol= (NCOL(t_series)-1))

#summary information for each image (i.e., time step)
for(i in 1:NROW(u_images))
{
  #i <- 1
  temp <- filter(t_series, IMAGE == u_images[i])
  
  #sum number of penguins in given polygon for each image
  summed <- apply(temp[,-1], 2, sum)
  
  OUT[i,] <- summed
  setTxtProgressBar(pb, i)
}
close(pb)

#bind time series data with image names
summary <- data.frame(IMAGE= u_images, OUT)
colnames(summary) <- colnames(t_series)

#time series for each nest - see reference image for nest number
summary


