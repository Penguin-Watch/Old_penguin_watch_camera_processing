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
#Just adult data currently - need chick and egg
#Need to check methods on other sites 0 may need tweaking
#
#
#Created: April 10, 2016
#Modified: April 10, 2016
#
#Author: Casey Youngflesh
##################

### Fiona commit test


### Penguins - hatch rate 

###another


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

osx <- '/Users/caseyyoungflesh/Google Drive/R/Camera trap - mark recapture/'
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


#function to add legend to density plot heat map
#from: http://menugget.blogspot.com/2011/08/adding-scale-to-image-plot.html#more
image.scale <- function(z, zlim, col = rainbow(20),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...)
{
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
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
    stop('"OUT" argument must be with "HEX" or "RGB"')
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



# Load/process data -------------------------------------------------------

setwd(paste0(dir, 'Data'))

#import NEKOc data
NEKO_con_data_imp <- read.csv('NEKOc_consensus.csv', header=TRUE) 

#just 2013
NEKO_con_data <- NEKO_con_data_imp[grep('NEKOc2013', NEKO_con_data_imp$path),c(1:5)]
colnames(NEKO_con_data) <- c('ID', 'ZOOID', 'path', 'x', 'y')



# Orthorectification ------------------------------------------------------

#due to oblique angle of camera, image needs to be altered to correctly determine click density
#essentially normalizing area

#sc <- 2048/1536 #image dimensions
#1000 - range of x clicks
#1000/sc - y range must be 750 

#remove erroneous clicks outside of defined region (not likely any with consensus data)
to_rm <- which(NEKO_con_data$x > 1000 | NEKO_con_data$x < 0 | NEKO_con_data$y < 0 | NEKO_con_data$y > 750)

#transform image
if(length(to_rm) > 0)
{
  NEKO_x <- scale(NEKO_con_data$x[-to_rm], scale=FALSE)
  NEKO_y <- 750 - NEKO_con_data$y[-to_rm]
}else{
  NEKO_x <- scale(NEKO_con_data$x, scale=FALSE)
  NEKO_y <- 750 - NEKO_con_data$y
}

x_val <- NEKO_x * (NEKO_y + 150)
y_val <- NEKO_y * (NEKO_y + 150)



#----------------------------------------------#
#Function to reverse transform for orthorectified to original
#does the reverse of above for given set of points
#put function here as code above determines code here

#For reverse ortho
x_center <- attributes(NEKO_x)$'scaled:center'

#column 1 must be x, column 2 must be y

back_trans <- function(input)
{
  XVAL <- input[,1]
  YVAL <- input[,2]
  
  #quadratic equation for Y
  ay <- 1
  by <- 150
  cy <- -YVAL
  DEL_y <- by^2 - (4*ay*cy)
  out1_y <- (-by + sqrt(DEL_y))/(2*ay)
  
  #use y to solve for x
  out1_x <- XVAL/(out1_y + 150)
  
  #back to original coords
  orig_y <- 750 - (YVAL/(out1_y + 150))
  orig_x <- (XVAL/(out1_y + 150)) + x_center
  
  OUT <- data.frame(orig_x= orig_x, orig_y= orig_y)
  return(OUT)
}
#----------------------------------------------#



#Plots of normal vs transformed clicks to see effect of transformation
#first plot camera image
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

#NEKO data merged with transformed x and transformed y
tNEKO_con_data <- data.frame(NEKO_con_data, xtr= x_val, ytr= y_val)



# Click Density -----------------------------------------------------------

#unique images that we have consensus data for
unique_images <- unique(tNEKO_con_data$path)

#just 380 of images - about when creche happens 
#this was determined here manually but can be automated with chick clicks
series <- unique_images[1:380]

temp_image <- c()
for (i in 1:length(series))
{
  #i<-1
  temp_lp <- tNEKO_con_data[grep(series[i], tNEKO_con_data$path),]
  temp_image <- rbind(temp_image, temp_lp)
}


#dimensions for kernel density estimation
dimx <- 2048
dimy <- 1536

#kernel density estimation - calculates clicks density over continuous interval
#bandwidth calculated using width.SJ function
f2 <- kde2d(temp_image$xtr, temp_image$ytr, n=c(dimx, dimy),
            h= c(width.SJ(temp_image$xtr), width.SJ(temp_image$ytr)))

#scale density to 0,1 to better interpret
sc_z <- apply(f2$z, scale, MARGIN= c(1, 2), center= 0, scale= max(f2$z))
s2_s <- list(x= f2$x, y= f2$y, z= sc_z)



# Density plots -----------------------------------------------------------

#density plot heat map with scale bar
#TAKES SOME TIME TO PLOT
layout(matrix(c(1,2), nrow=2, ncol=1), heights=c(4,1))
layout.show(2)
par(mar=c(1,1,1,1))
image(s2_s, col=rainbow(20), xaxt= 'n', yaxt= 'n')
par(mar=c(3,1,1,1))
image.scale(s2_s$z)

#contour plot
#MUCH QUICKER THAN HEAT MAP
contour(s2_s)



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



# K-means -----------------------------------------------------------------

#Number of nests has to be fed into code - don't think there is a way around this
#There are 26 nests in this image - determined manually
#This is the time limiting step. Many starting points must be run to make
#...sure cluster centers are determined correctly. 2million seems alright here
#It will be easy to run this on AWS or the supercoputer here at SBU

#for benchmarking - run between lines 323-344
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

#-----------#
#benchmark results for MBP

#1 core - 95 minutes for 2 million iterations
#2 cores - 50 minutes for 2 million iterations (mclapply)
#4 cores - CRASHED
#-----------#



# Plot and process --------------------------------------------------------

#need to reverse orthorectify and transform to camera image dimensions to visualize
#reverse orthorectification
btrans_pts <- back_trans(nests$center)
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


#determin which points are in which polygons
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


