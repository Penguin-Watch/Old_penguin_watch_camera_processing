##################
#Penguin Watch nest time series creator
#
#
#Script uses consensus zooniverse clicks
#-Defines nests based on density thresholds
#-Creates time series for each nest
#
##################

#TO DO
#add master data input
#grep site of interest
#NND to determine nest and creche time
#grep images of interest to run through processing pipeline
#NOTE: function might not run on windows, as it uses mcapply() which
#...doesn't work on windows (fine for anything but windows)


#DATA FOR ANALYSIS
#ADPE cams
#YALOa (3? seasons) - likely YALO and PETE together for methods paper
#PETEd (3 seasons)
#Hinke (2 cams, 1 season)
#SSSIc (1 incomplete season)

#NOs
#Aussies (need to ask permission)
#HERO (don't have now)



#post-zooniverse processing:
#1) zoo out consensus data
#2) grep one site at a time
#3) calculate NND to determine correct time to look at nests
#4) subset point of interest to just that specified in Step 3
#5) orthorectify image (with scalar)
#6) density threshold
#7) filter for points only in high density areas
#8) kmeans/click tool to determine nest ceners
#9) run tesselation on nests specified in Step 8
#10) create time series for each nest using consensus data



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




# Nearest Neighbor --------------------------------------------------------


require(spatstat)
require(moments)
tdat <- NEKO_con_data[,3:5]
head(tdat, n= 100)

images <- unique(tdat$path)
#image 1 clicks
plot(img_1[,2:3], pch=19)

#loop through images and find mean, sd, skew, kurt of NND for each image
OUT <- c()
for (i in 1:length(images))
{
  #i <- 1
  temp <- filter(tdat, path == images[i])
  NND <- nndist(temp)
  temp_mean <- mean(NND)
  temp_sd <- sd(NND)
  temp_sk <- skewness(NND)
  temp_kurt <- kurtosis(NND)
  
  TOUT <- data.frame(img= images[i], MN= temp_mean, SD= temp_sd, 
                     SKEW= temp_sk, KURT= temp_kurt) 
  
  OUT <- rbind(OUT, TOUT)
}



plot(OUT$MN, type='l')
plot(OUT$SD, type='l')
plot(OUT$SKEW, type='l')
plot(OUT$KURT, type='l')


#change in mean NND over time
#or
#change in distance 




# Orthorectification ------------------------------------------------------

#due to oblique angle of camera, image needs to be altered to correctly determine click density
#essentially normalizing area
#IN is post NND

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
  OUT <- data.frame(x= x_val, y= y_val, x_scale = SCALE, OBL = OBL) #scale is used in reverse ortho function
  
  return(OUT)
}

ptm <- proc.time()
post_ortho <- ortho_fun(NEKO_con_data, 150)
proc.time() - ptm




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#NOT NEEDED IF GREP AFTER NND AT BEGINNING
#|
#v

#just 380 of images - about when creche happens 
#this was determined here manually but can be automated with NND
unique_images <- unique(tNEKO_con_data$path)

series <- unique_images[1:380]

temp_image <- c()
for (i in 1:length(series))
{
  #i<-1
  temp_lp <- tNEKO_con_data[grep(series[i], tNEKO_con_data$path),]
  temp_image <- rbind(temp_image, temp_lp)
}

#^
#|
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#need to ortho for just period of interest and then (which would be done
#earlier in NND function)


# Click Density -----------------------------------------------------------

den_fun <- function(POST_ORTHO)
{
  #POST_ORTHO <- post_ortho
  #dimensions for kernel density estimation
  dimx <- 2048
  dimy <- 1536

  #dimx <- 1000
  #dimy <- 750
  
  #kernel density estimation - calculates clicks density over continuous interval
  #bandwidth calculated using width.SJ function
  #could maybe make dunction quicker by reducing n...
  f2 <- kde2d(POST_ORTHO$x, POST_ORTHO$y, n=c(dimx, dimy),
              h= c(width.SJ(POST_ORTHO$x), width.SJ(POST_ORTHO$y)))

  #scale density to 0,1 to better interpret
  sc_z <- apply(f2$z, scale, MARGIN= c(1, 2), center= 0, scale= max(f2$z))
  s2_s <- list(x= f2$x, y= f2$y, z= sc_z)
  
  OUT <- s2_s
  
  return(OUT)
}

ptm <- proc.time()
den_out <- den_fun(post_ortho)
proc.time() - ptm



# Contours ----------------------------------------------------------------

#specifies density threshold, determines polygons of that density
#then calculates which points are inside those density thresholds

filter_fun <- function(DEN_OUT, POST_ORTHO, D_THR = 0.25)
{

  #DENSITY THRESHOLD
  #D_THR <- 0.25
  #DEN_OUT <- den_out
  #POST_ORTHO <- post_ortho

  #contour lines with density threshold
  c_lines <- contourLines(DEN_OUT, levels = D_THR)

  #number of polygons for 0.25 contour
  n_poly <- length(c_lines)

  #all clicks inside density threshold contour polygons
  X <- POST_ORTHO[,1:2]
  HD_clicks <- c()
  for (k in 1:n_poly)
  {
    #k <- 1
    tmat <- cbind(c_lines[[k]]$x, c_lines[[k]]$y)
    temp <- which(pnt.in.poly(X, tmat)$pip > 0)
    TT <- X[temp,]
    HD_clicks <- rbind(HD_clicks, TT)
  }
  OUT <- data.frame(x= HD_clicks[,1], y= HD_clicks[,2])
  
  return(OUT)
}

ptm <- proc.time()
filter_out <- filter_fun(den_out, post_ortho, D_THR = 0.25)
proc.time() - ptm




# K-means -----------------------------------------------------------------


#Number of nests has to be fed into code
#There are 26 nests in this image - determined manually
#This is the time limiting step. Many starting points must be run to make
#...sure cluster centers are determined correctly. 2million seems alright her
#return nest centers in original coordinate system
#NOTE: MULTIPLE CORES DOES NOT WORK ON WINDOWS (WORKS ON EVERY OTHER OS)


km_fun <- function(FILTER_OUT, NESTS, CORES = 1, ITERS = 2e6)
{
  #FILTER_OUT <- filter_out
  #ITERS = 2000
  #CORES = 1
  #NESTS = 26
  
  ncore <- detectCores()
  if(CORES > ncore)
  {
    stop("You don't have that many cores on your machine, dummy!")
  }
  
  
  DATA <- as.matrix(FILTER_OUT)
  
  
  if (CORES == 1)
  {
    OUT <- kmeans(DATA, NESTS, nstart= ITERS, iter.max= 1e9, algorithm = 'Hartigan-Wong')
  }else{

    #parallelization
    par.function <- function(i)
    {
      kmeans(DATA, NESTS, nstart= i, iter.max= 1e9, algorithm = 'Hartigan-Wong')
    }

  
    PER_CORE <- round(ITERS/CORES)
    VEC <- rep(PER_CORE, CORES)
  
  
    #run function
    res <- mclapply(VEC, FUN = par.function)

    #merge output from different cores
    temp.vec <- sapply(res, function(nests) {nests$tot.withinss})
    OUT <- res[[which.min(temp.vec)]]
  }
  
  return(OUT$centers)
}

ptm <- proc.time()
km_out <- km_fun(filter_out, NESTS = 26, CORES = 2, ITERS= 2000)
proc.time() - ptm

#-----------#
#benchmark results for MBP

#1 core - 95 minutes for 2 million iterations
#2 cores - 50 minutes for 2 million iterations (mclapply)
#4 cores - CRASHED
#-----------#





# reverse ortho on nest centers -------------------------------------------

#Function to reverse transform for orthorectified to normal 1000 x 750 coords

rev_ortho_fun <- function(KM_OUT, POST_ORTHO)
{
  
  #IN is df of points to be transformed - column 1 must be x, column 2 must be y
  #POST_ORTHO output from ortho_fun -> scaled x data used to find centers and reverse ortho

  #get centers of scaled data from ortho
  x_center <- POST_ORTHO$x_scale[1]
  
  #points to be transformed
  XVAL <- KM_OUT[,1]
  YVAL <- KM_OUT[,2]
  
  #ortho adjuster used in original ortho
  OBL <- POST_ORTHO$OBL[1]
  
  #quadratic equation to backsolve for y
  ay <- 1
  by <- OBL
  cy <- -YVAL
  DEL_y <- by^2 - (4*ay*cy)
  out1_y <- (-by + sqrt(DEL_y))/(2*ay)
  
  #use y to solve for x
  out1_x <- XVAL/(out1_y + OBL)
  
  #transform back to original coords
  orig_y <- (YVAL/(out1_y + OBL))
  orig_x <- ((XVAL/(out1_y + OBL)) + x_center)
  
  #out object
  OUT <- data.frame(orig_x= orig_x, orig_y= orig_y)
  
  return(OUT)
  
}

ptm <- proc.time()
km_rev_ortho <- rev_ortho_fun(km_out, post_ortho)
proc.time() - ptm





# Tesselation ----------------------------------------

#Tests each click location to determine which nest it is located in


poly_fun <- function(KM_REV_ORTHO)
{
  
  #KM_REV_ORTHO <- km_rev_ortho
  
  width <- 1000
  height <- 750
  
  #Voronoi tesselation using specified nest sites
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


ptm <- proc.time()
poly_out <- poly_fun(km_rev_ortho)
proc.time() - ptm




# Data for time series creation --------------------------------------------


#post NND data, post grep for site - images of interest
#chronological order - likely needs to be updated using metadata


order_fun <- function(POST_NND)
{
  
  #POST_NND <- NEKO_con_data
  
  #Arrange data by image # (time)
  #get jpg number from path name and sort
  data_order <- order(substr(POST_NND$path, start=18, stop=23))
  image_names <- POST_NND$path[data_order]
  
  x.pt <- (POST_NND$x[data_order])
  y.pt <- (750 - (POST_NND$y[data_order]))
  
  #zooniverse consensus click data - sorted in chronological order
  click.dat <- data.frame(IMG= image_names, X= x.pt, Y= y.pt)
  
  #remove erroneous clicks outside of defined region
  to_rm <- which(click.dat$X > 1000 | click.dat$X < 0 | click.dat$Y < 0 | click.dat$Y > 750)
  
  #remove erroneous clicks
  if(length(to_rm) > 0)
  {
    OUT <- click.dat[-to_rm,]
  }else{
    OUT <- click.dat
  }
  
  return(OUT)
}

ptm <- proc.time()
order_out <- order_fun(NEKO_con_data)
proc.time() - ptm





# Point in poly -----------------------------------------------------------

point_fun <- function(POLY, ORDER_OUT)
{
  #POLY <- poly_out
  #ORDER_OUT <- order_out
  
  DATA <- cbind(ORDER_OUT[,2], ORDER_OUT[,2])
  
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
  t_series <- data.frame(IMAGE = ORDER_OUT[,1], out)

  return(t_series)
}

ptm <- proc.time()
point_fun_out <- point_fun(poly_out, order_out)
proc.time() - ptm





# Create time series ------------------------------------------------------

ts_fun <- function(POINT_FUN_OUT)
{
  #POINT_FUN_OUT <- point_fun_out
  
  #unique images
  u_images <- unique(POINT_FUN_OUT$IMAGE)

  #progress bar
  pb <- txtProgressBar(min = 1, max = NROW(u_images), style = 3)

  #create empty matrix
  OUT <- matrix(nrow= NROW(u_images), ncol= (NCOL(POINT_FUN_OUT)-1))

  #summary information for each image (i.e., time step)
  for(i in 1:NROW(u_images))
  {
    #i <- 1
    temp <- filter(POINT_FUN_OUT, IMAGE == u_images[i])
  
    #sum number of penguins in given polygon for each image
    summed <- apply(temp[,-1], 2, sum)
  
    OUT[i,] <- summed
    setTxtProgressBar(pb, i)
  }
  close(pb)

  #bind time series data with image names
  summary <- data.frame(IMAGE= u_images, OUT)
  colnames(summary) <- colnames(POINT_FUN_OUT)

  #time series for each nest - see reference image for nest number
  return(summary)
}


ptm <- proc.time()
ts_out <- ts_fun(point_fun_out)
proc.time() - ptm








# Master function ---------------------------------------------------------

#grep -> NND -> grep -> ortho_fun -> den_fun -> filter_fun -> km_fun ->
#... rev_ortho_fun -> poly_fun -> order_fun -> point_fun -> ts_fun 


master_fun <- function(input, nests, obl, d_thr = 0.25, cores = 1, iters = 2e6)
{

  #input <- NEKO_con_data
  #nests <- 26
  #obl <- 150
  #d_thr <- 0.25
  #cores <- 1
  #iters <- 2000
  
  t_post_ortho <- ortho_fun(IN= input, OBL= obl)
  
  t_den_out <- den_fun(POST_ORTHO = t_post_ortho)
  
  t_filter_out <- filter_fun(DEN_OUT = t_den_out, 
                      POST_ORTHO = t_post_ortho, D_THR = d_thr)
  
  t_km_out <- km_fun(FILTER_OUT = t_filter_out, NESTS = nests,
                     CORES = cores, ITERS = iters)
  
  t_km_rev_ortho <- rev_ortho_fun(KM_OUT = t_km_out, 
                                  POST_ORTHO = t_post_ortho)
  
  t_poly_out <- poly_fun(KM_REV_ORTHO = t_km_rev_ortho)
  
  t_order_out <- order_fun(POST_NND = input)
  
  t_point_out <- point_fun(POLY = t_poly_out, ORDER_OUT = t_order_out)
  
  OUT <- ts_fun(POINT_FUN_OUT = t_point_out)
  
  return(OUT)
}




ptm <- proc.time()
master_OUT <- master_fun(input = NEKO_con_data,
                         nests = 26, 
                         obl = 150, 
                         d_thr = 0.25,
                         cores = 2,
                         iters = 2000)
proc.time() - ptm











# PLOTTING ----------------------------------------------------------------

#all points are done in 1000 x 750 resolution
#jpegs are plotted in 2048 x 1536 resolution and inverted y-axis

#cam_trans function to scale and invert if necessary



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
#first column must be x coords, second column y coords
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




# All clicks over camera image --------------------------------------------

#unique images that we have consensus data for
unique_images <- unique(NEKO_con_data$path)

#plot camera image
setwd(paste0(dir, 'Images/NEKOc'))
i <- 23
img_to_plot <- paste0(substr(unique_images[i], 7,27), '.JPG')
plot_jpeg(img_to_plot)

#plot all consensus clicks for NEKOc_2013
#invert y-axis because these are original zoo input clicks
tp <- cam_trans(cbind(NEKO_con_data$x, NEKO_con_data$y), INVERT = TRUE)
points(tp[,1], tp[,2], pch='.', col=rgb(.3,.8,.3, alpha=.3))


# Orthoed clicks ----------------------------------------------------------

#plot all consensus clicks for NEKOc_2013 orthorectified
plot(post_ortho$x, post_ortho$y, pch='.')



# density plots ------------------------------------------------------------

#takes time
#contour(den_out)

#function to add legend to density plot heat map
#from: http://menugget.blogspot.com/2011/08/adding-scale-to-image-plot.html#more

image.scale <- function(z, zlim, col = rainbow(20), breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...)
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

#TAKES SOME TIME TO PLOT
#layout(matrix(c(1,2), nrow=2, ncol=1), heights=c(4,1))
#layout.show(2)
#par(mar=c(1,1,1,1))
#image(s2_s, col=rainbow(20), xaxt= 'n', yaxt= 'n')
#par(mar=c(3,1,1,1))
#image.scale(s2_s$z)



# plot filtered orthoed points --------------------------------------------

#plot filtered points (only points in areas where scaled density is > 0.25)
plot(filter_out[,1], filter_out[,2], pch='.')





# plot filtered clicks with nest centers ----------------------------------
btrans_pts <- rev_ortho_fun(km_out, post_ortho)
#transform from 1000 x 750 to 2048 x 1536
nest_cam_bt <- cam_trans(btrans_pts, INVERT = FALSE)

#filtered clicks in high density area with nest centers
plot(filter_out[,1], filter_out[,2], pch='.')
points(km_out, col= gg_color_hue(26), pch= 19)



# Camera image with nest centers ------------------------------------------

plot_jpeg(img_to_plot)
#pts.fun(NEKO_con_data[,4:5]) # plots all consensus click points
points(nest_cam_bt, col= gg_color_hue(26), pch= 19)



# Image with all clicks and nest centers ----------------------------------

trans_order <- cam_trans(order_out[,2:3], INVERT = FALSE)

plot_jpeg(img_to_plot)
#clicks
points(trans_order[,1], trans_order[,2], col=rgb(.3,.6,.3, alpha=.5), pch='.')
#nest centers
points(nest_cam_bt, col= gg_color_hue(26), pch= 19)



# Add nest numbers to plot ------------------------------------------------

#number nests on image
text(nest_cam_bt, labels=paste(1:26), col= 'white', cex = 1.2)



# Add polygons ------------------------------------------------------------


#Transform polygons frmo 1000 x 750 to 2048 x 1536 
#Plot polygons on image
for (i in 1:length(poly_out))
{
  polygon(cam_trans(poly_out[[i]], INVERT = FALSE), lwd=3)
}




