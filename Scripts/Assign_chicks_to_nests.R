
#############################
#############################
#Relevant for nest time series
#############################
#############################



#1) input nest locations
#2) tesselate using nest locations
#3) arrange data in time series
#4) determine which polygon (i.e., nest zone) the points are in used the tesselation polygons (static)
#5) aggregate to creat time series



require(deldir)
require(SDMTools)
require(dplyr)


#CREATE FAKE NEST LOCATION DATA
x <- runif(10, 0, 100)
y <- runif(10, 0, 100)
data <- cbind(x, y)



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



#RUN FUNCTION WITH FAKE DATA
polys <- poly_fun(data)


#PLOT FAKE DATA
plot(data, pch = 19)

#PLOT POLYGONS FROM TESSELATION
for (i in 1: length(polys))
{
  lines(polys[[i]])
}





# Point in poly -----------------------------------------------------------


#generate more fake data - actual consesnsus clicks penguin watch for a single image
x_user <- runif(10, 0, 100)
y_user <- runif(10, 0, 100)
data_user <- cbind(x_user, y_user)


#input is arg 1: polygons (output from previous function), arg 2: consensus clicks
point_fun <- function(POLY, ORDER_OUT)
{
  #POLY <- polys
  #ORDER_OUT <- data_user
  
  #CHANGED TO ORDER_OUT[,1], ORDER_OUT[,2] from ORDER_OUT[,2], ORDER_OUT[,3] because col 1 was image name (used in function below)
  DATA <- cbind(ORDER_OUT[,1], ORDER_OUT[,2])
  
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



#RUN FUNCTION WITH FAKE USER DATA
points_nests <- point_fun(polys, data_user)




# Create time series ------------------------------------------------------

ts_fun <- function(POINT_FUN_OUT)
{
  #POINT_FUN_OUT <- points_nests
  
  #unique images - because input had name for each camera image
  u_images <- unique(POINT_FUN_OUT$name)
  
  #progress bar
  pb <- txtProgressBar(min = 1, max = NROW(u_images), style = 3)
  
  #create empty matrix
  OUT <- matrix(nrow= NROW(u_images), ncol= (NCOL(POINT_FUN_OUT)-9))
  
  #summary information for each image (i.e., time step)
  for(i in 1:NROW(u_images))
  {
    #i <- 4
    temp <- filter(POINT_FUN_OUT, name == u_images[i])
    

    #filter for chicks
    ch_pos <- which(temp$probability_of_chick > 0.5)
    chick <- temp[ch_pos,-c(1:9)]
    #sum number of adult in each polygon
    sum_ch <- apply(chick, 2, sum)
    

    #fill matrix by multiples of three
    OUT[(i*2)+(i-1),] <- sum_ch
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  colnames(OUT) <- colnames(POINT_FUN_OUT[-c(1:9)])
  
  #bind time series data with image names
  summary <- data.frame(IMAGE= rep(u_images, each=3), 
                        TYPE= rep(c('ADULT', 'CHICK', 'EGG'), times = length(u_images)),
                        OUT)
  
  
  #time series for each nest - see reference image for nest number
  return(summary)
}




