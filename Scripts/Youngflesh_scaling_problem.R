#Youngflesh scaling problem

#I have a set of points in a rectangular space. The x coordinates are scaled
#so that they are centered on 0. 

#original points
x <- scale(runif(10000, min= 0, max= 1000), scale=FALSE)
y <- runif(10000, min= 0, max= 750)

plot(x,y, pch='.')



#I want to scale these points into a parabola, so that the x points near y=0 are
#closer than x points near x=750 (bascially warping the image). As below

#SC = scale
SC <- 0
x_scale = x * (y + SC)
y_scale = y * (y + SC)

plot(x_scale,y_scale, pch='.')


#Sometimes I don't want a complete parabola, though. I want there to be a bit
#of a buffer.

SC <- 200
x_scale = x * (y + SC)
y_scale = y * (y + SC)

plot(x_scale,y_scale, pch='.')


#As SC approaches infinity, the plotted space approaches the original rectangle

SC <- 10000
x_scale = x * (y + SC)
y_scale = y * (y + SC)

plot(x_scale,y_scale, pch='.')


#I would like to create a variable 'T', that scales SC from 0 to 1. I essentially 
#don't know (and clearly don't understand math well enough to calculate) what the 
#decay function (or whatever it might be called) is as SC gets larger. 


#In my mind, T=0 would be the parabola, T=1 would be a rectangle. I could 
#just multiply T by 10000 and use that, however the difference between T=0 and T=0.1 
#will be much larger than the difference between T=0.9 and T=1.0. I'd like T to scale 
#things linearly. I feel like the solution to this is probably pretty straight 
#forward, I'm just lost. Any help would be much appreciated!


