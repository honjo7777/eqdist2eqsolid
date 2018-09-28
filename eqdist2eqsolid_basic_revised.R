#conversion from theta image to fish-eye image 
# and sky view factor
#theta2svf, copyright 2017(c) Tsuyoshi Honjo
# 
library(jpeg)
library(png)

#following lines are changed if directory, file name, threshold value, rgb ratio are changed
#setwd("/Users/xxx/example") #directory of the Theta's image file
setwd("/Users/honjo/Desktop/eqdist2eqsolid")
fname0<-"example_dist" #file name
fname<-paste(fname0,".jpg",sep="") #file name + .jpg

xth<-0.6 #threshold value in making binary images
#rgb ratio
r_ratio<-1
g_ratio<-1
b_ratio<-1

x<-readJPEG(fname)
xdim<-nrow(x)
x2<-array(0, dim=c(xdim, xdim, 3))
xdim2<-xdim/2
ix0<-xdim2

for(i in 1:xdim2){
  for(j in 1:xdim2){
    ix<-atan(j/i)
    iy<-sqrt(i^2+j^2)
    iy2<-xdim2*asin(iy/xdim2/sqrt(2))/pi*4 #equi-area
    i2<-round(iy2*cos(ix))
    j2<-round(iy2*sin(ix))
    if(i2>xdim2)i2<-xdim2
    if(j2>xdim2)j2<-xdim2
    x2[i+xdim2,j+xdim2,1:3]<-x[i2+xdim2,j2+xdim2,1:3]
  }
}

for(i in -xdim2:-1){
  for(j in -xdim2:-1){
      ix<-atan(j/i)
      iy<-sqrt(i^2+j^2)
      iy2<-xdim2*asin(iy/xdim2/sqrt(2))/pi*4 #equi-area
      i2<- -round(iy2*cos(ix))
      j2<- -round(iy2*sin(ix))
      if(i2< -xdim2)i2<- -xdim2
      if(j2< -xdim2)j2<- -xdim2
      x2[i+xdim2+1,j+xdim2+1,1:3]<-x[i2+xdim2+1,j2+xdim2+1,1:3]
  }
}

for(i in 1:xdim2){
  for(j in -xdim2:-1){
    ix<-atan(j/i)
    iy<-sqrt(i^2+j^2)
    iy2<-xdim2*asin(iy/xdim2/sqrt(2))/pi*4 #equi-area
    i2<- round(iy2*cos(ix))
    j2<- round(iy2*sin(ix))
    if(i2> xdim2)i2<- xdim2
    if(j2< -xdim2)j2<- -xdim2
    x2[i+xdim2,j+xdim2+1,1:3]<-x[i2+xdim2,j2+xdim2+1,1:3]
  }
}

for(i in -xdim2:-1){
  for(j in 1:xdim2){
    ix<-atan(j/i)
    iy<-sqrt(i^2+j^2)
    iy2<-xdim2*asin(iy/xdim2/sqrt(2))/pi*4 #equi-area
    i2<- -round(iy2*cos(ix))
    j2<- -round(iy2*sin(ix))
    if(i2< -xdim2)i2<- -xdim2
    if(j2> xdim2)j2<- xdim2
    x2[i+xdim2+1,j+xdim2,1:3]<-x[i2+xdim2+1,j2+xdim2,1:3]
  }
}
writePNG(x2,paste(fname0,"_area.png",sep=""))

#making equal-area binary image
x<-(x2[,,1]*r_ratio+x2[,,2]*g_ratio+x2[,,3]*b_ratio)/(r_ratio+g_ratio+b_ratio)
x[x>=xth]<-1
x[x<xth]<-0

writePNG(x,paste(fname0,"_area_bi.png", sep=""))

#calculation of sky view factor from equal-area image
(svf<-sum(x)/(pi*xdim2^2))
