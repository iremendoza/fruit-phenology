####FUNCTIONS FOR MENDOZA et al. REVIEW FRUITING PHENOLOGY NEOTROPICS####
#some simple functions for internal operations
lengthunique=function(x) return(length(unique(x)))
lengthisna=function(x) return(length(which(is.na(x))))

####draw.tropics####
# draws lines (in the sea only) for the tropic of cancer and capricorn based on units of lat and long

draw.tropics = function(color = "grey", ltype = 2, lwidth = 0, ...){
  lines(c(-180, -106.5), c(23.5, 23.5), col = color, lty = ltype, lwd = lwidth) # tropic of cancer
  lines(c(-98, -16), c(23.5, 23.5), col = color, lty = ltype, lwd = lwidth)
  lines(c(59, 68), c(23.5, 23.5), col = color, lty = ltype, lwd = lwidth) 
  lines(c(117, 180), c(23.5, 23.5), col = color, lty = ltype, lwd = lwidth)
  lines(c(-180, -71), c(-23.5, -23.5), col = color, lty = ltype, lwd = lwidth) 	# tropic of capricorn
  lines(c(-47, 15), c(-23.5, -23.5), col = color, lty = ltype, lwd = lwidth)
  lines(c(36, 113), c(-23.5, -23.5), col = color, lty = ltype, lwd = lwidth) 
  lines(c(151, 180), c(-23.5, -23.5), col = color, lty = ltype, lwd = lwidth)
}

####bubble.plot####
#bubble plot is copied from R book for illustrating variation across different locations in the x-y plane
bubble.plot<-function(xv,yv,rv,bs=0.1,maint,...)
{
  r<-rv/max(rv)
  yscale<-max(yv,na.rm=T)-min(yv,na.rm=T)
  xscale<-max(xv,na.rm=T)-min(xv,na.rm=T)
  plot(xv,yv,type="n",main=maint,...)
  for (i in 1:length(xv)) bubble(xv[i],yv[i],r[i],bs,xscale,yscale) }

bubble<-function (x,y,r,bubble.size,xscale,yscale) 
{
  theta<-seq(0,2*pi,pi/200)
  yv<-r*sin(theta)*bubble.size*yscale
  xv<-r*cos(theta)* bubble.size*xscale
  lines(x+xv,y+yv) 
}

