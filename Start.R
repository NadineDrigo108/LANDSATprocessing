#set temp dir 
rasterOptions(tmpdir="~/tmp")
rasterTmpFile(prefix="ras_ndrigo")

dati_wd = "/whrc/biomass/ndrigo/dati"
setwd(dati_wd)

getwd()

library(gdalUtils)
library(rgdal)
library(raster)
library(sp)
library(grid)
library(ggplot2)

## ~~~~~~~~~~~~~~~       UNTAR setup
setwd("tar/L52008")
lftar = list.files(path = ".", pattern = ".tar")

# .........  UNTAR FOR-LOOP ............

for (i in 1:length(lftar)) {
  system(paste0("tar -xvf ", lftar[i]))
}

#~~~~~~~~~~~~~~~~~~~  PREPARING FOR THE CLOUD MASK FUNCTION ~~~~~~~~~~~~~~~~~~

# #~~~~~~~~~~~~~~~     DEBUGGGGGGGG    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lf = list.files(path = "/whrc/biomass/ndrigo/dati/tar/L72008", pattern = "20080101")
# bands = lf[grep("band", lf)]
# bands = bands[-1]
# cloudmask = lf[grep("pixel", lf)]
# image_stack = stack(paste0(wd, "/", bands))
# image_mask_f = raster(paste0(wd,"/",cloudmask))
# RecMatC = matrix(c(72,0,136,0,96,0,112,0,160,0,176,0,224,0), ncol = 2, byrow = T)   # all cloud and shadows get 0 (in the mask)
# RecMatO = matrix(c(1,255,1), ncol = 3, byrow = T)                                   # all other values get 1 (in the mask)
# image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
# name = substr(bands[1],1,44)

# ....... PREPARING FOR MaskCould_stack ......................
wd = "/whrc/biomass/ndrigo/dati/tar/2008"
setwd(wd)
l = c("20080101","20080305", "20080406", "20080508", "20080828", "20080913", "20080929","20081015", "20081031", "20081116", "20081218")
l = c("20080101","20080305")



MaskCloud_stack(l)
#~~~~~~~~~~        MY LOVELY FUNCTION ~~~   there is need to have already created the "2008CloudFreeStacks" directory (where to put the stack)
MaskCloud_stack = function(datelist){       # ORIGINALLLL 
  for (i in 1:length(datelist)) {
    lf = list.files(path = "/whrc/biomass/ndrigo/dati/tar/2008", pattern = datelist[i])
    bands = lf[grep("band", lf)]
    bands = bands[-1]
    cloudmask = lf[grep("pixel", lf)]
    image_stack = stack(paste0("/whrc/biomass/ndrigo/dati/tar/2008/", bands))
    image_mask_f = raster(paste0("/whrc/biomass/ndrigo/dati/tar/2008/",cloudmask))
    RecMatC = matrix(c(72,0,136,0,96,0,112,0,160,0,176,0,224,0), ncol = 2, byrow = T)   # all cloud and shadows get 0 (in the mask)
    RecMatO = matrix(c(1,255,1), ncol = 3, byrow = T)                                   # all other values get 1 (in the mask)
    image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
    name = substr(bands[1],1,44)
    clean_image_name = paste0(name, "CloudFreeStack.tif")
    clean_image = mask(image_stack, image_mask, maskvalue = 0)
    clean_image[clean_image > 10000 | clean_image <= 1] = NA
    writeRaster(clean_image, filename = paste0("2008CloudFreeStacks/",clean_image_name), "GTiff", datatype = "INT2S")
  }
  
}

l5 = c("20080601", "20080905", "20080921")
dpath = "/whrc/biomass/ndrigo/dati/tar/2008"
MaskCloud_stack(l5,dpath)

#.................................................    SAME VERSION AS ABOVE, BUT WITH DIRECTORY PATH AS A VARIABLE 
#................................... didn't check if it works...
MaskCloud_stack = function(datelist, dirpath){      # directory path as varable
  for (i in 1:length(datelist)) {
    lf = list.files(path = dirpath, pattern = datelist[i])
    bands = lf[grep("band", lf)]
    bands = bands[-1]
    cloudmask = lf[grep("pixel", lf)]
    image_stack = stack(paste0(dirpath,"/", bands))
    image_mask_f = raster(paste0(dirpath,"/",cloudmask))
    RecMatC = matrix(c(72,0,136,0,96,0,112,0,160,0,176,0,224,0), ncol = 2, byrow = T)   # all cloud and shadows get 0 (in the mask)
    RecMatO = matrix(c(1,255,1), ncol = 3, byrow = T)                                   # all other values get 1 (in the mask)
    image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
    name = substr(bands[1],1,44)
    clean_image_name = paste0(name, "CloudFreeStack.tif")
    clean_image = mask(image_stack, image_mask, maskvalue = 0)
    clean_image[clean_image > 10000 | clean_image <= 1] = NA
    writeRaster(clean_image, filename = paste0(dirpath,"/2008CloudFreeStacks/",clean_image_name), "GTiff", datatype = "INT2S")
  }
  
}



# # BEFORE I MAKE A MESS BKUP 
# # LANDSAT scenes folders name
# J01_folder = "LT051690602008060101T1-SC20180509120607"
# S05_folder = "LT051690602008090501T1-SC20180509120647"
# S21_folder = "LT051690602008092101T1-SC20180509120522"
# MaskCloud(J01_folder)
# MaskCloud(S05_folder)
# MaskCloud(S21_folder)

# MaskCloud = function(folder){
#   dati_wd = "/whrc/biomass/ndrigo/dati"
#   setwd(dati_wd)
#   image_dir = paste0(getwd(),"/", folder)
#   setwd(image_dir)
#   lf = list.files(path = ".", pattern = "band")
#   #lf = lf[-1]
#   mf = list.files(path = ".", patter = "pixel")
#   image_stack = stack(paste0(image_dir, "/", lf))
#   image_mask_f = raster(mf)
#   RecMatC = matrix(c(72,0,136,0,96,0,112,0,160,0,176,0,224,0), ncol = 2, byrow = T)# all cloud and shadows get 0 (in the mask)
#   RecMatO = matrix(c(1,255,1), ncol = 3, byrow = T)# all other values get 1 (in the mask)
#   image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
#   name = substr(lf[1],1,44)
#   clean_image_name = paste0(name, "CloudFreeStack.tif")
#   clean_image = mask(image_stack, image_mask, maskvalue = 0)
#   clean_image[clean_image > 10000 | clean_image <= 1] = NA
#   writeRaster(clean_image,filename = clean_image_name, "GTiff", datatype = "INT2S")
# }
# 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wd_2008 = "/whrc/biomass/ndrigo/dati/tar/2008/2008CloudFreeStack"
setwd(wd_2008)

# #output check L5
J01 <- stack("LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif")
J01
plot(J01,1)
click(J01,1)

S05 = stack("LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif")
S05
plot(S05,1)
click(S05,1)

S21 = stack("LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif")
S21
plot(S21,2)
click(S21,2)

#~~~~~~~~~~~~~~~~~~~~~~~      for 3 images   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#merge using gdal_merge.py MERGING STACKS
setwd(dati_wd)
J01m = "LT051690602008060101T1-SC20180509120607/LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif"
S05m = "LT051690602008090501T1-SC20180509120647/LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif"
S21m = "LT051690602008092101T1-SC20180509120522/LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif"

system(paste0("gdal_merge.py -separate -a_nodata -32768 -o gdalTest/L52008_J01_S05_S21.tif ",J01m," ",S05m, " ", S21m))
system(paste0("gdal_merge.py -separate -init -32768 -a_nodata -32768 -o gdalTest/L52008_J01_S05_S21_n0.tif ",J01m," ",S05m, " ", S21m)) #make no difference adding the -n 0

system("gdal_translate -a_nodata 0 gdalTest/L52008_J01_S05_S21.tif gdalTest/L52008_J01_S05_S21_n0.tif") #not working

# test with normal
L52008 = stack("gdalTest/L52008_J01_S05_S21.tif")
plot(L52008)
l = calc(L52008, fun=function(x)length(na.omit(x)))
#test with -n 0
L52008n0 = stack("gdalTest/L52008_J01_S05_S21_n0.tif")
plot(L52008n0)
n = calc(L52008n0, fun=function(x)length(na.omit(x)))

#~~~~~~~~~~~~~~~~~~~~~~~~~      for all 2008 images   (14)     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dati_wd = "/whrc/biomass/ndrigo/dati"
setwd(dati_wd)
setwd("tar/2008/2008CloudFreeStacks")
l <<- NULL                          # empty the list
lf = list.files(path = ".")
for (i in 1:length(lf)){
  l = paste(l,lf[i], collapse = " ")
}
print(l)
tosys = paste0("gdal_merge.py -separate -init -32768 -a_nodata -32768 -o 2008.tif ",l)
print(tosys)
system(tosys)

#~~~~~~~~~~~~~~~~~~     create layer with number of observations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

L2008 = stack("2008.tif")
L2008
plot(L2008)
#L52008[L52008 == 0] = NA
plot(L2008,2)
click(L2008,2)

l = calc(L2008, fun=function(x)length(na.omit(x)))
writeRaster(l,filename = "2008ValNum.tif", "GTiff", datatype = "INT2S")
numVal2008 = raster("2008ValNum.tif")
realValue2008 = numVal2008/5
plot(realValue2008)

#..................     decide what is the minimun number of observations that is accetable     ..........
# ....................  I haven't checked that yet, as what should be the minimum values accettable?
NAmask = l
NAmask[NAmask<2] <- 0       # in this case 2 is set as treshold
NAmask[NAmask>=2] <- 1
plot(NAmask)
L52008mask = mask(L52008, NAmask, filename = "gdalTest/L52008_J01_S05_S21_NAmask.tif", maskvalue = 0)
setwd("test11May/testStack")
writeRaster(L52008mask, filename = "gdalTest/L52008_J01_S05_S21.tif")



#~~~~~~~~~~~~~~~~~~ THIS IS THE NEXT NEXT NEXT STEP...PLOT PIXELS REFLECTANCE FOR EACH DATE 
x = c(1:57228171)                   # create a vector long as all pixels in the image...maybe is possible to make a variable of it
# x = c(1:L2008$ncell)   # why I can't access it as ncell?????
samplex = sample(x, 10)             # this is sampling 10 points
extractx = extract(L2008,samplex)   # this extracts the values out of the stack
v = data.frame(extractx)            # this stansform into a dataframe
x = c(v$X2008.1, v$X2008.6,v$X2008.11,v$X2008.16,v$X2008.21,
      v$X2008.26,v$X2008.31,v$X2008.36,v$X2008.41,v$X2008.46,v$X2008.51,
      v$X2008.56,v$X2008.61,v$X2008.66)
plot(x)

# #~~~~~~~~~~~~~    MAKE A LIST OF NUMBERS CORRESPONDING TO 
# bandsNum = function(star, maxm){
#   x = star
#   mylist = c(star)
#   while(x < maxm) {
#     x = x+5
#     mylist = c(mylist,x)
#   }
#   mylist = mylist[-15]
#   return(mylist)
# }

# here you define the band (1 to 5...1 is b2, 5 is b7SWIR2)
#could be nice to do it into a for loop...but I thinl it willl take me more time that what I will actually save
# for now is like this...
list1 = bandsNum(1, 70)
list2 = bandsNum(2, 70)
list3 = bandsNum(3, 70)
list4 = bandsNum(4, 70)
list5 = bandsNum(5, 70)


#....... what about subsetting each band?
b2 = subset(L2008, list1)
b3 = subset(L2008, list2)
b4 = subset(L2008, list3)
b5 = subset(L2008, list4)
b7 = subset(L2008, list5)

t = c(1:57228171)                   
samplet = sample(t, 1000)
# B2 Green
b2df = as.data.frame(extract(b2,samplet))
b2h = hist(b2df[b2df !="NA"], main = "B2 Green")
# B3 Red
b3df = as.data.frame(extract(b3,samplet))
b3h = hist(b3df[b3df !="NA"], main = "B3 Red")
# B4 NIR
b4df = as.data.frame(extract(b4,samplet))
b4h = hist(b4df[b4df !="NA"], main = "B4 NIR")
# B5 SWIR1
b5df = as.data.frame(extract(b5,samplet))
b5h = hist(b5df[b5df !="NA"], main = "B5 SWIR1")
# B7 SWIR2
b7df = as.data.frame(extract(b7,samplet))
b7h = hist(b7df[b7df !="NA"], main = "B7 SWIR2")






# # this is not working, and I am not even sure is the wright approach...but it would make the a direct plotting by selecting the colums of the dataframe I need to plot...
# all = c()
# for (i in 1:length(list2)){
#   one = as.symbol(paste0("v$X2008.",list2[i]))
#   all = c(all, one)
# }
# print(all)
# vec = unlist(all, use.names=FALSE)
# plot(all[1])
# one = as.symbol(paste0("v$X2008.",list2[3]))
# one
