# 
dati_wd = "/whrc/biomass/ndrigo/dati"
setwd(dati_wd)

getwd()


library(gdalUtils)
library(rgdal)
library(raster)
library(sp)
library(grid)


###read raster scene from L5 169060 year 2008
#folders names
J01 = "LT051690602008060101T1-SC20180509120607"
S05 = "LT051690602008090501T1-SC20180509120647"
S21 = "LT051690602008092101T1-SC20180509120522"

#fmask
mask = "_pixel_qa.tif"

#----------------JUNE 01 2008--------------------------
#set wd in the Landsat image I am interested in
setwd(paste0(dati_wd, "/", J01))

#list files to put in the stack (bands 1 to 7)
lf = list.files(path = "/whrc/biomass/ndrigo/dati/LT051690602008060101T1-SC20180509120607", 
           pattern ="band"  )
mask = list.files(path = ".", pattern = "pixel")
#create a stack
J01stack = stack(paste0("/whrc/biomass/ndrigo/dati/LT051690602008060101T1-SC20180509120607/", lf))

#general info about the stack or raster
J01stack

#visualise one band of the stack
plot(J01stack, 1)

#select 
J01mask = raster(mask)

#reclassify the raster based on the values: to do so i need a matrix with 3 colums:
# 1) from, 2) to, 3) becomes. this can be done by a vector that can be transformed 
#in matrix with byrow=TRUE

#matrix for clouds Clouds and shadows = 2
recMatC = matrix(c(72,2,136,2,96,2,112,2,160,2,176,2,224,2), ncol = 2, byrow = T)
recMatC

#matrix for other: all other codes = 0
recMatO = matrix(c(3,255,0), ncol = 3, byrow = T)
recMatO

# maskC2 = reclassify(J01mask, rcl = recMatC)
# maskJ01 = reclassify(maskC2, rcl = recMatO)

maskJ01 = reclassify(reclassify(J01mask, rcl = recMatC), rcl = recMatO)
maskJ01
plot(maskJ01)

CleanJ01 = mask(J01stack, maskJ01, filename = "J01Clean", maskvalue = 2)
plot(CleanJ01,1)

writeRaster(CleanJ01,"ClenaTest", "GTiff")

#-------------------SEPTEMBER 05 2008--------------
#set wd in the Landsat image I am interested in
S05path = paste0(dati_wd, "/", S05)
setwd(S05path)

#list files to put in the stack (bands 1 to 7)
lf = list.files(path = S05path, pattern ="band")
lf = list.files(path = ".", pattern ="band")
mask = list.files(path = ".", pattern = "pixel")
#create a stack
S05stack = stack(paste0(S05path, "/", lf))


#select 
S05mask = raster(mask)

#reclassify the raster based on the values: to do so i need a matrix with 3 colums:
# 1) from, 2) to, 3) becomes. this can be done by a vector that can be transformed 
#in matrix with byrow=TRUE

#matrix for clouds Clouds and shadows = 2
recMatC = matrix(c(72,2,136,2,96,2,112,2,160,2,176,2,224,2), ncol = 2, byrow = T)
recMatC

#matrix for other: all other codes = 0
recMatO = matrix(c(3,255,0), ncol = 3, byrow = T)
recMatO

# maskC2 = reclassify(J01mask, rcl = recMatC)
# maskJ01 = reclassify(maskC2, rcl = recMatO)

maskS05 = reclassify(reclassify(S05mask, rcl = recMatC), rcl = recMatO)
maskS05
plot(maskS05)

CleanS05 = mask(S05stack, maskS05, filename = "S05Clean.tif", maskvalue = 2)
CleanS05 = mask(S05stack, maskS05, maskvalue = 2)
plot(CleanS05,1)
plot(CleanS05,4)


#~~~~~~~~~~        MY LOVELY FUNCTION           ~~~~~~~~~~~~~~
MaskCloud = function(folder){
  dati_wd = "/whrc/biomass/ndrigo/dati"
  setwd(dati_wd)
  image_dir = paste0(getwd(),"/", folder)
  setwd(image_dir)
  lf = list.files(path = ".", pattern = "band")
  #lf = lf[-1]
  mf = list.files(path = ".", patter = "pixel")
  image_stack = stack(paste0(image_dir, "/", lf))
  image_mask_f = raster(mf)
  RecMatC = matrix(c(72,NA,136,NA,96,NA,112,NA,160,NA,176,NA,224,NA), ncol = 2, byrow = T)
  RecMatO = matrix(c(3,255,0), ncol = 3, byrow = T)
  NAcleanMat = matrix(c(-Inf,0, NA,10000, Inf, NA), ncol = 3, byrow = T)
  image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
  name = substr(lf[1],1,44)
  clean_image_name = paste0(name, "CloudFreeStack.tif")
  clean_image = mask(image_stack, image_mask, overwrite = TRUE)
  NAclean_image = reclassify(clean_image, rcl = NAcleanMat)
  writeRaster(NAclean_image,filename = clean_image_name, "GTiff")
}

# # # # # #.................DEBUG..............
# dati_wd = "/whrc/biomass/ndrigo/dati"
# setwd(dati_wd)
# image_dir = paste0(getwd(),"/", J01)
# setwd(image_dir)
# lf = list.files(path = ".", pattern = "band")
# #lf = lf[-1]
# mf = list.files(path = ".", patter = "pixel")
# image_stack = stack(paste0(image_dir, "/", lf))
# 
# hist(image_stack,1)
# plot(image_stack,1)
# 
# image_mask_f = raster(mf)
# RecMatC = matrix(c(72,NA,136,NA,96,NA,112,NA,160,NA,176,NA,224,NA), ncol = 2, byrow = T)
# RecMatO = matrix(c(3,255,0), ncol = 3, byrow = T)
# NAcleanMat = matrix(c(-Inf,-20000, NA,20000, Inf, NA), ncol = 3, byrow = T)
# image_mask = reclassify(reclassify(image_mask_f, rcl = RecMatC), rcl = RecMatO)
# 
# plot(image_mask_f)
# plot(image_mask)
# click(image_mask)
# 
# name = substr(lf[1],1,44)
# clean_image_name = paste0(name, "CloudFreeStack.tif")
# clean_image = mask(image_stack, image_mask, overwrite = TRUE)
# NAclean_image = reclassify(clean_image, rcl = NAcleanMat)
# plot(NAclean_image,1)
# click(NAclean_image)
# NAclean_image
# writeRaster(NAclean_image,filename = clean_image_name, "GTiff", overwrite = T)

 #~~~~~~~~~~~~~~~~   RUN THE LOVELY FUNCTION ~~~~~~~~~~~~~
# LANDSAT scenes folders name
J01_folder = "LT051690602008060101T1-SC20180509120607"
S05_folder = "LT051690602008090501T1-SC20180509120647"
S21_folder = "LT051690602008092101T1-SC20180509120522"
MaskCloud(J01_folder)
MaskCloud(S05_folder)
MaskCloud(S21_folder)

# #output check
setwd(dati_wd)
setwd(J01_folder)
J01 <- stack("LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif")
J01
plot(J01,1)
click(j01,1)

setwd(dati_wd)
setwd(S05_folder)
S05 = stack("LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif")
S05
setwd(dati_wd)
setwd(S21_folder)
S21 = stack("LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif")
S21


#FOR THE MERGE ~~~~~~~~~~~~~~~~~~~~ACTUALLY WE CAN DO IT WITH THE MERGE FUNCTION OF R OR DIRECTLY TRANSFORM IT INTO A STACK
# FROM THE STACK WE SHOULD BE ABLE TO PLOT THE REFLECTANCE AT THE DIFFERENT DATES
setwd(dati_wd)
J01 = "LT051690602008060101T1-SC20180509120607/LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif"
S05 = "LT051690602008090501T1-SC20180509120647/LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif"
S21 = "LT051690602008092101T1-SC20180509120522/LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif"

#merge stacks calling the terminal
#system(paste0("gdal_merge.py ",J01," ",S05, " ", S21, " -o gdalTest/J01_S05_S21.tif -separate"))
system(paste0("gdal_merge.py ",J01," ",S05, " ", S21, " -o gdalTest/L52008_J01_S05_S21.tif -separate"))
system(paste0("gdal_merge.py -o gdalTest/L52008_J01_S05_S21.tif -separate ",J01," ",S05, " ", S21))
# -separate = put all layer separately
#


#read merged file...stack?
getwd()
setwd(dati_wd)
setwd("gdalTest")
J01S05S21 = stack("J01_S05_S21.tif") #18 bands because also the blue band is consider
J01S05S21


# #random sample...this should be to set 
# #generate 10 random numbers
# #extract for each band
# x = c(1:55505931)
# samplex = sample(x, 10)
# extractx = as.data.frame(extract(L52008,samplex))
# extractx
# v = c(extractx$J01_S05_S21_18bands.1[1],extractx$J01_S05_S21_18bands.7[1], extractx$J01_S05_S21_18bands.13[1])
# plot(c(1:3), v)

##~~~~~~~~~~~ SELECT ONLY PIXEL WITH AT LEAST 2 VALUES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

L52008 = "/whrc/biomass/ndrigo/dati/L5169060_2008"
setwd(L52008)
filel = list.files(path = ".")

overlap = raster("Overlappppp.tif")


#to create a mask I need 1 layer for each stack
j01 = stack("LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif")
j011 = raster(j01,1)
s05 = stack("LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif")
s051 = raster(s05,1)
s21 = stack("LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif")
s211 = raster(s21,1)


# #output check
setwd(dati_wd)
setwd(J01_folder)
J01b1 <- raster("LT05_L1TP_169060_20080601_20161031_01_T1_sr_band1.tif")
J01b1
# plot(J01,1)
# click(j01,1)

setwd(dati_wd)
setwd(S05_folder)
S05b1 = raster("LT05_L1TP_169060_20080905_20161029_01_T1_sr_band1.tif")
S05b1
setwd(dati_wd)
setwd(S21_folder)
S21b1 = raster("LT05_L1TP_169060_20080921_20161029_01_T1_sr_band1.tif")
S21b1

s = stack(J01b1, S05b1, S21b1)

EX = extent(J01b1)

S05b1ex = setExtent(S05b1, EX, keepres = T)
S21b1ex = setExtent(S21b1, EX, keepres = T)
stackk = stack(J01b1, S05b1ex, S21b1ex)
l = calc(stackk, fun=length)
#j011_trim = trim(j011, padding = 0, values = NA) # trim is a function that removes the outer rows and colums TOOOOO SLOW



NAcheckStack1 = raster(L52008stack,1)
NAcheckStack7 = raster(L52008stack,7)
NAcheckStack14 = raster(L52008stack,14)
NAstack = stack(NAcheckStack1, NAcheckStack7, NAcheckStack14)

####### LENGTH  
l = calc(NAstack, fun=length)


stack2008_l = stack2008

s = rowSums(!is.na(getValues(stack2008)))


#read merged file...stack?
getwd()
setwd(dati_wd)
setwd("gdalTest")
L52008stack = stack("L52008_J01_S05_S21.tif") #18 bands because also the blue band is consider
plot(L52008stack,1)
click(L52008stack,1)

#this should return me a raster layer with counting the NA values in the stack
# is an 18 band stack, so it will be multiple of 6
#if NA values is higher than 6 
rNA = sum(!is.na(L52008stack)) 
rNA = raster(rowSums(!is.na(L52008stack)))
rNA2 = apply(as.array(L52008stack), 1:2, function(x) length(na.omit(x))) 
rNA3 = stackApply(L52008stack, indices = c(1), function(x)length(na.omit(x)), na.rm = FALSE)










