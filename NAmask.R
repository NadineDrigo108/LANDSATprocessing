dati_wd = "/whrc/biomass/ndrigo/dati"
setwd(dati_wd)

# #output check
J01_folder = "LT051690602008060101T1-SC20180509120607"
S05_folder = "LT051690602008090501T1-SC20180509120647"
S21_folder = "LT051690602008092101T1-SC20180509120522"

setwd(dati_wd)
setwd(J01_folder)
J01 <- stack("LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif")
J01
plot(J01,1)
click(J01,1)

setwd(dati_wd)
setwd(S05_folder)
S05 = stack("LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif")
S05
setwd(dati_wd)
setwd(S21_folder)
S21 = stack("LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif")
S21
plot(S21)

#merge using gdal_merge.py
setwd(dati_wd)
J01m = "LT051690602008060101T1-SC20180509120607/LT05_L1TP_169060_20080601_20161031_01_T1_sr_CloudFreeStack.tif"
S05m = "LT051690602008090501T1-SC20180509120647/LT05_L1TP_169060_20080905_20161029_01_T1_sr_CloudFreeStack.tif"
S21m = "LT051690602008092101T1-SC20180509120522/LT05_L1TP_169060_20080921_20161029_01_T1_sr_CloudFreeStack.tif"

#system(paste0("gdal_merge.py -o gdalTest/L52008_J01_S05_S21_2.tif -separate ",J01m," ",S05m, " ", S21m))

setwd("gdalTest")
L52008 = stack("L52008_J01_S05_S21_2.tif")
plot(L52008,4)

#merge with the extent
EXs = c(699585,931515,-104115,102615)

J01_crop = crop(J01,EXs)
S05_crop = crop(S05,EXs)
S21_crop = crop(S21,EXs)
stac = stack(J01_crop,S05_crop,S21_crop)
writeRaster(stac, filename = "test11May/testStack/2008Stack2", "GTiff")

S2008 = stack("test11May/testStack/S05test.tif")
light_stac = stack(stac,c(1,7,14))
writeRaster(light_stac, filename = "test11May/testStack/2008light_Stack2", "GTiff")
l = calc(light_stac, fun=function(x)length(na.omit(x)))
NAmask = l
NAmask[NAmask<6] <- NA
NAmask[NAmask>=6] <- 1
setwd("test11May/testStack")
writeRaster(l, filename = "2008NAmask.tif")

NAcorrStack = mask(stac, NAmask)

# writeRaster(S05_crop, filename = "test11May/testStack/S05test.tif")








