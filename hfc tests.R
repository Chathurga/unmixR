
# Some values of alpha
  
al <- seq(-1, -20, by = -1)
al <- 10^al

library("hyperSpec")
res <- hfc(chondro[[]], al) # 4 3 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
res <- hfc(laser[[]], al) # 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7

# BigM from DS0, with noise
res <- hfc(BigM, al) # 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

# BigM from DS0, NO noise (strange)
res <- hfc(BigM, al) # 226 223 222 222 222 222 222 222 221 221 220 220 220 220 219 219 219 219 219 219

# With the blind test image, available at http://dirsapps.cis.rit.edu/blindtest/

img <- read.ENVI(file = "~/Desktop/blind_test/HyMap/blind_test_refl.img")
res <- hfc(img[[]], al) # 15 15 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13

# blind test image takes about 12 sec to compute (224000 x 126)

# AVIRIS cuprite data set, scene 1
cup <- read.ENVI("~/Desktop/f970619t01p02r02c/f970619t01p02_r02_sc01.a.rfl")
plotmap(cup) # looks good
al <- seq(-1, -10, by = -1)
al <- 10^al
res <- hfc(cup[[]], al) # 38 36 35 34 34 34 34 34 33 32 (about 45 sec)