
# Some values of alpha
  
al <- seq(-1, -20, by = -1)
al <- 10^al

library("hyperSpec")

# Note: now testing with two different versions of hfc: hfcV and hfcS

res <- hfcV(chondro[[]], al) # 4 3 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
res <- hfcV(laser[[]], al) # 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
res <- hfcS(chondro[[]], al) # 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
res <- hfcS(laser[[]], al) # 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# BigM from DS0, with noise
res <- hfcV(BigM, al) # 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
res <- hfcS(BigM, al) # 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# BigM from DS0, NO noise (strange)
res <- hfcV(BigM, al) # 226 223 222 222 222 222 222 222 221 221 220 220 220 220 219 219 219 219 219 219

# With the blind test image, available at http://dirsapps.cis.rit.edu/blindtest/

img <- read.ENVI(file.choose())
res <- hfcV(img[[]], al) # 15 15 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 (12 sec)
res <- hfcS(img[[]], 0.01) # Cannot get this to work, it chokes on tcrossprod and goes off working forever

# AVIRIS cuprite data set, scene 1
cup <- read.ENVI(file.choose())
plotmap(cup) # looks good
al <- seq(-1, -10, by = -1)
al <- 10^al
res <- hfcV(cup[[]], al) # 38 36 35 34 34 34 34 34 33 32 (about 45 sec)