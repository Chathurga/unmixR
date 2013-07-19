
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