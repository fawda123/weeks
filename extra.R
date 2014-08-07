######
# eval of Weeks Bay DO time series
# Aug. 2014, M. Beck

rm(list = ls())

######
# load/prep data

dat <- prep_wtreg('WKBWB')

# setup parallel and process

cl <- makeCluster(8)
registerDoParallel(cl)

wtreg <- wtreg_fun(dat[1:5000,], wins = c(4, 6, 1), parallel = T, 
  progress = T)

stopCluster(cl)