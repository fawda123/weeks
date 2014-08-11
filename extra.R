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

wtreg <- wtreg_fun(dat, wins = c(4, 6, 1), parallel = T, 
  progress = T)

stopCluster(cl)

######
# calc and plot metab by monthly agg

load('weeks_wtreg.RData')

meta.path <- paste0(getwd(), '/sampling_stations.csv')

# metab before dtd
met_obs <- nem.fun(weeks_wtreg, 'WKBWB', DO_var = 'DO_obs', 
  meta.path = meta.path)

# metab after dtd
met_dtd <- nem.fun(weeks_wtreg, 'WKBWB', DO_var = 'DO_nrm', 
  meta.path = meta.path)

# combine for plots
met_obs$sub_var <- 'Observed'
met_dtd$sub_var <- 'Detided'

col_sel <- c('Date', 'Temp', 'Pg', 'Rt', 'NEM', 'sub_var')
met <- rbind(met_obs[, col_sel], met_dtd[, col_sel])

met$Month <- format(met$Date, '%m')
met$Year <- format(met$Date, '%Y')
met <- melt(met, id.var = c('Date', 'Temp', 'sub_var', 'Month', 'Year'))

# annual monthly means
met <- ddply(met, 
  .(variable, Month, Year, sub_var),
  .fun = function(x) mean(x$value, na.rm = T)
  )

# plot
to.plo <- met

# convert values to mg C m-2 d-1
# 1mmolO2 = 32 mg O2, 1000mg = 1g, multiply by 32/1000
# 1mmol C = 12 mg C, 1mmol C = 1mmolO2
ylab <- expression(paste('Yearly Pg (mg C ', m^-2, d^-1, ')'))
p1 <- ggplot(to.plo[to.plo$variable %in% 'Pg', ], 
    aes(x = Month, y = 12 * V1, fill = sub_var)) +
  geom_boxplot() + 
  facet_wrap(~ variable, scales = 'free_y') + 
  theme(legend.position = 'top', legend.title = element_blank()) +
  ylab(ylab) + 
  ylim(c(0,6000))
print(p1)

######
# metabolism after 15% reduction of DO 

load('WKBWB.RData')

# original wq data, note that rows past 195714 were removed
dat_ori <- WKBWB[1:195714,]

# data from Jim, 15% reduction of DO using mw
# add to orig wq, remove NA
dat_scl <- read.csv('WKBWB_do.csv', header = T)
dat_ori$DO_scl <- dat_scl$DO_scl
dat_ori$DO_scl[is.na(dat_ori$DO_mgl)] <- NA

# metabolism from observed DO and reduced DO (scaled)
met_ori <- nem.fun(dat_ori, 'WKBWB', DO_var = 'DO_mgl')
met_scl <- nem.fun(dat_ori, 'WKBWB', DO_var = 'DO_scl')

# save output
col_sel <- c('Date', 'ATemp', 'Pg', 'Rt', 'NEM')
met_ori <- met_ori[, col_sel]
met_scl <- met_scl[, col_sel]
write.csv(met_ori, 'wkbwb_met_obs.csv', quote = F, 
  row.names = F)
write.csv(met_scl, 'wkbwb_met_red.csv', quote = F, 
  row.names = F)

##
# reload ouput, combine
met_ori <- read.csv('wkbwb_met_obs.csv', header = T)
met_scl <- read.csv('wkbwb_met_red.csv', header = T)
met_ori$var <- 'Observed'
met_scl$var <- 'Reduced'
met_dat <- rbind(met_ori, met_scl)

# add month, year columns
met_dat$Month <- format(as.Date(met_dat$Date), '%m')
met_dat$Year <- format(as.Date(met_dat$Date), '%Y')

# annual monthly means
met_dat<- ddply(met_dat, 
  .(Month, var),
  .fun = function(x){
    Pg <- mean(x$Pg, na.rm = T)
    ATemp <- mean(x$ATemp, na.rm = T)
    
    data.frame(Pg, ATemp)
    
    }
  
  )

# convert Pg units as mmol O2 m^-2 d^-1 to mg C m^-2 d^-1
# 1 mmol of O2 is 1 mmol of C
# 1 mmol of C is 12 mg of C, so multiply by 12
met_dat$Pg <- 12 * met_dat$Pg

# plot
to.plo <- met_dat

y_lab <- expression(paste('mg C ', m^-2,' ', d^-1))
x_lab <- expression(paste('Air temp ', degree, 'C'))
ggplot(to.plo, aes(x = ATemp, y = Pg, colour = var)) + 
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  ylim(c(0,4500)) +
  ylab(y_lab) + 
  xlab(x_lab)
