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
