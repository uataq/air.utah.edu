# Ben Fasoli
# Process data for website render
setwd('/uufs/chpc.utah.edu/common/home/lin-group9/measurements')

library(data.table)
library(fasttime)

source('pipeline/_global.r')

# Determine site list
stids <- site_config$stid[site_config$active]
trx_stids <- grep('trx', stids, value = T)
stids <- setdiff(stids, trx_stids)

# Determine number of files to read
month_start <- as.POSIXct(format(Sys.time(), tz = 'UTC', '%Y-%m-01'), tz = 'UTC')
nf <- ifelse(difftime(Sys.time(), month_start, units = 'days') > 8, 1, 2)

data <- list()
data$qaqc <- rbindlist(lapply(stids, function(stid) {
  base_path <- file.path('data', stid)
  instrument <- grep('lgr|licor', dir(base_path), value = T)[1]
  columns <- switch(instrument,
                    'lgr_ugga' = c(1, 4, 8, 10, 12, 24, 25, 26),
                    'licor_6262' = c(1, 7, 16, 17, 20, 21))
  files <- tail(dir(file.path(base_path, instrument, 'qaqc'), full.names = T), nf)
  df <- rbindlist(lapply(files, fread, 
                         showProgress = F, 
                         select = columns))
  df$stid <- stid
  df
}), fill = T)

data$calibrated <- rbindlist(lapply(stids, function(stid) {
  base_path <- file.path('data', stid)
  instrument <- grep('lgr|licor', dir(base_path), value = T)[1]
  files <- tail(dir(file.path(base_path, instrument, 'calibrated'), full.names = T), nf)
  df <- rbindlist(lapply(files, fread, 
                         showProgress = F))
  df$stid <- stid
  df
}), fill = T)

data$trx <- rbindlist(lapply(trx_stids, function(stid) {
  base_path <- file.path('data', stid)
  instruments <- dir(base_path)
  gps <- fread(tail(dir(file.path(base_path, 'gps', 'qaqc'), full.names = 1), 1),
               select = c('Time_UTC', 'Lati_deg', 'Long_deg'))
  gps$Time_UTC <- fastPOSIXct(gps$Time_UTC, tz = 'UTC')
  gps <- gps %>%
    filter(Time_UTC > Sys.time() - 7200) %>%
    mutate(Time_UTC = as.POSIXct(trunc(Time_UTC, 'secs'))) %>%
    rename(lati = Lati_deg, long = Long_deg)
  if ('lgr_ugga' %in% instruments) {
    lgr_ugga <- fread(tail(dir(file.path(base_path, 'lgr_ugga', 'qaqc'), full.names = 1), 1),
                      select = c(1, 6, 7, 13, 14))
    lgr_ugga$Time_UTC <- fastPOSIXct(lgr_ugga$Time_UTC, tz = 'UTC')
    lgr_ugga <- lgr_ugga %>%
      filter(Time_UTC > Sys.time() - 7200,
             ID_CO2 == -10) %>%
      mutate(Time_UTC = as.POSIXct(trunc(Time_UTC, 'secs'))) %>%
      dplyr::select(-ID_CO2, -QAQC_Flag)
  }
  inner_join(gps, lgr_ugga, by = 'Time_UTC') %>%
    group_by(stid = stid,
             long = round(long, 3),
             lati = round(lati, 3)) %>%
    summarize(Time_UTC = max(Time_UTC, na.rm = T),
              CO2d_ppm = mean(CO2d_ppm, na.rm = T),
              CH4d_ppm = mean(CH4d_ppm, na.rm = T)) %>%
    na.omit()
}))

# Convert timestamps
data$qaqc$Time_UTC <- fastPOSIXct(data$qaqc$Time_UTC, tz = 'UTC')
data$calibrated$Time_UTC <- fastPOSIXct(data$calibrated$Time_UTC, tz = 'UTC')

# Filter by recent data
time_start <- Sys.time() - 10 * 86400
data$qaqc <- data$qaqc[data$qaqc$Time_UTC > time_start]
data$calibrated <- data$calibrated[data$calibrated$Time_UTC > time_start]

saveRDS(data, 'air.utah.edu/_data.rds')

