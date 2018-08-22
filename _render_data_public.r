#!/uufs/chpc.utah.edu/sys/installdir/R/3.4.2i/bin/Rscript
# Ben Fasoli
setwd('~/links/lin-group2/measurements-beta/')

library(data.table)
library(fasttime)
library(fst)
library(tidyverse)

stids <- dir('data')
stids <- grep('trx', stids, value = T, invert = T)

data <- rbindlist(lapply(stids, function(stid) {
  message('Running ', stid, '...')
  base_path <- file.path('data', stid)
  instrument <- grep('lgr|licor', dir(base_path), value = T)[1]
  
  path <- file.path(base_path, instrument, 'calibrated')
  files <- dir(path, full.names = T)
  if (length(files) < 1) next
  
  calibrated <- rbindlist(lapply(files, fread, showProgress = F))
  calibrated$Time_UTC <- fastPOSIXct(calibrated$Time_UTC, tz = 'UTC')
  
  calibrated %>%
    filter(QAQC_Flag >= 0, ID_CO2 == -10) %>%
    group_by(Time_UTC = as.POSIXct(trunc(Time_UTC, 'hours'), tz = 'UTC')) %>%
    summarize(CO2d_ppm_avg = mean(CO2d_ppm_cal, na.rm = T),
              CO2d_ppm_sd = sd(CO2d_ppm_cal, na.rm = T),
              CO2d_ppm_n = length(CO2d_ppm_cal[!is.na(CO2d_ppm_cal)]),
              CH4d_ppm_avg = ifelse('CH4d_ppm_cal' %in% names(.),
                                    mean(CH4d_ppm_cal, na.rm = T), NA),
              CH4d_ppm_sd = ifelse('CH4d_ppm_cal' %in% names(.),
                                   sd(CH4d_ppm_cal, na.rm = T), NA),
              CH4d_ppm_n = ifelse('CH4d_ppm_cal' %in% names(.),
                                  length(CH4d_ppm_cal[!is.na(CH4d_ppm_cal)]), NA)) %>%
    mutate(stid = stid)
}))

data <- as_tibble(data)
format(object.size(data), 'MB')
write_fst(data, 'air.utah.edu/_data_public.fst')
