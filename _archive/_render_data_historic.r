# # Ben Fasoli
# rm(list = ls())
# 
# library(tidyverse)
# 
# site_abbv <- c('dbk', 'imc', 'rpk', 'sug', 'wbb')
# 
# # Import Historic Data c. Logan Mitchell ---------------------------------------
# path <- '/uufs/chpc.utah.edu/common/home/lin-group2/measurements/archive/slcco2'
# site_abbv_lm <- c('DBK', 'MUR', 'RPK', 'SUG', 'UOU')
# 
# lm <- lapply(site_abbv_lm, function(site) {
#   message(site)
#   dir(path, pattern = paste0('CO2_', site), full.names = T) %>%
#     lapply(read_csv, skip = 50, col_types = 'iiiiiidii') %>%
#     bind_rows() %>%
#     filter(`QAQC descriptor flag` == 0) %>%
#     mutate(Time = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second),
#                              tz = 'UTC', format = '%Y %m %d %H %M %S')) %>%
#     select(Time, CO2_ppm = `CO2 (ppm)`) %>%
#     filter(format(Time, tz = 'America/Denver', '%H') %>%
#              as.numeric() %in% 12:14) %>%
#     mutate(Time = as.Date(Time)) %>%
#     group_by(Time) %>%
#     summarize(CO2_ppm = mean(CO2_ppm, na.rm = T),
#               Site = site_abbv[site_abbv_lm == site])
# }) %>%
#   bind_rows()
# 
# 
# # Import Modern Data c. Ben Fasoli ---------------------------------------------
# path <- '/uufs/chpc.utah.edu/common/home/lin-group2/measurements/data'
# 
# meas <- lapply(site_abbv, function(site) {
#   message(site)
#   inst <- dir(file.path(path, site), pattern = 'licor-6262|lgr-ugga')
#   col_types <- switch(inst,
#                       'licor-6262' = 'Td______c',
#                       'lgr-ugga' = 'Td_____________c')
#   file.path(path, site, inst, 'calibrated') %>%
#     dir(full.names = T) %>%
#     lapply(read_csv, col_types = col_types, locale = locale(tz = 'UTC')) %>%
#     bind_rows() %>%
#     rename(Time = Time_UTC, CO2_ppm = CO2d_ppm_cal, Site = site_id) %>%
#     filter(format(Time, tz = 'America/Denver', '%H') %>%
#              as.numeric() %in% 12:14) %>%
#     mutate(Time = as.Date(Time)) %>%
#     group_by(Time) %>%
#     summarize(CO2_ppm = mean(CO2_ppm, na.rm = T),
#               Site = site)
# }) %>%
#   bind_rows()
# 
# agg <- bind_rows(lm, meas) %>%
#   arrange(Site, Time)
# 
# saveRDS(agg, '_data_historic.rds')
