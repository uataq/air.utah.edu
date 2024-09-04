# Ben Fasoli
# Process data for website render

library(data.table)
library(fasttime)

source('pipeline/_global.r')

# Determine site list
stids <- site_config$stid[site_config$active]

# Determine months to read
today <- Sys.time()
if (as.numeric(format(today, '%d')) > 10) {
  months <- format(today, '%Y_%m')
} else {
  months <- c(today - 86400 * 10, today) %>%
    format('%Y_%m')
}

# Recent data filters
ten_days_ago <- today - 10 * 86400  # stationary data
two_hours_ago <- today - 7200  # mobile data

# Greenhouse gas instruments
ghg_instruments <- c('lgr_ugga', 'licor_7000', 'licor_6262')

# Meta columns
meta_cols <- c('Time_UTC', 'QAQC_Flag')
gps_cols <- c('Pi_Time', 'Latitude_deg', 'Longitude_deg')

# GHG columns
ghg_cols <- c('CO2d_ppm', 'CH4d_ppm', 'H2O_ppm',
              'Flow_mLmin', 'Cavity_P_torr',
              'ID_CO2', 'ID_CH4')

# Non GHG variables
non_ghg_vars <- c('O3_ppb', 'NO2_ppb', 'NO_ppb', 'PM2.5_ugm3', 'CO_ppb', 'BC6_ngm3')


get_instrument_files <- function(stid, lvl, subset = 'all') {
  base_path <- file.path('data', stid)

  instruments <- list.files(base_path)
  if (subset != 'all') {
    is_ghg <- grepl(paste(ghg_instruments, collapse = '|'), instruments)
    if (subset == 'ghg') {
      instruments <- instruments[is_ghg]
    } else if (subset == 'non-ghg') {
      instruments <- instruments[!is_ghg]
    } else stop('Invalid subset')
  }
  if (length(instruments) == 0) return(NULL)

  files <- lapply(instruments, function(inst) {
    files <- file.path(base_path, inst, lvl, paste0(months, '_', lvl, '.dat'))
    files <- files[sapply(files, file.exists)]
    return(files)
  })

  # Remove instruments without files
  has_files <- sapply(files, length) > 0
  files <- files[has_files]
  if (length(files) == 0) return(NULL)

  names(files) <- instruments[has_files]

  return(files)
}


merge_instrument_data <- function(instrument_files, select = NULL) {
  if (is.null(instrument_files)) return(NULL)
  df <- suppressWarnings(rbindlist(lapply(unlist(instrument_files), fread,
                                          showProgress = F,
                                          select = select),
                                          fill = T))
  return(df)
}


data <- list()

# QAQC Data
data$qaqc <- rbindlist(lapply(stids, function(stid) {
  instrument_files <- get_instrument_files(stid, 'qaqc', subset = 'ghg')
  df <- merge_instrument_data(instrument_files, select = c(meta_cols, ghg_cols))
  if (is.null(df)) return(NULL)
  df$stid <- stid
  return(df)
}), fill = T)

# Calibrated Data
data$calibrated <- rbindlist(lapply(stids, function(stid) {
  instrument_files <- get_instrument_files(stid, 'calibrated', subset = 'ghg')
  df <- merge_instrument_data(instrument_files)
  if (is.null(df)) return(NULL)
  df$stid <- stid
  return(df)
}), fill = T)

# Convert timestamps
data$qaqc$Time_UTC <- fastPOSIXct(data$qaqc$Time_UTC, tz = 'UTC')
data$calibrated$Time_UTC <- fastPOSIXct(data$calibrated$Time_UTC, tz = 'UTC')

# Filter by recent data
data$qaqc <- data$qaqc[data$qaqc$Time_UTC > ten_days_ago]
data$calibrated <- data$calibrated[data$calibrated$Time_UTC > ten_days_ago]

# Map Data
data$map <- rbindlist(lapply(stids, function(stid) {
  # Get qaqc instrument files for non-ghg instruments
  non_ghg_files <- get_instrument_files(stid, 'qaqc', subset = 'non-ghg')
  # Check for GPS data
  is_mobile <- 'gps' %in% names(non_ghg_files)
  if (is_mobile) {
    gps_files <- non_ghg_files$gps
    non_ghg_files <- non_ghg_files[-grep('gps', names(non_ghg_files))]
  }

  site_data <- list()

  # Use QAQC data for GHG (final data only updates after calibration)
  ghg <- data$qaqc[ , data$qaqc[stid == ..stid]]
  # Drop calibration data and filter ghg columns
  if (!is.null(ghg) && nrow(ghg) > 0) {
    ghg <- ghg %>%
      dplyr::filter(ID_CO2 == -10) %>%
      select(Time_UTC, CO2d_ppm, CH4d_ppm, QAQC_Flag)
    site_data$ghg <- ghg
  }

  # Use QAQC data for non-ghg instruments
  if (!is.null(non_ghg_files) && length(non_ghg_files) > 0) {
    # Get non-ghg data
    non_ghg <- merge_instrument_data(non_ghg_files, select = c(meta_cols, non_ghg_vars))
    # Format non-ghg time, truncate to seconds, and filter to past 10 days
    non_ghg$Time_UTC <- fastPOSIXct(non_ghg$Time_UTC, tz = 'UTC')
    non_ghg <- non_ghg[non_ghg$Time_UTC > ten_days_ago, ]
    site_data$non_ghg <- non_ghg
  }

  df <- site_data %>%
    # Merge data
    rbindlist(fill = T) %>%
    # Filter out bad data
    # dplyr::filter(QAQC_Flag >= 0) %>%
    select(-QAQC_Flag) %>%
    # Gather data into long format
    gather(variable, value, -Time_UTC, na.rm = T)

  if (nrow(df) == 0) return(NULL)

  df$stid <- stid
  df$is_mobile <- is_mobile

  if (is_mobile && length(gps_files) > 0) {
    # Merge with GPS data if mobile
    gps <- rbindlist(lapply(gps_files, fread,
                            showProgress = F,
                            select = c(meta_cols, gps_cols)))
    gps$Time_UTC <- fastPOSIXct(gps$Time_UTC, tz = 'UTC')
    gps$Pi_Time <- fastPOSIXct(gps$Pi_Time, tz = 'UTC')
    gps <- gps %>%
      dplyr::filter(QAQC_Flag >= 0,  # Filter out bad gps data
                    Time_UTC > two_hours_ago) %>%
      select(-QAQC_Flag) %>%
      mutate(Pi_Time = as.POSIXct(trunc(Pi_Time, 'secs'))) %>% # Truncate to sec
      distinct(Pi_Time, .keep_all = T) %>%  # Remove duplicate times
      rename(lati = Latitude_deg, long = Longitude_deg)

    df <- df %>%
      rename(Pi_Time = Time_UTC) %>%  # rename to match gps
      mutate(Pi_Time = trunc(Pi_Time, 'secs')) %>%  # Truncate to sec
      inner_join(gps, by = 'Pi_Time') %>%  # join on Pi_Time
      select(-Pi_Time) %>%
      # Keep only the most recent point for each location
      group_by(long = round(long, 3),
               lati = round(lati, 3)) %>%
      slice_max(order_by = Time_UTC) %>%
      ungroup()

  } else if (!is_mobile) {
    # Get site location from config
    df <- df %>%
      inner_join(dplyr::select(site_config, stid, lati, long), by = 'stid')
  } else return(NULL)

  return(arrange(df, Time_UTC))  # Sort each site's data by time
}), fill = T)


saveRDS(data, 'air.utah.edu/_data.rds')
