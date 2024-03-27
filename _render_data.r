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


data <- list()

# QAQC Data
data$qaqc <- rbindlist(lapply(stids, function(stid) {
  instrument_files <- get_instrument_files(stid, 'qaqc', subset = 'ghg')
  if (is.null(instrument_files)) return(NULL)
  instrument <- names(instrument_files)[1]
  columns <- c('Time_UTC', 'CO2d_ppm', 'CH4d_ppm', 'H2O_ppm',
               'Flow_mLmin', 'Cavity_P_torr',
               'ID_CO2', 'ID_CH4', 'QAQC_Flag')
  files <- instrument_files[[instrument]]
  df <- suppressWarnings(rbindlist(lapply(files, fread,
                                          showProgress = F,
                                          select = columns)))
  df$stid <- stid
  return(df)
}), fill = T)

# Calibrated Data
data$calibrated <- rbindlist(lapply(stids, function(stid) {
  instrument_files <- get_instrument_files(stid, 'calibrated', subset = 'ghg')
  if (is.null(instrument_files)) return(NULL)
  instrument <- names(instrument_files)[1]
  files <- instrument_files[[instrument]]
  df <- rbindlist(lapply(files, fread,
                         showProgress = F))
  df$stid <- stid
  return(df)
}), fill = T)

# Map Data
data$map <- rbindlist(lapply(stids, function(stid) {
  # Get final instrument files for non-ghg instruments
  non_ghg_files <- get_instrument_files(stid, 'final', subset = 'non-ghg')

  # Use QAQC data for GHG (final data only updates after calibration)
  ghg <- data$qaqc[ , data$qaqc[stid == ..stid]]

  # Check for GPS data
  is_mobile <- 'gps' %in% names(non_ghg_files)
  if (is_mobile) {
    gps_files <- non_ghg_files$gps
    non_ghg_files <- non_ghg_files[-grep('gps', names(non_ghg_files))]
  }

  if ((is.null(non_ghg_files) || length(non_ghg_files) == 0)
      & (is.null(ghg) || nrow(ghg) == 0)) return(NULL)

  # Get non-ghg instrument data
  inst_dfs <- lapply(seq_along(non_ghg_files), function(i) {
    instrument <- names(non_ghg_files)[i]
    files <- non_ghg_files[[instrument]]

    inst_df <- rbindlist(lapply(files, fread, showProgress = F))

    return(inst_df)
  }) %>%
    purrr::discard(is.null)

  # 'finalize' ghg data
  if (!is.null(ghg) && nrow(ghg) > 0) {
    ghg <- ghg %>%
      dplyr::filter(QAQC_Flag >= 0,
                    ID_CO2 == -10) %>%
      select(Time_UTC, CO2d_ppm, CH4d_ppm)

    inst_dfs <- c(list(ghg), inst_dfs)
  }

  # Combine instrument data
  df <- inst_dfs %>%
    rbindlist(fill = T) %>%
    # Gather data into long format
    gather(variable, value, -Time_UTC, na.rm = T)

  # Format time, truncate to seconds, and filter to past 10 days
  df$Time_UTC <- as.POSIXct(trunc(fastPOSIXct(df$Time_UTC, tz = 'UTC'),
                                  'secs'))
  df <- df[df$Time_UTC > ten_days_ago, ]

  if (nrow(df) == 0) return(NULL)

  df$stid <- stid
  df$is_mobile <- is_mobile

  if (is_mobile && length(gps_files) > 0) {
    # Merge with GPS data if mobile
    gps_cols <- c('Time_UTC', 'Pi_Time', 'Latitude_deg', 'Longitude_deg')
    gps <- rbindlist(lapply(gps_files, fread,
                            showProgress = F,
                            select = gps_cols))
    gps$Time_UTC <- fastPOSIXct(gps$Time_UTC, tz = 'UTC')
    gps$Pi_Time <- fastPOSIXct(gps$Pi_Time, tz = 'UTC')
    gps <- gps %>%
      filter(Time_UTC > two_hours_ago) %>%
      mutate(Pi_Time = as.POSIXct(trunc(Pi_Time, 'secs'))) %>% # Truncate to sec
      distinct(Pi_Time, .keep_all = T) %>%  # Remove duplicate times
      rename(lati = Latitude_deg, long = Longitude_deg)

    df <- df %>%
      rename(Pi_Time = Time_UTC) %>%  # rename to match gps
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

# Convert timestamps
data$qaqc$Time_UTC <- fastPOSIXct(data$qaqc$Time_UTC, tz = 'UTC')
data$calibrated$Time_UTC <- fastPOSIXct(data$calibrated$Time_UTC, tz = 'UTC')

# Filter by recent data
data$qaqc <- data$qaqc[data$qaqc$Time_UTC > ten_days_ago]
data$calibrated <- data$calibrated[data$calibrated$Time_UTC > ten_days_ago]

saveRDS(data, 'air.utah.edu/_data.rds')
