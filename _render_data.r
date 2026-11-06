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

  if (length(site_data) == 0) return(NULL)

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


# Instrument diagnostics -------------------------------------------------------
# Every instrument at an active site, greenhouse gas and air quality alike,
# for the status grid and the species tabs on status.html. Which variables
# each instrument is checked on, and their limits, are read from the
# pipeline's QAQC_Flags.md, so the page shows the same checks the pipeline makes.

squash <- function(x) gsub('[^a-z0-9]', '', tolower(x))

match_section <- function(section, instruments) {
  # The instrument folders a QAQC_Flags.md section belongs to, by name:
  # "Teledyne T200" -> teledyne_t200, "Licor 7000 IRGA" -> licor_7000, or by
  # initials, "Los Gatos Research UGGA" -> lgr_ugga and lgr_ugga_manual_cal.
  # A folder matches when one name starts with the other. A new instrument
  # needs no change here if its section is named like this.
  words <- strsplit(section, ' ')[[1]]
  initials <- paste0(paste(substr(words[-length(words)], 1, 1), collapse = ''),
                     words[length(words)])
  names <- squash(c(section, initials))
  instruments[sapply(squash(instruments), function(f) {
    any(startsWith(f, names) | startsWith(names, f))
  })]
}

species_name <- function(var) sub('d$', '', sub('_.*', '', var))  # 'CO2d_ppm' -> 'CO2'

read_qaqc_checks <- function(instruments, file = 'pipeline/QAQC_Flags.md') {
  # One row per flag and instrument, e.g. for
  #   | -81 | `Flow_Lmin` flow rate out of range (1.9, 2.1) |
  # flag = -81, variable = 'Flow_Lmin', lo = 1.9, hi = 2.1
  rows <- grep('^\\|', readLines(file, warn = F), value = T)
  cells <- str_split_fixed(rows, '\\|', 4)
  flag <- str_trim(cells[, 2])
  description <- str_trim(cells[, 3])

  # Section rows look like "| ↓ | **Teledyne T200** |"
  is_section <- str_detect(description, '^\\*\\*')
  sections <- str_remove_all(description[is_section], '\\*')

  range <- str_match(description, '\\((-?[0-9.]+),\\s*(-?[0-9.]+)\\)')
  variable <- str_match(description, '^`([^`]+)`')[, 2]
  checks <- data.table(
    name = c(NA, sections)[cumsum(is_section) + 1],
    flag = suppressWarnings(as.integer(flag)),
    variable = variable,
    lo = as.numeric(range[, 2]),
    hi = as.numeric(range[, 3]),
    # The measured species are the "measurement out of range" checks; water
    # vapour is a diagnostic for the greenhouse gas analyzers, kept with the checks
    is_species = str_detect(description, 'measurement out of range') &
      !str_detect(variable, '^H2O'),
    description = description
  )[str_detect(flag, '^-?[0-9]+$')]

  section_folders <- rbindlist(lapply(sections, function(sec) {
    data.table(name = sec, instrument = match_section(sec, instruments))
  }))
  merge(checks, section_folders, by = 'name', allow.cartesian = T)
}


build_instrument_data <- function() {
  # Every instrument folder at an active site whose QAQC_Flags.md section
  # checks a measured species (not gps or met)
  folders <- unique(unlist(lapply(stids, function(stid) {
    names(get_instrument_files(stid, 'qaqc'))
  })))
  checks <- read_qaqc_checks(folders)
  instruments <- unique(checks$instrument[checks$is_species])
  checks <- checks[instrument %in% instruments]

  # Read each instrument's qaqc data from the past 10 days
  raw <- list()
  for (stid in stids) {
    files <- get_instrument_files(stid, 'qaqc')
    files <- files[names(files) %in% instruments]
    for (instrument in names(files)) {
      df <- rbindlist(lapply(files[[instrument]], fread, showProgress = F),
                      fill = T)
      df$Time_UTC <- fastPOSIXct(df$Time_UTC, tz = 'UTC')
      df <- df[Time_UTC > ten_days_ago]
      if (nrow(df) == 0) next
      df$stid <- stid
      raw[[instrument]] <- rbind(raw[[instrument]], df, fill = T)
    }
  }
  if (length(raw) == 0) return(NULL)

  # Keep a species value only where it passed QAQC and, for analyzers that
  # also measure reference tanks (an ID_<species> column), where it is the
  # atmosphere (ID -10)
  species_only_good <- function(df, species) {
    for (v in species) {
      bad <- df$QAQC_Flag < 0
      id <- paste0('ID_', species_name(v))
      if (id %in% names(df)) bad <- bad | is.na(df[[id]]) | df[[id]] != -10
      set(df, which(bad), v, NA_real_)
    }
    df
  }

  # Hourly means: the species from good rows only, the variables the
  # pipeline checks from all rows (to see why rows fail)
  obs <- lapply(names(raw), function(inst) {
    df <- copy(raw[[inst]])  # copy: changes below would change raw too
    inst_checks <- checks[instrument == inst]
    vars <- intersect(inst_checks$variable, names(df))
    vars <- setdiff(vars, 'Time_UTC')
    species <- intersect(inst_checks$variable[inst_checks$is_species], vars)

    df[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
    df <- species_only_good(df, species)
    df[, Time_UTC := as.POSIXct(trunc(Time_UTC, 'hours'))]
    df[, lapply(.SD, mean, na.rm = T), by = .(stid, Time_UTC), .SDcols = vars]
  })
  names(obs) <- names(raw)

  # Last time each instrument reported, and its flags over the past day
  everything <- rbindlist(lapply(names(raw), function(instrument) {
    raw[[instrument]][, .(stid, instrument = instrument, Time_UTC, QAQC_Flag)]
  }))
  last_seen <- everything[, .(last_seen = max(Time_UTC)), by = .(stid, instrument)]
  last_seen$is_ghg <- grepl(paste(ghg_instruments, collapse = '|'), last_seen$instrument)
  flags <- everything[Time_UTC > today - 86400, .N,
                      by = .(stid, instrument, QAQC_Flag)]

  # Each species over the past day, good rows only: its point-to-point noise
  # (as `co2 ptp` for the greenhouse gases) and its mean over the latest hour
  species_day <- rbindlist(lapply(names(raw), function(inst) {
    species <- checks[instrument == inst & is_species, unique(variable)]
    species <- intersect(species, names(raw[[inst]]))
    df <- copy(raw[[inst]][Time_UTC > today - 86400])
    df[, (species) := lapply(.SD, as.numeric), .SDcols = species]
    df <- species_only_good(df, species)
    rbindlist(lapply(species, function(v) {
      df[!is.na(get(v)), .(instrument = inst, variable = v,
                           ptp = mean(abs(diff(get(v)))),
                           latest = mean(get(v)[Time_UTC > max(Time_UTC) - 3600])),
         by = stid]
    }))
  }))

  list(checks = checks, obs = obs, last_seen = last_seen, flags = flags,
       species_day = species_day)
}

# A failure here must not stop the rest of the site from updating
data$instruments <- tryCatch(build_instrument_data(), error = function(e) {
  message('Instrument diagnostics data failed: ', conditionMessage(e))
  NULL
})


saveRDS(data, 'air.utah.edu/_data.rds')
