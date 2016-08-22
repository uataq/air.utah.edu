# Ben Fasoli
library(dplyr)
library(flexdashboard)
library(ggplot2)
library(readr)

setwd('/home/benfasoli/cron/air.utah.edu/')

# Dashboard setup --------------------------------------------------------------
sites <- dir('/projects/data') %>%
  grep(pattern = 'trx|csp|mur', x = ., invert = T, value = T)

cal <- lapply(sites, function(site) {
  paths <- file.path('/projects/data', site, 'calibrated') %>%
    dir(full.names = T) %>%
    tail(2)
  col_types <- read_lines(paths[1], n_max = 1) %>%
    grepl('CH4d_ppm_cal', x = .) %>%
    (function(x) {
      if (x) return('Tddddddddddddddc')
      else return('Tdddddddc')
    })
  df <- paths %>%
    lapply(read_csv, locale = locale(tz = 'UTC'), col_types = col_types,
           progress = F) %>%
    bind_rows
  
  if (nrow(df) < 100)
    return(NULL)
  
  if ('rmse_ch4' %in% colnames(df)) {
    df <- df %>% select(Time_UTC, rmse_co2, rmse_ch4)
  } else {
    df <- df %>% select(Time_UTC, rmse_co2) %>% mutate(rmse_ch4 = NA)
  }
  df %>%
    mutate(site) %>%
    filter(Time_UTC >= Sys.time() - 7 * 24 * 3600) %>%
    return
}) %>%
  bind_rows()

n <- 24 * 7 # Hourly resolution
small <- cal %>%
  # mutate(rmse_co2 = uataq::run_smooth(rmse_co2, n = n),
  #        rmse_ch4 = uataq::run_smooth(rmse_ch4, n = n)) %>%
  do(.[trunc(seq(from = 1, to = nrow(.), length.out = n * length(sites))), ])%>%
  mutate(rmse_ch4 = rmse_ch4 * 1000) %>%
  rename(Time_Mountain = Time_UTC)

# CO2 --------------------------------------------------------------------------
max_rmse <- 2.0
bad <- small %>% filter(rmse_co2 > max_rmse)
bad_sites <- unique(bad$site)
if (length(bad_sites) > 0) {
  txt <- paste(sep = '\n', collapse = '',
               paste0('RMSE > ', max_rmse, 'ppm: '),
               paste(bad_sites, sep = ', ', collapse = ', '))
} else txt <- ''
f <- ggplot(data = small, aes(x = Time_Mountain, y = rmse_co2, color = site)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim = c(0, max_rmse)) +
  annotate('text', x = Sys.time() - 5 * 24 * 3600,
           y = max_rmse * 0.8, label = txt, size = 3.5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(size = 10))
saveRDS(f, 'data/rmse_co2.rds')


# CH4 --------------------------------------------------------------------------
max_rmse <- 10
bad <- small %>%
  filter(rmse_ch4 > max_rmse)
bad_sites <- unique(bad$site)
if (length(bad_sites) > 0) {
  txt <- paste(sep = '\n', collapse = '',
               paste0('RMSE > ', max_rmse, 'ppb: '),
               paste(bad_sites, sep = ', ', collapse = ', '))
} else txt <- ''
f <- ggplot(data = small, aes(x = Time_Mountain, y = rmse_ch4, color = site)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL) +
  coord_cartesian(ylim = c(0, max_rmse)) +
  annotate('text', x = Sys.time() - 5 * 24 * 3600,
           y = max_rmse * 0.8, label = txt, size = 3.5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(size = 10))
saveRDS(f, 'data/rmse_ch4.rds')


# Stats ------------------------------------------------------------------------
out <- lapply(sites, function(site) {
  # Import all parsed data
  path <- file.path('/projects/data', site, 'parsed') %>%
    dir(full.names = T) %>%
    tail(1)
  col_types <- read_lines(path, n_max = 1) %>%
    grepl('CH4d_ppm', x = .) %>%
    (function(x) {
      if (x) return('Tdddddddddddddddddddddccdd')
      else return('Tdddddddddddddddddddcddc')
    })
  parsed <- read_csv(path, locale=locale(tz='UTC'), col_types = col_types,
                     progress = F) %>%
    filter(ID_co2 != -1,
           ID_co2 != -2,
           ID_co2 != -3)
  
  tmp <- parsed %>%
    filter(ID_co2 > 0)
  if (nrow(tmp) < 1)
    return( data_frame(site) )
  
  if ('CH4d_ppm' %in% colnames(tmp)) {
    tmp <- tmp %>%
      mutate(diff_co2 = abs(CO2d_ppm - ID_co2),
             diff_ch4 = abs(CH4d_ppm - ID_ch4))
  } else {
    tmp <- tmp %>%
      mutate(diff_co2 = abs(CO2d_ppm - ID_co2),
             diff_ch4 = NA)
  }
  meandiff_co2 <- mean(tmp$diff_co2, na.rm = T)
  meandiff_ch4 <- mean(tmp$diff_ch4, na.rm = T)
  
  # Data recovery rate ---------------------------------------------------------
  # Find elapsed time in raw data since the start of the dataset and compare to
  # the amount of time covered by 10-s calibrated measurements
  nsec_smp <- parsed %>%
    (function(x) {
      len <- nrow(x)
      dt = as.numeric(x$Time_UTC[2:len]) - as.numeric(x$Time_UTC[1:(len-1)])
      # Define observations as having <100 seconds between
      sampling <- dt < 100
      sum(dt[sampling], na.rm = T)
    })
  
  # CALIBRATION TESTING --------------------------------------------------------
  # UATAQ Calibration routine
  # https://github.com/benfasoli/uataq/blob/master/R/calibrate.r
  # Import all calibrated data
  path <- file.path('/projects/data', site, 'calibrated') %>%
    dir(full.names = T) %>%
    tail(1)
  col_types <- read_lines(path, n_max = 1) %>%
    grepl('CH4d_ppm', x = .) %>%
    (function(x) {
      if (x) return('Tddddddddddddddc')
      else return('Tdddddddc')
    })
  cal <- read_csv(path, locale=locale(tz='UTC'), col_types = col_types,
                  progress = F)
  n_co2 <- tail(cal$n_co2, 1)
  cal <- cal
  
  # Result output --------------------------------------------------------------
  # Output results to data_frame
  out <- data_frame(
    site,
    nsec_smp,
    n_co2    = n_co2,
    bias_co2 = meandiff_co2,
    rmse_co2 = mean(cal$rmse_co2, na.rm = T)
  )
  if ('rmse_ch4' %in% colnames(cal)) {
    out$bias_ch4 <- meandiff_ch4
    out$rmse_ch4 <- mean(cal$rmse_ch4, na.rm = T)
  }
  out
})

result <- bind_rows(out) %>%
  mutate(nsec_total = as.numeric(Sys.time()) -
           Sys.time() %>%
           strftime('%Y-%m-01', tz = 'UTC') %>%
           as.POSIXct(tz = 'UTC') %>%
           as.numeric)
result <- bind_rows(result,
                    data_frame(
                      site = 'Total',
                      nsec_smp = sum(result$nsec_smp),
                      nsec_total = sum(result$nsec_total),
                      bias_co2 = mean(result$bias_co2, na.rm = T),
                      bias_ch4 = mean(result$bias_ch4, na.rm = T),
                      rmse_co2 = mean(result$rmse_co2, na.rm = T),
                      rmse_ch4 = mean(result$rmse_ch4, na.rm = T)
                    )
) %>%
  mutate(data_recovery_rate = nsec_smp / nsec_total)
saveRDS(result, 'data/stats.rds')


render_air <- function() {
  # Compile status page ----------------------------------------------------------
  tmp_path <- rmarkdown::render(input = './_dash_src.Rmd',
                                output_file = './.tmp.html',
                                output_format = flex_dashboard(
                                  css = 'styles.css',
                                  orientation = 'rows'
                                ))
  
  html <- read_lines(tmp_path)
  system(paste('rm', tmp_path))
  nav  <- read_lines('_navbar.html')
  delete <- c(
    grep('<div class="navbar navbar-inverse navbar-fixed-top" role="navigation">', 
         html, fixed=T),
    grep('</div><!--/.navbar-->', html))
  out <- html[1:(delete[1]-1)]
  out <- append(out, nav)
  out <- append(out, html[(delete[2]+1):length(html)])
  write_lines(out, 'status.html')
  
  # Compile webpage --------------------------------------------------------------
  rmarkdown::render_site(encoding = 'UTF-8')
  system('rm status.html')
  system(paste('cp -R _site/* /var/www/air.utah.edu/'))
}

render_air()
