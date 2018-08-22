# Ben Fasoli
setwd('/uufs/chpc.utah.edu/common/home/lin-group2/measurements-beta')

library(flexdashboard)
library(rmarkdown)

# Required to specify pandoc path for command line use
# CHPC has an old version of pandoc installed that fails
Sys.setenv('RSTUDIO_PANDOC' = '/usr/lib/rstudio-server/bin/pandoc')

render_flex <- function(src) {
  require(rmarkdown)
  
  # Flexdashboard initial html build
  file <- render(input = src,
                 output_file = paste0('../', tools::file_path_sans_ext(basename(src)), '.html'),
                 output_format = flex_dashboard(
                   css = '../styles.css',
                   includes = includes(
                     in_header = c('../_includes/head.html',
                                   '../_includes/head_refresh.html')
                   ),
                   orientation = 'rows',
                   vertical_layout = 'scroll'
                 )
  )
  html <- readLines(file)
  
  # Replace navigation with global _navbar
  nav  <- readLines('_navbar.html')
  delete <- c(grep('<div class="navbar navbar-inverse navbar-fixed-top"',
                   html, fixed = T),
              grep('</div><!--/.navbar-->', html, fixed = T))
  out <- html[1:(delete[1]-1)]
  out <- append(out, nav)
  out <- append(out, html[(delete[2]+1):length(html)])
  writeLines(out, file)
  file
}

source('air.utah.edu/_render_data.r')
setwd('/uufs/chpc.utah.edu/common/home/lin-group2/measurements-beta/air.utah.edu')

render_flex('_flexdashboard/status.Rmd')
render_flex('_flexdashboard/historic-co2.Rmd')

render_site(encoding = 'UTF-8')
system('rsync -avz _site/* benfasoli@air.utah.edu:/var/www/air.utah.edu/')
# system('rm -r _site historic-co2.html status.html')
