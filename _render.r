#!/usr/bin/env Rscript
# Ben Fasoli

library(flexdashboard)
library(rmarkdown)

print(paste('R version: ', R.home()))

# Required to specify pandoc path for command line use
# CHPC has an old version of pandoc installed that fails
Sys.setenv('RSTUDIO_PANDOC' = '/usr/lib/rstudio-server/bin/pandoc')

setwd('/uufs/chpc.utah.edu/common/home/lin-group9/measurements')
source('air.utah.edu/_render_data.r')
setwd('/uufs/chpc.utah.edu/common/home/lin-group9/measurements/air.utah.edu')

source('_render_flex.r')
render_flex('_flexdashboard/status.Rmd')
# render_flex('_flexdashboard/historic-co2.Rmd')

render_site(encoding = 'UTF-8')
system('cp -r _site/* ~/public_html/air.utah.edu/')
system('rm -r historic-co2.html historic-co2_files status.html status_files')

