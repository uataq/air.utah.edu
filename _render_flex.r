render_flex <- function(src, ...) {
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
                   self_contained = F,
                   vertical_layout = 'scroll'
                 ), ...
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
  
  #file
}
