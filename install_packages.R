# Run from the AridHab-Q repository root.
required <- c("shiny", "jsonlite")
optional <- c("plotly") # interactive PCA graphics
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")
message("Optional interactive charts: install.packages('plotly')")
message("Launch: shiny::runApp('.')")
