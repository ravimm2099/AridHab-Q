# R and Git commands

## Install and run in R console
```r
install.packages(c("shiny", "jsonlite"))
install.packages("plotly") # optional interactive PCA
setwd("PATH/TO/AridHab-Q")
source("install_packages.R")
shiny::runApp(".")
```

## Optional deployment
```r
install.packages("rsconnect")
rsconnect::deployApp(appDir=".", appName="AridHab-Q")
```
Authentication and a deployment account are required.

## GitHub commands (terminal)
```bash
git init
git add .
git commit -m "Release AridHab-Q application and field guide"
git branch -M main
git remote add origin https://github.com/OWNER/AridHab-Q.git
git push -u origin main
```

## Computation as implemented
```r
# r is a complete 37-element vector of degradation ratings
d <- 25 * (r - 1)
D_E <- mean(d[1:7]); D_H <- mean(d[8:17])
D_W <- mean(d[18:25]); D_F <- mean(d[26:37])
HQI <- mean(c(D_E, D_H, D_W, D_F))
# Only report final HQI if every rating is available.
```
