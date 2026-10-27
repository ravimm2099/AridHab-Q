# Update: Multi-site dashboard and interactive PCA

## Navigation order
1 Start & site metadata; 2 Data import & site inventory; 3 Multi-site CSV & validation; 4 Entire vegetation; 5 Herbaceous; 6 Woody; 7 Ecosystem functional integrity; 8 Intelligent scoring guidance; 9 Current habitat status & scorecard; 10 Attribute statistics; 11 PCA; 12 Methodology.

## Bulk CSV and dashboard
Upload a wide CSV in tab 3 and confirm import. The first imported site is selected for the dashboard, and any other saved site can be selected from the dashboard's site dropdown. Ratings marked invalid are missing and do not produce final HQI. The live working assessment remains independent. All-site inventory refreshes after import.

## PCA
Install optional plotly (`install.packages("plotly")`) for zoomable hover biplot with site scores, eigenvector arrows and attribute labels. Tables include eigenvalues, explained and cumulative variance, site scores, loadings, and variable contributions. PCA is exploratory on standardized ordinal ratings and requires at least four complete sites.

## Limitations
R runtime was not available in the build environment. Run a live test in RStudio, including multi-site import and dashboard selection. Back up records before importing: matching site IDs are overwritten.
