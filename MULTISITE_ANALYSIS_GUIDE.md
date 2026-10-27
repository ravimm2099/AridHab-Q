# AridHab-Q multi-site field analysis update

## Uploading a multi-site CSV

Open **Multi-site CSV & validation**, upload a wide CSV with one row per site, Site ID in the first column and the 37 attributes in the exact application order in the following columns. The supplied `Testing data of 36 sites.csv` follows this layout. Review the validation report before checking the confirmation box and importing. Values not in the published/proposed allowed rating categories are flagged and stored as missing. Original data are never rewritten. Importing an existing Site ID overwrites its saved JSON record; back up `records/` before import.

The bulk import preserves site IDs and ratings, but no coordinates, observer, dates or photographs are supplied by the 38-column source file. Add these in the Site Metadata tab as needed. Saved site JSON files are on the host computer in `records/`; use persistent disk storage on hosted deployments.

## Analyses

- **Attribute statistics & frequency**: counts and percentages of ratings 1–5; sample mean and standard deviation, moment skewness and excess kurtosis. The Kolmogorov–Smirnov normality p-value is exploratory because mean and SD are estimated from the same data and ratings have ties. Shapiro–Wilk is included. Ordinal ratings do not satisfy continuous-normal assumptions automatically.
- **Two-group comparison**: manually select independent sets of sites (at least two valid ratings per group) for Welch's t-test and the Mann–Whitney test. A site is not a replicate group by itself. For 37 separate hypothesis tests, account for multiplicity (e.g., Benjamini–Hochberg) in formal research reporting; exploratory p-values should not be presented as confirmatory evidence.
- **PCA**: complete sites only; standardized nonconstant attributes; points are sites and attribute loadings are plotted separately. Ordinal PCA is exploratory; consider polychoric correlation methods in formal validation work.
- **Intelligent scoring guidance**: displays ratings of ecologically related attributes and a nonbinding consistency prompt. No rating is forbidden and no score is automatically imputed. The field evidence and the specific attribute's allowed classes are authoritative.

## Known limitations

No R runtime was available in the build environment. This release has been source-inspected and packaged, but it needs live R Shiny verification before production deployment. In particular, test CSV import, reload, missing-value handling, two-group tests and PCA with a complete dataset.

R packages: `shiny` and `jsonlite` (the app uses base R statistical functions).
