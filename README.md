# AridHab-Q — Integrated Habitat Degradation Assessment

# AridHab-Q: Integrated Habitat Degradation Assessment

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23697818.svg)](https://doi.org/10.5281/zenodo.23697818)

**Developer:** Dr. Manish Mathur  
**Affiliation:** ICAR–Central Arid Zone Research Institute, Jodhpur, India  
**Version:** 2.0.2  
**Software DOI:** https://doi.org/10.5281/zenodo.23697818

## Citation

Mathur, M. (2026). AridHab-Q: Integrated Habitat
Degradation Assessment (Version 2.0.2)
[Computer software]. Zenodo.
https://doi.org/10.5281/zenodo.23697818
  

**Dr. Manish Mathur**, ICAR-Central Arid Zone Research Institute, Jodhpur, India.
Contact: eco5320@gmail.com; manish.mathur@icar.org.in.

Interactive R Shiny software with 37 illustrated attributes, four components, color-coded navigation, site metadata, rating evidence, autosave, site-wise records, CSV/JSON import/export, multi-site analyses, and optional interactive PCA. The software includes a color illustration for each attribute at `www/attribute_guides/attribute_01.png` through `attribute_37.png`.

## Run

```r
install.packages(c("shiny", "jsonlite"))
install.packages("plotly") # optional
shiny::runApp(".")
```

## Scoring direction — IMPORTANT

Ratings are 1 (least degradation) to 5 (most degradation). Normalized degradation `d_i = 25(r_i - 1)`; each component is the mean of its normalized degradation ratings; `HQI_37` is the equal mean of four components, **only for complete 37-rating assessments**. Higher HQI = more degradation. Provisional bands: 0–25 None; >25–50 Slight; >50–70 Moderate; >70–90 Severe; >90–100 Very severe. These are *not validated ecological thresholds*. Original 25 Kumar-derived ratings and new 12 proposed functional ratings are distinguished.

## Files

- `app.R` — complete working application source (unchanged from latest supplied working release)
- `scorecard.csv`, `ecological_extension.csv` — rating descriptions
- `www/` — color illustrations and application photos
- `manuscript/` — final methodological manuscript and supplementary field guide
- `docs/COMMANDS_AND_CODE.md` — R and Git commands
- `docs/ZENODO_RELEASE.md` — Zenodo archiving and DOI steps
- `install_packages.R`, `run_app.R` — local setup

**Testing disclosure:** The author reports having tested the application with various datasets. A reproducible test matrix and independently verified results were not supplied with these two methodological references. This release should not be described as independently validated.

**License:** The existing LICENSE is retained unchanged; verify code, photographs and illustration reuse rights before distributing or relicensing.
