# AridHab-Q — enhanced field workflow

## Requirements
R with `shiny` and `jsonlite`: `install.packages(c('shiny','jsonlite'))`. Open `app.R` from the `AridHabQ` folder and click Run App.

## Saving work
1. Enter a unique **Site ID** and site metadata (including latitude and longitude in decimal degrees).
2. Rate an indicator and click **Save assessment**. With a Site ID entered, each saved rating automatically updates a JSON record under `records/<site-id>.json`.
3. Click **Save / update this site** to save changed metadata, even if no rating has been recorded.
4. Reopen via **Open a saved site → Load selected site**. For portability, download a JSON assessment backup on the results tab. JSON restoration is on **Data import & site inventory**.
5. Keep the `records/` folder when updating the application; do not publish private field records to GitHub.

**Important:** An unsaved selection is not committed. Site records are stored on the R server's filesystem, not automatically in the browser. On ephemeral cloud hosting, download JSON backups or configure persistent storage. Each site ID has one current saved record; using the same ID overwrites that record.

## Navigation
Each component offers Previous, Next and Next unrated. Progress is visible above the tabs. The current rating selection shows an immediate normalized score; click Save assessment to commit it. The app retains its original radio-based rating chooser to avoid changing established categories.

## Field photos
Select a Site ID first, then upload a JPEG or PNG on the site tab. Images are stored under `records/photos/<site-id>/`. They are not embedded in the downloaded JSON or CSV; back up that folder separately.

## CSV template and imports
The new Data import tab downloads a CSV with all 37 IDs and their names. Columns `id`, `rating`, `measurement`, `evidence` are used on import; `attribute` is informational. Ratings must be valid for each indicator according to the app's category definitions. Imported data replace the currently displayed assessment; save the site to persist it. Exported complete-assessment CSVs can also be imported.

## Inventory
Each saved site is summarized with ID, name, date, coordinates, rated count, four component qualities, and final HQI when complete. Export the inventory as CSV. A grouped component chart compares saved sites. No final HQI is calculated for incomplete sites.

## Interpretation
The original 25 indicators retain their original rating categories and original degradation total. The integrated 37-indicator HQI uses four equal-weight component means, only when all indicators have been assessed. Illustrations remain advisory where generated image text conflicts with the app's categories.

## Limitations
This enhanced release has been statically inspected but not executed in R in the build environment (Rscript was unavailable). Test the save/restore flow for indicators 1, 8, 18, and 26 before production field deployment.
