# Zenodo archiving checklist

1. Create a GitHub repository and upload all contents of AridHab-Q. Do not upload the repository ZIP as the only file.
2. Run the app locally, check all 37 illustrated guides and test site saving/import/export before tagging.
3. Update the placeholder GitHub repository URL in CITATION.cff.
4. Connect the repository to Zenodo at https://zenodo.org and enable archiving for it.
5. Create a GitHub release tag (e.g. v2026.10); Zenodo should archive the release and mint a DOI.
6. Insert the minted DOI in the manuscript, README and CITATION.cff; do not invent a DOI.
7. Attach the manuscript and Supplement S1 to the Zenodo record if desired.
8. Check code/photo rights before assigning a public reuse license. The included LICENSE currently reserves rights.

GitHub Pages does not run Shiny server-side R; deploy to shinyapps.io, Posit Connect, or Shiny Server.
