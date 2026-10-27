Startup reactive-context fix (25 October 2026)

In app.R, refresh_sites() now increments site_revision using isolate(site_revision()). This prevents the Shiny startup error "Operation not allowed without an active reactive context" at the initial refresh_sites() call. The rating and HQI calculations are unchanged.

Close the old app, extract this ZIP to a new folder, and launch app.R. Live R execution was unavailable in the build environment.
