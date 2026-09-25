# Run this locally from the app directory before deploying to Posit Connect Cloud.
# Connect Cloud requires manifest.json for R content.

needed <- c("shiny", "httr2", "jsonlite", "rsconnect")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
cat("Created manifest.json\n")
