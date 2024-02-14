# C. Savonen

# Update workshop info from Slido
# Only needs to be rerun when another workshop with Slido responses is done

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

dir.create(file.path(root_dir, "data"), showWarnings = FALSE)
dir.create(file.path(root_dir, "plots"), showWarnings = FALSE)

set.seed(1234)

library(metricminer)
library("magrittr")

auth_from_secret("google",
                 refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                 access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                 cache = TRUE)

itcr_drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
itcr_slido_data <- get_slido_files(itcr_drive_id)
saveRDS(itcr_slido_data, file.path(root_dir, "data", "itcr_slido_data.RDS"))
