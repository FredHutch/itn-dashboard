# Download that ITN data so that it can be used by the dashboard.
library(metricminer)
library(magrittr)
library(httr2)
library(googlesheets4)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

dir.create(file.path(root_dir, "data"), showWarnings = FALSE)
dir.create(file.path(root_dir, "plots"), showWarnings = FALSE)

set.seed(1234)

auth_from_secret("google",
                 refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                 access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                 cache = TRUE)

auth_from_secret("github", token = Sys.getenv("METRICMINER_GITHUB_PAT"))

ga_accounts <- get_ga_user()

fhdsl_account_id <- ga_accounts %>%
  filter(name == "fhDaSL") %>%
  pull(id)

itcr_account_id <- ga_accounts %>%
  filter(name == "itcrtraining") %>%
  pull(id)

fhdsl_stats_raw <- get_multiple_ga_metrics(account_id = fhdsl_account_id)
fhdsl_stats <- fhdsl_stats_raw$metrics

itcr_stats_raw <- get_multiple_ga_metrics(account_id = itcr_account_id)
itcr_stats <- itcr_stats_raw$metrics

# Filter out Google Analytics that aren't ITCR courses
not_itcr <- c("hutchdatasci", "whoiswho", "MMDS", "FH Cluster 101", "DaSL Collection",  "proof",
              "Developing_WDL_Workflows", "ITN Website", "OTTR website", "metricminer.org", "widget", "Using Leanpub Course")

# itcr_ga_metric_data.csv ----------------------------------------------------
itcr_ga_metric_data <- fhdsl_stats %>%
  bind_rows(itcr_stats) %>%
  filter(!(website %in%not_itcr))

# Save as CSV
readr::write_csv(itcr_ga_metric_data, file.path(root_dir,"data", "itcr_ga_metric_data.csv"))





# itcr_course_metrics.csv ----------------------------------------------------
# TODO: Where did the website_count column come from?
manual_course_info <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit#gid=1550012125", sheet = "Course_data",
  col_types = "ccDDDciii") %>%
  mutate_if(is.numeric.Date, lubridate::ymd)

# Join this all together
itcr_course_metrics <- itcr_ga_metric_data %>%
  dplyr::left_join(manual_course_info) %>%
  dplyr::mutate(website = dplyr::case_when(
    website == "Advanced Reproducibility in Cancer Informatics" ~ "Advanced Reproducibility",
    TRUE ~ website))

# Save these to CSVs
readr::write_csv(itcr_course_metrics, file.path(root_dir,"data", "itcr_course_metrics.csv"))




# itcr_slido_data.csv ----------------------------------------------------

# ITCR Google Drive
itcr_drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
itcr_slido_data_raw <- get_slido_files(itcr_drive_id)
itcr_slido_data <- itcr_slido_data_raw$`Polls-per-user`

  
# Save these to CSVs
readr::write_csv(itcr_slido_data, file.path(root_dir,"data", "itcr_slido_data.csv"))


# cran_download_stats.csv ----------------------------------------------------

# CRAN Download Stats
download_stats <- cranlogs::cran_downloads(packages = c("ottrpal", "conrad", "ari", "text2speech", "metricminer"), 
                                           # first version of ari published on 2017-08-31 
                                           from = "2017-08-31",
                                           to = "last-day") 
# Saved as `download_stats.csv`
download_stats_processed <- download_stats %>% 
  separate(date, into=c("year", "month name", "day"), sep = "-") %>% 
  unite("Month", c("year", "month name"), sep='-', remove=TRUE) %>%  
  # cran_downloads returns the daily downloads,
  # Some prep work to turn that into monthly downloads
  group_by(Month, package) %>% 
  # summarize monthly downloads by package
  summarise(monthly_downloads = sum(count)) %>%
  # drop the 0's 
  filter(monthly_downloads > 0) %>% 
  ungroup()

# Save this to a TSV
readr::write_csv(download_stats_processed, file.path(root_dir,"data", "cran_download_stats.csv"))

sessionInfo()