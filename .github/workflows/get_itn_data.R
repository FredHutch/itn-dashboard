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
  dplyr::filter(name == "fhDaSL") %>%
  dplyr::pull(id)

itcr_account_id <- ga_accounts %>%
  dplyr::filter(name == "itcrtraining") %>%
  dplyr::pull(id)

fhdsl_stats_list <- get_multiple_ga_metrics(account_id = fhdsl_account_id)
itcr_stats_list <- get_multiple_ga_metrics(account_id = itcr_account_id)

# There's some google analytics that aren't ITCR courses
not_itcr <- c("hutchdatasci", "whoiswho", "MMDS", "FH Cluster 101", "AnVIL_Researcher_Journey")

# itcr_ga_metric_data.csv ----
itcr_ga_metric_data <- dplyr::bind_rows(fhdsl_stats_list$metrics ,itcr_stats_list$metrics) %>%
  dplyr::filter(!(website %in%not_itcr))

# Save as CSV
readr::write_csv(itcr_ga_metric_data, file.path(root_dir,"data", "itcr_ga_metric_data.csv"))





# itcr_course_metrics.csv ----
manual_course_info <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit#gid=1550012125", sheet = "Course_data",
  col_types = "ccDDDciii") %>%
  dplyr::mutate_if(is.numeric.Date, lubridate::ymd)

# Join this all together
itcr_course_metrics <- itcr_ga_metric_data %>%
  dplyr::left_join(manual_course_info) %>%
  dplyr::mutate(website = dplyr::case_when(
    website == "Advanced Reproducibility in Cancer Informatics" ~ "Advanced Reproducibility",
    TRUE ~ website))

# Save these to CSVs
readr::write_csv(itcr_course_metrics, file.path(root_dir,"data", "itcr_course_metrics.csv"))




# itcr_slido_data.csv ----

# ITCR Google Drive
itcr_drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
itcr_slido_data_raw <- get_slido_files(itcr_drive_id)
itcr_slido_data <- itcr_slido_data_raw$`Polls-per-user`

# Save these to CSVs
readr::write_csv(itcr_slido_data, file.path(root_dir,"data", "itcr_slido_data.csv"))





# cran_download_stats.csv ----

# CRAN Download Stats
download_stats <- cranlogs::cran_downloads(packages = c("ottrpal", "conrad", "ari", "text2speech"), 
                                           # first version of ari published on 2017-08-31 
                                           from = "2017-08-31",
                                           to = "last-day") 
# Saved as `download_stats.csv`
download_stats_to_plot <- download_stats %>% 
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
readr::write_csv(download_stats_to_plot, file.path(root_dir,"data", "cran_download_stats.csv"))




# open_meeting_attendance.csv -----

# Function that creates OAuth client for Google APIs
google_client <- function() {
  httr2::oauth_client(
    # Client ID and Secret for itcrtrainingnetwork@gmail.com
    id = Sys.getenv("GOOGLE_CLIENT_ID"),
    secret = Sys.getenv("GOOGLE_CLIENT_SECRET"),
    token_url = "https://oauth2.googleapis.com/token",
    name = "itn-metric-google-docs"
  )
}

# Create a new HTTP request
open_agenda_url <- "https://docs.googleapis.com/v1/documents/1sPVsWeBZKn9A8sbfM666aRYwxVq5Emrhp0ws_R5X1n4"
req <- request(open_agenda_url)

# Authenticate and perform request
resp <- req %>%
  req_oauth_auth_code(
    client = google_client(),
    auth_url = "https://accounts.google.com/o/oauth2/v2/auth",
    scope = "https://www.googleapis.com/auth/documents"
  ) %>%
  req_perform()

# Extract content from Google Doc
resp_body_json <- resp %>% resp_body_json()
content <- resp_body_json$body$content

# Initialize a list to store attendees, keyed by their meeting dates
result <- list()

# Iterate through content of the document and extracts names of attendees
# by grabbing the values that come after the line 'When you join the meeting, please enter your name and institution below:'
for (ii in seq(1, length(content))) {
  if (!is.null(content[[ii]]$paragraph$elements)) {
    for (jj in seq(1, length(content[[ii]]$paragraph$elements))) {
      if (!is.null(content[[ii]]$paragraph$elements[[jj]]$textRun$content) &&
          (content[[ii]]$paragraph$elements[[jj]]$textRun$content == "When you join the meeting, please enter your name and institution below:\n" |
           content[[ii]]$paragraph$elements[[jj]]$textRun$content == "When you join the meeting, please enter your name and institution below:")) {
        kk <- ii + 1
        while (!is.null(content[[kk]]$paragraph$elements[[1]]$textRun$content) && content[[kk]]$paragraph$elements[[1]]$textRun$content != "\n") {
          result <- c(result, content[[kk]]$paragraph$elements[[1]]$textRun$content)
          kk <- kk + 1
        }
      }
    }
  } else {
    next
  }
}

# Remove '\n' after names
open_meeting_attendance <- gsub("\n$", "", result)

open_meeting_attendance <- data.frame(names = open_meeting_attendance)

# Save this to a CSV
readr::write_csv(open_meeting_attendance, file.path(root_dir, "data", "open_meeting_attendance.csv"))

sessionInfo()