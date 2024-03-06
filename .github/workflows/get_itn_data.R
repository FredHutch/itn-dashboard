library(metricminer)
library(magrittr)
library(httr2)
library(googlesheets4)
library(purrr)
library(stringr)

# Download that ITN data so that it can be used by the dashboard.

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

fhdsl_stats_list <- get_all_ga_metrics(account_id = fhdsl_account_id)
itcr_stats_list <- get_all_ga_metrics(account_id = itcr_account_id)

# There's some google analytics that aren't ITCR courses
not_itcr <- c("hutchdatasci", "whoiswho", "MMDS", "FH Cluster 101", "AnVIL_Researcher_Journey")

# Set up each data frame
ga_metrics <- dplyr::bind_rows(fhdsl_stats_list$metrics ,itcr_stats_list$metrics) %>%
  dplyr::filter(
    !(website %in%not_itcr))

# Save RDS
saveRDS(ga_metrics, file.path(root_dir, "data","itcr_ga_metric_data.RDS"))

ga_dims <- dplyr::bind_rows(fhdsl_stats_list$dimensions, itcr_stats_list$dimensions) %>%
  dplyr::filter(
    !(website %in% not_itcr))

# Save RDS
saveRDS(ga_dims, file.path(root_dir, "data","itcr_ga_dims_data.RDS"))

ga_link_clicks <- dplyr::bind_rows(fhdsl_stats_list$link_clicks,itcr_stats_list$link_clicks) %>%
  dplyr::filter(
    !(website %in% not_itcr))


# Save RDS
saveRDS(ga_link_clicks, file.path(root_dir, "data","itcr_ga_link_click_data.RDS"))

manual_course_info <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit#gid=1550012125", sheet = "Course_data",
  col_types = "ccDDDciii") %>%
  dplyr::mutate_if(is.numeric.Date, lubridate::ymd)

# Join this all together
itcr_course_data <- ga_metrics %>%
  dplyr::left_join(manual_course_info) %>%
  dplyr::mutate(website = dplyr::case_when(
    website == "Advanced Reproducibility in Cancer Informatics" ~ "Advanced Reproducibility",
    TRUE ~ website))

# Save these to TSVs
readr::write_tsv(itcr_course_data, file.path(root_dir,"data", "itcr_course_metrics.tsv"))

# ITCR Google Drive
itcr_drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
itcr_slido_data <- get_slido_files(itcr_drive_id)

# Save RDS
saveRDS(itcr_slido_data, file.path("data", "itcr_slido_data.RDS"))

collabs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit#gid=0")

# Save this to a TSV
readr::write_tsv(collabs, file.path(root_dir, "data", "collabs.tsv"))

loqui_usage <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit#gid=0")

# Save this to a TSV
readr::write_tsv(loqui_usage, file.path(root_dir,"data", "loqui_usage.tsv"))

career_stage_counts <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit#gid=8290691", range = "Workshop attendee type")
readr::write_tsv(career_stage_counts, file.path(root_dir, "data", "career_stage_counts.tsv"))

repos <- c(
  "fhdsl/metricminer",
  "fhdsl/metricminer.org",
  "jhudsl/ottrpal",
  "jhudsl/ari",
  "jhudsl/cow",
  "jhudsl/ottrproject.org",
  "jhudsl/ottr_docker",
  "jhudsl/ottr-reports",
  "fhdsl/conrad",
  "jhudsl/text2speech",
  "jhudsl/OTTR_Quizzes",
  "jhudsl/OTTR_Template",
  "jhudsl/OTTR_Template_Website",
  "jhudsl/ITCR_Tables",
  "jhudsl/ITN_Platforms",
  "fhdsl/Choosing_Genomics_Tools",
  "jhudsl/Informatics_Research_Leadership",
  "jhudsl/Documentation_and_Usability",
  "jhudsl/Reproducibility_in_Cancer_Informatics",
  "jhudsl/Adv_Reproducibility_in_Cancer_Informatics",
  "fhdsl/GitHub_Automation_for_Scientists",
  "jhudsl/Computing_for_Cancer_Informatics",
  "fhdsl/Overleaf_and_LaTeX_for_Scientific_Articles",
  "fhdsl/Ethical_Data_Handling_for_Cancer_Research",
  "fhdsl/AI_for_Decision_Makers",
  "fhdsl/AI_for_Efficient_Programming",
  "fhdsl/NIH_Data_Sharing"
  #"FredHutch/loqui"
)

gh_metrics <- get_multiple_repos_metrics(repo_names = repos)

saveRDS(gh_metrics, file.path(root_dir,"data", "itcr_gh_metrics.RDS"))


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
readr::write_tsv(download_stats_to_plot, file.path(root_dir,"data", "cran_download_stats.tsv"))




# OPEN Agenda Attendees

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

# Save this to a TSV
readr::write_tsv(open_meeting_attendance, file.path(root_dir, "data", "open_meeting_attendance.tsv"))

sessionInfo()