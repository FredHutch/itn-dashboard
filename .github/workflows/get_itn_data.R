# C. Savonen

# Download that ITN data so that it can be used by the dashboard.

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
    !(website %in%not_itcr)
    )
saveRDS(ga_metrics, file.path(root_dir, "data","itcr_ga_metric_data.RDS"))

ga_dims <- dplyr::bind_rows(fhdsl_stats_list$dimensions, itcr_stats_list$dimensions) %>%
  dplyr::filter(
    !(website %in% not_itcr)
    )
saveRDS(ga_dims, file.path(root_dir, "data","itcr_ga_dims_data.RDS"))

ga_link_clicks <- dplyr::bind_rows(fhdsl_stats_list$link_clicks,itcr_stats_list$link_clicks) %>%
  dplyr::filter(
    !(website %in% not_itcr)
    )
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

gh_metrics <- get_repos_metrics(repo_names = repos)

saveRDS(gh_metrics, file.path(root_dir,"data", "itcr_gh_metrics.RDS"))

sessionInfo()