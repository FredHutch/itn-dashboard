library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(ggplot2)
library(fontawesome)
library(DT)

# Datasets ----

# ITCR Course data
itcr_course_data <- read_tsv(file.path("data", "itcr_course_metrics.tsv")) %>% 
  mutate(target_audience = replace_na(target_audience, "Everyone"))

itcr_course_data$webAndEnrollmentTotals <- itcr_course_data %>%
  select(website_count, coursera_count, leanpub_count) %>% rowSums(na.rm = TRUE)

itcr_course_data_long <- itcr_course_data %>% 
  select(c("website", 
           "totalUsers",
           "coursera_count", 
           "leanpub_count", 
           "target_audience")) %>%
  tidyr::pivot_longer(!c(website, target_audience),
                      names_to = "modality", 
                      values_to = "learner_count") %>%
  filter(!(website %in% c("ITN Website", "OTTR website", "metricminer.org"))) %>%
  mutate(modality = case_when(
    modality == "leanpub_count" ~ "Total Leanpub Enrollments", 
    modality == "coursera_count" ~ "Total Coursera Enrollments",
    modality == "totalUsers" ~ "Website Learners", 
    TRUE ~ modality
  ))

# Google Analytics metrics
ga_metrics <- readRDS(file.path("data","itcr_ga_metric_data.RDS"))

user_totals <- ga_metrics %>% 
  janitor::clean_names() %>% 
  select(website, active_users, average_session_duration)

user_engagement <- ga_metrics %>% 
  janitor::clean_names() %>% 
  select(website, screen_page_views_per_user, 
         sessions, screen_page_views, engagement_rate)

# Everyone, Leadership, new to data science, software developers
cbPalette <- c("#E69F02", "#56B4E9", "#009E73", "#008080") 


# OPEN Meeting Attendance
open_meeting_attendance <- read_csv("data/open_meeting_attendance.csv")

# CRAN Download Stats
cran_download_stats <- read_csv("data/cran_download_stats.csv")

xlabel_view <- c(rep(c("black", "transparent", "transparent", "transparent"), 41), "black", "transparent") #166 rows
#cc <- rev(c("#fde725", "#addc30", "#5ec962", "#28ae80", "#21918c", "#2c728e", "#3b528b", "#472d7b", "#440154"))
viridis_cc <- c("#440154", "#2c728e", "#28ae80", "#addc30")


link_itn <- tags$a(
  shiny::icon("house"), "ITN",
  href = "https://www.itcrtraining.org/home",
  target = "_blank"
)
link_code <- tags$a(
  shiny::icon("github"), "Code",
  href = "https://github.com/FredHutch/itn-dashboard",
  target = "_blank"
)
link_help <- tags$a(
  shiny::icon("circle-question"), "Help",
  href = "https://github.com/FredHutch/itn-dashboard/issues/new",
  target = "_blank"
)

# UI ----
ui <- page_navbar(
  # Favicon
  header = tags$head(tags$link(rel="shortcut icon", href="i/img/favicon.ico")),
  # Hard-code version of bootstrap used
  theme = bs_theme(version = 5),
  title = "ITN Dashboard",
  fillable = TRUE,
  nav_spacer(),
  nav_panel("Online Courses",
            layout_column_wrap(
              fill = TRUE,
              width = NULL,
              style = css(grid_template_columns = "1.2fr 1fr"),
              navset_card_underline(
                height = 900,
                full_screen = TRUE,
                title = NULL,
                nav_panel(
                  "Unique Visitors to Websites",
                  plotOutput("unique_visitor_website")
                ),
                nav_panel(
                  "Engagement Stats",
                  plotOutput("engagement_stat")
                ),
                nav_panel(
                  "Learners by Modality",
                  plotOutput("learner_by_modality")
                ),
                nav_panel(
                  "Learners by Course",
                  plotOutput("learner_by_course")
                ),
                nav_panel(
                  "Coursera Learners",
                  plotOutput("coursera_learner")
                ),
                nav_panel(
                  "Leanpub Learners",
                  plotOutput("leanpub_learner")
                ),
                nav_panel(
                  "Learners by Launch Date",
                  plotOutput("learner_by_launch_date")
                )
              ),
              navset_card_underline(
                height = 900,
                full_screen = TRUE,
                title = "Tables of User Data",
                nav_panel(
                  "User Totals",
                  DTOutput("user_totals")
                ),
                nav_panel(
                  "User Engagement",
                  DTOutput("user_engagement")
                )
              )
            )
            
  ),
  nav_panel("Workshops"),
  nav_panel("Software Usage",
            navset_card_underline(
              height = 900,
              full_screen = TRUE,
              title = NULL,
              nav_panel(
                "CRAN Monthly Downloads",
                plotOutput("cran_download_stats")
              )
            )),
  nav_panel("Collaborations",
            navset_card_underline(
              height = 900,
              full_screen = TRUE,
              title = NULL,
              nav_panel(
                "OPEN Meeting Attendance",
                plotOutput("open_meeting_attendance")
              )
            )
  ),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_itn),
    nav_item(link_code),
    nav_item(link_help)
  )
)


# Server ----
server <- function(input, output) {
  # Plots --------
  
  # Unique Visitors to Websites
  output$unique_visitor_website <- renderPlot({
    ggplot(itcr_course_data, aes(x = reorder(website, -totalUsers), 
                                 y = totalUsers, 
                                 fill = target_audience)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = totalUsers), size = 4, vjust = - 1) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = c(0.85, 0.85),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Total # of Visitors",
           fill = "Target Audience",
           title = "Visitor Distribution across Educational Resources") +
      ylim(c(0, 6000)) + 
      scale_fill_manual(values=cbPalette)
  })
  
  # Engagement Stats
  output$engagement_stat <- renderPlot({
    itcr_course_data %>% 
      janitor::clean_names() %>%
      select(website, screen_page_views_per_user, average_session_duration, 
             event_count_per_user, engagement_rate, target_audience) %>%
      tidyr::pivot_longer(!c(website, target_audience), 
                          names_to = "metric_name", 
                          values_to = "value") %>% 
      filter(!(website %in% c("ITN Website", "OTTR website", "metricminer.org"))) %>%
      ggplot(aes(x = website, y = value, fill = target_audience)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() +
      labs(x = NULL,
           y = NULL,
           fill = "Target Audience",
           title = "Website Engagement for each Course") +
      facet_wrap(~metric_name, scales = "free_y",
                 labeller = labeller(metric_name = c(screen_page_views_per_user = "Screen Page Views per User",
                                                     average_session_duration = "Average Session Duration",
                                                     event_count_per_user = "Event Count per User",
                                                     engagement_rate = "Engagement Rate"))) + 
      scale_fill_manual(values=cbPalette) +
      scale_x_discrete(limits = c("Leadership in Cancer Informatics", "NIH Data Sharing", "Ethical Data Handling", "Overleaf and Latex for Scientific Articles", "AI for Decision Makers",
                                  "Reproducibility in Cancer Informatics", "Choosing Genomics Tools", "Computing for Cancer Informatics",
                                  "Documentation and Usability", "Advanced Reproducibility", "AI for Efficient Programming", "GitHub Automation for Scientists")) +
      theme(axis.text.x=element_text(angle=90, hjust=1), 
            plot.margin = unit(c(1.5,.5,.5,1.5), "cm"),
            text = element_text(size = 17, family = "Arial"))
    
  })
  
  # Learners by Modality
  output$learner_by_modality <- renderPlot({
    itcr_course_data_long %>% 
      group_by(modality, target_audience) %>% 
      summarize(total_learners = sum(learner_count, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(modality, -total_learners), y = total_learners, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Visitors/Enrollees",
           fill = "Target Audience",
           title = "Course Engagement by Modality") +
      geom_text(aes(label = total_learners), size = 4, vjust = - 1, na.rm = TRUE) + 
      ylim(c(0, 4200)) + 
      facet_wrap(~target_audience) + 
      scale_fill_manual(values=cbPalette)
    
  })
  
  
  # Learner by Course
  output$learner_by_course <- renderPlot({
    itcr_course_data_long %>% 
      group_by(website, target_audience) %>% 
      summarize(total_learners = sum(learner_count, na.rm = TRUE)) %>%
      ggplot(aes(y = total_learners, x = reorder(website, -total_learners), fill = target_audience)) + 
      geom_bar(stat = "identity") + 
      labs(x = NULL, 
           y = "Total Learners by Course",
           fill = "Target Audience",
           title = "Total Number of Learners for each Course") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle = 70, hjust=1), 
            legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial")) + 
      geom_text(aes(label = total_learners), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1800)) + 
      scale_fill_manual(values=cbPalette)
  })
  
  
  
  # Coursera Learners
  output$coursera_learner <- renderPlot({
    ggplot(itcr_course_data %>% filter(coursera_count > 0), aes(x = reorder(website, -coursera_count), y = coursera_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(x = NULL,
           y = "Coursera enrollments",
           fill = "Target Audience",
           title = "Number of Coursera Enrollments by Course") +
      geom_text(aes(label = coursera_count), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1200)) + 
      scale_fill_manual(values=cbPalette) +
      theme(legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial"))
  })
  
  # Leanpub Learners
  output$leanpub_learner <- renderPlot({
    ggplot(itcr_course_data %>% filter(leanpub_count > 0) , aes(x = reorder(website, -leanpub_count), y = leanpub_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(x = NULL,
           y = "Leanpub enrollments",
           fill = "Target Audience",
           title = "Number of Leanpub Enrollments by Course") +
      geom_text(aes(label = leanpub_count), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 40)) + 
      scale_fill_manual(values=cbPalette) +
      theme(legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial"))
  })
  
  # Learners by Launch Date
  output$learner_by_launch_date <- renderPlot({
    itcr_course_data %>% 
      filter(!(website %in% c("ITN Website", "OTTR website", "metricminer.org"))) %>%
      mutate(duration = today() - website_launch) %>%
      ggplot(aes(x = duration, y = webAndEnrollmentTotals, color = target_audience)) + 
      geom_point() + 
      theme(panel.grid = element_line("black", linewidth = 0.25), 
            panel.background = element_blank(), 
            panel.border = element_rect("black", fill=NA, linewidth=0.5),
            legend.position = c(0.918, 0.25),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = "How long the course has been out",
           y = "Bookdown Views + Coursera & Leanpub Enrollments",
           color = "Target Audience",
           title = "Course Popularity over Time") +
      scale_color_manual(values=cbPalette) + 
      ggrepel::geom_text_repel(aes(x = duration, y = webAndEnrollmentTotals, label = website), size = 6, vjust = - 1, na.rm = TRUE)
  })
  
  # CRAN Download Stats
  output$cran_download_stats <- renderPlot({
    cran_download_stats %>% 
      ggplot(aes(Month, monthly_downloads, group=package, color = package)) + 
      geom_line() + 
      geom_point() +
      scale_colour_manual(values=viridis_cc) +
      theme(panel.background = element_blank(), 
            panel.grid = element_blank(),
            text = element_text(size = 17, family = "Arial")) +
      geom_vline(aes(xintercept = "2019-05"), linetype='dashed', color = '#addc30') + #text2speech published date
      geom_vline(aes(xintercept="2022-02"), linetype='dashed', color = '#28ae80') + #ottrpal published date 
      geom_vline(aes(xintercept="2023-07"), linetype='dashed', color = '#2c728e') + #conrad published date
      theme(axis.text.x = element_text(angle = 90)) +
      theme(axis.text.x=element_text(color= xlabel_view),
            legend.position = c(0.05, 0.9)) + #clean up x-axis labels
      labs(x = NULL,
           y = "Monthly Downloads",
           color = "R Packages")
  })
  
  
  # OPEN Meeting Attendance
  output$open_meeting_attendance <- renderPlot({
    open_meeting_attendance %>% 
      ggplot(aes(x = date, y = attendance)) + 
      geom_bar(stat = "identity", fill = "lightgreen") +
      geom_text(aes(label = attendance), size = 4, vjust = - 1) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL, 
           y = "Attendance",
           title = "OPEN Meeting Attendance by Month")
  })
  
  
  # Tables --------
  
  # User Totals
  output$user_totals <- renderDT({
    datatable(
      user_totals, 
      colnames = c("Website", "Active Users", "Avg Session Duration"),
      options = list(lengthChange = FALSE, # remove "Show X entries"
                     searching = FALSE), # remove Search box
      # For the table to grow/shrink
      fillContainer = TRUE,
      escape = FALSE
    )
  })
  
  # User Engagement
  output$user_engagement <- renderDT({
    datatable(
      user_engagement, 
      colnames = c("Website", "Screen Page Views per User", "Sessions",
                   "Screen Page Views", "Engagement Rate"),
      options = list(lengthChange = FALSE, # remove "Show X entries"
                     searching = FALSE), # remove Search box
      # For the table to grow/shrink
      fillContainer = TRUE,
      escape = FALSE
    )
  })
  
  
}

# Code for Deployment to Hutch servers
addResourcePath("/i", file.path(getwd(), "www"))
options <- list()
if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"
  
}

shinyApp(ui, server, options=options)

