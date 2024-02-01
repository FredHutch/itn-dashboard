library(dplyr)
library(tidyr)
library(readr)
library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(ggplot2)
library(fontawesome)
library(DT)

# ITCR Course data
itcr_course_data <- read_tsv(file.path("data", "itcr_course_metrics.tsv")) %>% 
  mutate(target_audience = replace_na(target_audience, "Everyone"))

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

# TODO: Dynamic Rendering
# https://rstudio.github.io/bslib/articles/value-boxes/index.html#dynamic-rendering-shiny

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
  nav_panel("Software Usage"),
  nav_panel("Collaborations"),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_itn),
    nav_item(link_code),
    nav_item(link_help)
  )
)

server <- function(input, output) {
  # Unique Visitors to Websites
  output$unique_visitor_website <- renderPlot({
    ggplot(itcr_course_data, aes(x = reorder(website, -totalUsers), 
                                 y = totalUsers, 
                                 fill = target_audience)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = totalUsers), size = 3, vjust = - 1) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(x = NULL,
           y = "Total # of Visitors",
           fill = "Target Audience") +
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
           fill = "Target Audience") +
      facet_wrap(~metric_name, scales = "free_y",
                 labeller = labeller(metric_name = c(screen_page_views_per_user = "Screen Page Views per User",
                                                     average_session_duration = "Average Session Duration",
                                                     event_count_per_user = "Event Count per User",
                                                     engagement_rate = "Engagement Rate"))) + 
      scale_fill_manual(values=cbPalette) +
      scale_x_discrete(limits = c("Leadership in Cancer Informatics", "NIH Data Sharing", "Ethical Data Handling", "Overleaf and Latex for Scientific Articles", "AI for Decision Makers",
                                  "Reproducibility in Cancer Informatics", "Choosing Genomics Tools", "Computing for Cancer Informatics",
                                  "Documentation and Usability", "Advanced Reproducibility", "AI for Efficient Programming", "GitHub Automation for Scientists")) +
      theme(axis.text.x=element_text(angle=60, hjust=1), 
            strip.text.x = element_text(size = 8), 
            plot.margin = unit(c(1.5,.5,.5,1.5), "cm"))
    
  })
  
  # Learners by Modality
  output$learner_by_modality <- renderPlot({
    itcr_course_data_long %>% 
      group_by(modality, target_audience) %>% 
      summarize(total_learners = sum(learner_count, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(modality, -total_learners), y = total_learners, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(x = NULL,
           y = "Visitors/Enrollees",
           fill = "Target Audience") +
      geom_text(aes(label = total_learners), size = 3, vjust = - 1, na.rm = TRUE) + 
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
           fill = "Target Audience") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1), 
            strip.text.x = element_text(size = 8)) + 
      geom_text(aes(label = total_learners), size = 3, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1800)) + 
      scale_fill_manual(values=cbPalette)
  })
  
  
  
  # Coursera Learners
  output$coursera_learner <- renderPlot({
    ggplot(itcr_course_data %>% filter(coursera_count > 0), aes(x = reorder(website, -coursera_count), y = coursera_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(x = NULL,
           y = "Coursera enrollments",
           fill = "Target Audience") +
      geom_text(aes(label = coursera_count), size = 3, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1200)) + 
      scale_fill_manual(values=cbPalette)
  })
  
  # Leanpub Learners
  output$leanpub_learner <- renderPlot({
    ggplot(itcr_course_data %>% filter(leanpub_count > 0) , aes(x = reorder(website, -leanpub_count), y = leanpub_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(x = NULL,
           y = "Leanpub enrollments",
           fill = "Target Audience") +
      geom_text(aes(label = leanpub_count), size = 3, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 40)) + 
      scale_fill_manual(values=cbPalette)
  })
  
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

