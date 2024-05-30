# Packages ----------------------------------------------------
library(shiny)
library(shinydashboard)

library(tidyverse)
library(janitor)

library(udpipe)
library(wordcloud)
library(bsicons)
library(htmltools)

library(fontawesome)
library(DT)

library(googlesheets4)
gs4_deauth()

# Everyone, Leadership, new to data science, software developers
cbPalette <- c("#E69F02", "#56B4E9", "#009E73", "#008080") 

xlabel_view <- c(rep(c("black", "transparent", "transparent", "transparent"), 41), "black", "transparent") #166 rows
#cc <- rev(c("#fde725", "#addc30", "#5ec962", "#28ae80", "#21918c", "#2c728e", "#3b528b", "#472d7b", "#440154"))
viridis_cc <- c("#440154", "#2c728e", "#28ae80", "#addc30")

# Wordcloud 
ud_model <- udpipe::udpipe_load_model("wordcloud-model.udpipe")

# Time interval (ms)
time_interval <- 604800000

# Links
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


ui <- dashboardPage(
  # Dashboard Header ----------------------------------------------------
  dashboardHeader(
    title = "ITN Dashboard",
    # TODO: Dropdown menu for notification (Last Data Uploaded: )
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("users"), status = "info",
                                  "5 new members joined today"
                 ),
                 notificationItem(icon = icon("warning"), status = "danger",
                                  "Resource usage near limit."
                 ),
                 notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                  status = "success", "25 sales made"
                 ),
                 notificationItem(icon = icon("user", lib = "glyphicon"),
                                  status = "danger", "You changed your username"
                 )
    )
  ),
  # Dashboard Sidebar ----------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Courses", tabName = "tab_courses", icon = icon("chalkboard")),
      menuItem("Workshops", tabName = "tab_workshops", icon = icon("people-group")),
      menuItem("Software Usage", tabName = "tab_software_usage", icon = icon("robot")),
      menuItem("Collaborations", tabName = "tab_collaborations", icon = icon("people-arrows")),
      menuItem("About", tabName = "tab_about", icon = icon("info"))
    )
  ),
  # Dashboard Body ----------------------------------------------------
  dashboardBody(
    # Custom style
    includeCSS("www/style.css"),
    # Favicon
    tags$head(tags$link(rel="shortcut icon", href="i/img/favicon.ico")),
    
    tabItems(
      # Courses Tab ----------------------------------------------------
      tabItem(tabName = "tab_courses",
              # First row
              fluidRow(
                box(title = "Visitors across Educational Resources",
                    plotOutput("plot_visitor_website")),
                
                box(title = "Engagement by Modality",
                    selectInput("modality", "Modality:",
                                c("Website" = "website", 
                                  "Leanpub" = "leanpub",
                                  "Coursera" = "coursera")),
                    plotOutput("plot_engagement_modality"))
              ),
              # Second row
              fluidRow(
                box(title = "Website Engagement",
                    selectInput("metric", "Metric:",
                                c("Screen Page Views per User" = "screen_page_views_per_user", 
                                  "Average Session Duration" = "average_session_duration",
                                  "Event Count per User" = "event_count_per_user",
                                  "Engagement Rate" = "engagement_rate")),
                    plotOutput("plot_engagement_website")),
                
                box(title = "Course Engagement by Target Audience",
                    plotOutput("plot_engagement_target"))
              ),
              # Third row
              fluidRow(
                box(title = "Learners by Course",
                    plotOutput("plot_learner_course")),
                
                box(title = "Coursera Learners",
                    plotOutput("plot_coursera"))
              ),
              # Fourth row
              fluidRow(
                box(title = "Leanpub Learners",
                    plotOutput("plot_leanpub")),
                
                box(title = "Learners by Launch Date",
                    plotOutput("plot_learner_launch_date"))
              )
      ),
      # Workshops Tab ----------------------------------------------------
      tabItem(tabName = "tab_workshops",
              # First Row
              fluidRow(
                box(title = "Workshop Recommendation",
                    plotOutput("plot_workshop_recommendation")),
                
                box(title = "Workshop Relevance",
                    plotOutput("plot_workshop_relevance"))
              ),
              # Second Row
              fluidRow(
                box(title = "Workshop Registrant Career Stage",
                    width = 12,
                    plotOutput("plot_workshop_career_stage"))
              ),
              # Third Row
              fluidRow(
                box(title = "Workshop Review: What Did You Like Most?",
                    plotOutput("plot_workshop_review")),
                box(title = "Workshop Recommendations for Improvement",
                    plotOutput("plot_workshop_improvement"))
              )
      ),
      # Software Usage Tab ----------------------------------------------------
      tabItem(tabName = "tab_software_usage",
              # First Row
              fluidRow(),
              # Second Row
              fluidRow(
                box(title = "Monthly CRAN Downloads",
                    width = 12,
                    plotOutput("plot_monthly_cran_download"))
              )
      )
    )
  )
)

# Server ----------------------------------------------------
server <- function(input, output) {
  # Data: ITCR Course ----------------------------------------------------
  itcr_course_data_raw <- reactiveFileReader(time_interval, 
                                             NULL,
                                             "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/itcr_course_metrics.csv",
                                             readr::read_csv)
  
  itcr_course_data <- reactive({
    itcr_course_data <- itcr_course_data_raw() %>% 
      mutate(target_audience = replace_na(target_audience, "Everyone"))
    
    itcr_course_data$webAndEnrollmentTotals <- itcr_course_data %>%
      select(website_count, coursera_count, leanpub_count) %>% 
      rowSums(na.rm = TRUE)
    
    itcr_course_data
  })
  
  itcr_course_data_long <- reactive({ 
    itcr_course_data() %>% 
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
  })
  
  # Data: Course Engagement by Modality ----------------------------------------------------
  course_raw <- reactiveFileReader(time_interval,
                                   NULL,
                                   "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit?usp=sharing",
                                   googlesheets4::read_sheet,
                                   sheet = "engagement overall") 
  
  
  course_processed <- reactive({
    course_raw() %>% 
      pivot_longer(cols = contains("count"), names_to = "modality", values_to = "number_of_learners") %>%
      mutate(course_name = factor(course_name)) %>%
      separate(modality, sep = "_", into = c("modality", "meh")) %>% 
      mutate(modality = factor(modality, levels = c("website", "leanpub", "coursera"),
                               labels = c("website", "leanpub", "coursera"))) %>%
      mutate(course_order = case_when(course_type == "Leadership" ~ 1,
                                      course_type == "New to data" ~ 2,
                                      course_type == "Software developers" ~ 3)) %>%
      rename("Target Audience" =  course_type) %>%
      filter(modality == input$modality)
    
  })
  
  # Data: ITCR Slido ----------------------------------------------------
  itcr_slido_data <- reactiveFileReader(time_interval,
                                        NULL,
                                        "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/itcr_slido_data.csv",
                                        readr::read_csv) 
  
  # Data: Workshop Registrant Career Stage ----------------------------------------------------
  career_stage_counts_raw <- reactiveFileReader(time_interval, 
                                                NULL,
                                                "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit?usp=sharing",
                                                googlesheets4::read_sheet,
                                                range = "Copy of Workshop attendee type")
  
  career_stage_counts_summed <- reactive({
    tmp <- career_stage_counts_raw() %>%
      select(-1) %>% 
      slice(1:(n() - 1))
    
    colSums(tmp)
  })
  
  career_stage_processed <- reactive({
    career_stage_processed <- data.frame(
      Stage = names(career_stage_counts_summed()),
      count = as.numeric(career_stage_counts_summed()),
      stringsAsFactors = FALSE
    )
    
    career_stage_processed$Trainee <- ifelse(career_stage_processed$Stage %in% c("Phd student", 
                                                                                 "postdoc",
                                                                                 "Master's student",
                                                                                 "Research tech",
                                                                                 "undergrad"), 
                                             "yes",
                                             "no")
    career_stage_processed
  })
  
  # Data: Poll Results ---------------------------------------------------
  poll_data <- reactive({
    itcr_slido_data() %>%
      clean_names() %>% 
      mutate(merged_likely_rec = if_else(is.na(how_likely_would_you_be_to_recommend_this_workshop), how_likely_would_you_be_to_recommend_this_workshop_2, 
                                         how_likely_would_you_be_to_recommend_this_workshop))
  })
  
  
  poll_results <- reactive({
    udpipe::udpipe_annotate(ud_model, x = poll_data()$what_did_you_like_most_about_the_workshop) %>%
      as.data.frame() %>%
      dplyr::filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
      mutate(lemma= tolower(lemma)) %>%
      count(lemma)
  })
  
  poll_rec_results <- reactive({
    udpipe::udpipe_annotate(ud_model, x = poll_data()$please_share_any_recommendations_you_have_for_improvements) %>%
      as.data.frame() %>%
      filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
      mutate(lemma= tolower(lemma)) %>%
      count(lemma)
  }) 
  
  # Data: CRAN Downloads ----------------------------------------------------
  cran_download <-  reactiveFileReader(time_interval, 
                                       NULL,
                                       "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/cran_download_stats.csv",
                                       readr::read_csv)
  
  # Plot: Visitors to Websites of Educational Resources ----------------------------------------------------
  output$plot_visitor_website <- renderPlot({
    itcr_course_data() %>% 
      # Filter out ITN Website since it is not an "Educational Resource"
      filter(website != "ITN Website") %>% 
      ggplot(aes(x = reorder(website, -totalUsers), 
                 y = totalUsers, 
                 fill = target_audience)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1),
            legend.position.inside = c(0.85, 0.85),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Number of Visitors",
           fill = "Target Audience") +
      scale_fill_manual(values=cbPalette)
  })
  
  # Plot: Course Engagement by Modality ----------------------------------------------------
  output$plot_engagement_modality <- renderPlot({
    course_processed()  %>% 
      # Some courses have 0 learners
      filter(number_of_learners > 0) %>%
      ggplot(aes(x = fct_reorder(course_name, course_order),
                 y = number_of_learners, fill = `Target Audience`,)) +
      geom_col() + 
      coord_flip() +
      scale_fill_manual(values=cbPalette) + 
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "bottom",
            text = element_text(size = 17, family = "Arial")) + 
      labs(x = NULL,
           y = "Number of Learners")
  })
  
  # Plot: Course Engagement on Website ----------------------------------------------------
  output$plot_engagement_website <- renderPlot({
    itcr_course_data() %>% 
      clean_names() %>%
      select(website, screen_page_views_per_user, average_session_duration, 
             event_count_per_user, engagement_rate, target_audience) %>%
      pivot_longer(!c(website, target_audience), 
                   names_to = "metric_name", 
                   values_to = "value") %>% 
      filter(!(website %in% c("ITN Website", "OTTR website", "metricminer.org")),
             metric_name == input$metric) %>%
      ggplot(aes(x = website, y = value, fill = target_audience)) +
      geom_bar(position = "dodge", stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL,
           y = NULL,
           fill = "Target Audience") +
      scale_fill_manual(values=cbPalette) +
      scale_x_discrete(limits = c("Leadership in Cancer Informatics", "NIH Data Sharing", "Ethical Data Handling", "Overleaf and Latex for Scientific Articles", "AI for Decision Makers",
                                  "Reproducibility in Cancer Informatics", "Choosing Genomics Tools", "Computing for Cancer Informatics",
                                  "Documentation and Usability", "Advanced Reproducibility", "AI for Efficient Programming", "GitHub Automation for Scientists")) +
      theme(axis.text.x=element_text(angle=90, hjust=1), 
            plot.margin = unit(c(1.5,.5,.5,1.5), "cm"),
            text = element_text(size = 17, family = "Arial"))
  })
  
  # Plot: Course Engagement by Target ----------------------------------------------------
  output$plot_engagement_target <- renderPlot({
    itcr_course_data_long() %>% 
      group_by(modality, target_audience) %>% 
      summarize(total_learners = sum(learner_count, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(modality, -total_learners), y = total_learners, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "bottom",
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Visitors/Enrollees",
           fill = "Target Audience") +
      geom_text(aes(label = total_learners), size = 4, vjust = - 1, na.rm = TRUE) + 
      ylim(c(0, 4200)) + 
      facet_wrap(~target_audience) + 
      scale_fill_manual(values=cbPalette)
  })
  
  # Plot: Learner by Course ----------------------------------------------------
  output$plot_learner_course <- renderPlot({
    itcr_course_data_long() %>% 
      group_by(website, target_audience) %>% 
      summarize(total_learners = sum(learner_count, na.rm = TRUE)) %>%
      ggplot(aes(y = total_learners, x = reorder(website, -total_learners), fill = target_audience)) + 
      geom_bar(stat = "identity") + 
      labs(x = NULL, 
           y = "Total Learners by Course",
           fill = "Target Audience",
           title = "Total Number of Learners for each Course") +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.x=element_text(angle = 70, hjust=1), 
            legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial")) + 
      geom_text(aes(label = total_learners), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1800)) + 
      scale_fill_manual(values=cbPalette)
  })
  
  # Plot: Coursera Learners ----------------------------------------------------
  output$plot_coursera <- renderPlot({
    ggplot(itcr_course_data() %>% filter(coursera_count > 0), 
           aes(x = reorder(website, -coursera_count), y = coursera_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(x = NULL,
           y = "Coursera enrollments",
           fill = "Target Audience",
           title = "Number of Coursera Enrollments by Course") +
      geom_text(aes(label = coursera_count), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 1200)) + 
      scale_fill_manual(values = c("#56B4E9", "#009E73", "#008080")) +
      theme(legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial"))
  })
  
  # Plot: Leanpub Learners ----------------------------------------------------
  output$plot_leanpub <- renderPlot({
    ggplot(itcr_course_data() %>% filter(leanpub_count > 0),
           aes(x = reorder(website, -leanpub_count), y = leanpub_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      labs(x = NULL,
           y = "Leanpub enrollments",
           fill = "Target Audience",
           title = "Number of Leanpub Enrollments by Course") +
      geom_text(aes(label = leanpub_count), size = 4, vjust = - 1, na.rm = TRUE) +
      ylim(c(0, 40)) + 
      scale_fill_manual(values = c("#56B4E9", "#009E73", "#008080")) +
      theme(legend.position = c(0.9, 0.85),
            text = element_text(size = 17, family = "Arial"))
  })
  
  # Plot: Learners by Launch Date ----------------------------------------------------
  output$plot_learner_launch_date <- renderPlot({
    itcr_course_data() %>% 
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
  
  # Plot: Workshop Recommendation ----------------------------------------------------
  output$plot_workshop_recommendation <- renderPlot({
    itcr_slido_data() %>% 
      clean_names() %>% 
      mutate(merged_likely_rec = if_else(is.na(how_likely_would_you_be_to_recommend_this_workshop), 
                                         how_likely_would_you_be_to_recommend_this_workshop_2, 
                                         how_likely_would_you_be_to_recommend_this_workshop),
             merged_likely_rec = as.numeric(merged_likely_rec)) %>% 
      ggplot(aes(merged_likely_rec)) +
      geom_bar(fill = "#28ae80") +
      theme_classic() +
      theme(text = element_text(size = 17, family = "Arial")) +
      labs(y = "Count", 
           x = "Rating")
  })
  
  # Plot: Workshop Relevance ----------------------------------------------------
  output$plot_workshop_relevance <- renderPlot({
    itcr_slido_data() %>% 
      clean_names() %>% 
      mutate(merged_likely_rec = if_else(is.na(how_likely_would_you_be_to_recommend_this_workshop), 
                                         how_likely_would_you_be_to_recommend_this_workshop_2, 
                                         how_likely_would_you_be_to_recommend_this_workshop),
             merged_likely_rec = as.numeric(merged_likely_rec)) %>% 
      filter(how_likely_are_you_to_use_what_you_learned_in_your_daily_work %in% c("Extremely likely", 
                                                                                  "Likely",
                                                                                  "Not very likely", 
                                                                                  "Somewhat likely", 
                                                                                  "Very likely")) %>% 
      mutate(how_likely_are_you_to_use_what_you_learned_in_your_daily_work = factor(how_likely_are_you_to_use_what_you_learned_in_your_daily_work,
                                                                                    levels = c("Not very likely", 
                                                                                               "Somewhat likely", 
                                                                                               "Likely", 
                                                                                               "Very likely",
                                                                                               "Extremely likely"))) %>% 
      ggplot(aes(x = how_likely_are_you_to_use_what_you_learned_in_your_daily_work)) +
      geom_bar(stat = "count", fill = "#28ae80") +
      theme_classic() +
      theme(axis.text.x = element_text(hjust=1),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Count")
  })
  
  # Plot: Workshop Career Stage ----------------------------------------------------
  output$plot_workshop_career_stage <- renderPlot({
    career_stage_processed() %>% 
      ggplot(aes(x = reorder(Stage, -count), y = count, fill = Trainee)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylab("Number of Registrants") +
      theme_bw() + 
      theme(panel.background = element_blank(),
            panel.grid = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 17, family = "Arial")) +
      scale_fill_manual(values = c("#440154", "#28ae80"))
  })
  
  # Plot: Workshop Review ----------------------------------------------------
  output$plot_workshop_review <- renderPlot({
    wordcloud::wordcloud(words = poll_results()$lemma, 
                         freq = poll_results()$n,
                         colors = c("#98fb98", "#83D475", "#355E3B"),
                         min.freq = 3, scale = c(3, .4))
  })
  
  # Plot: Workshop Feedback ----------------------------------------------------
  output$plot_workshop_improvement <- renderPlot({
    wordcloud::wordcloud(words = poll_rec_results()$lemma, 
                         freq= poll_rec_results()$n,
                         colors = c("#98fb98", "#83D475", "#355E3B"),
                         min.freq = 3, scale = c(4, .4))
  })
  
  # Plot: Monthly CRAN Download ----------------------------------------------------
  output$plot_monthly_cran_download <- renderPlot({
    cran_download() %>% 
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
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom") + #clean up x-axis labels
      labs(x = NULL,
           y = "Monthly Downloads",
           color = "R Packages")
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

