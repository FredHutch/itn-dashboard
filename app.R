library(shiny)

# Packages for Data Manipulation
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(forcats)
library(janitor)

# Packages for Word Clouds
library(udpipe)
library(wordcloud)

# Packages for Bootstrap
library(bslib)
library(bsicons)
library(htmltools)
library(ggplot2)
library(fontawesome)
library(DT)

# Misc
library(googlesheets4)
# Suspend authorization
gs4_deauth()

# Everyone, Leadership, new to data science, software developers
cbPalette <- c("#E69F02", "#56B4E9", "#009E73", "#008080") 

xlabel_view <- c(rep(c("black", "transparent", "transparent", "transparent"), 41), "black", "transparent") #166 rows
#cc <- rev(c("#fde725", "#addc30", "#5ec962", "#28ae80", "#21918c", "#2c728e", "#3b528b", "#472d7b", "#440154"))
viridis_cc <- c("#440154", "#2c728e", "#28ae80", "#addc30")

# Wordcloud 
ud_model <- udpipe::udpipe_load_model("wordcloud-model.udpipe")

time_interval <- 604800000

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
                  plotOutput("unique_visitor_website"),
                  br(),
                  textOutput("unique_visitor_website_caption"),
                ),
                nav_panel(
                  "Course Engagement by Modality",
                  plotOutput("engagement_by_modality"),
                  br(),
                  textOutput("engagement_by_modality_caption"),
                ),
                nav_panel(
                  "Course Engagement Stats",
                  plotOutput("engagement_stat"),
                  br(),
                  textOutput("engagement_stat_caption")
                ),
                nav_panel(
                  "Learners by Modality",
                  plotOutput("learner_by_modality"),
                  br(),
                  textOutput("learner_by_modality_caption")
                ),
                nav_panel(
                  "Learners by Course",
                  plotOutput("learner_by_course"),
                  br(),
                  textOutput("learner_by_course_caption")
                ),
                nav_panel(
                  "Coursera Learners",
                  plotOutput("coursera_learner"),
                  br(),
                  textOutput("coursera_learner_caption")
                ),
                nav_panel(
                  "Leanpub Learners",
                  plotOutput("leanpub_learner"),
                  br(),
                  textOutput("leanpub_learner_caption")
                ),
                nav_panel(
                  "Learners by Launch Date",
                  plotOutput("learner_by_launch_date"),
                  br(),
                  textOutput("learner_by_launch_date_caption")
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
  nav_panel("Workshops",
            layout_column_wrap(
              fill = TRUE,
              width = NULL,
              style = css(grid_template_columns = "1.2fr 1fr"),
              navset_card_underline(
                height = 900,
                full_screen = TRUE,
                title = NULL,
                nav_panel(
                  "Recommend this Workshop?",
                  plotOutput("recommend_workshop"),
                  br(),
                  textOutput("recommend_workshop_caption"),
                ),
                nav_panel(
                  "Workshop Relevance Feedback",
                  plotOutput("workshop_relevance_feedback"),
                  br(),
                  textOutput("workshop_relevance_feedback_caption"),
                ),
                nav_panel(
                  "Career Stage of Workshop Registrants",
                  plotOutput("workshop_career_stage"),
                  br(),
                  textOutput("workshop_career_stage_caption")
                )
              ),
              navset_card_underline(
                height = 900,
                full_screen = TRUE,
                title = NULL,
                nav_panel(
                  "What did You Like Most about Workshop?",
                  plotOutput("like_most_about_workshop"),
                  textOutput("like_most_about_workshop_caption")
                ),
                nav_panel(
                  "Recommendations for Improvements",
                  plotOutput("recommendation_improvement"),
                  textOutput("recommendation_improvement_caption")
                )
              )
            )
  ),
  nav_panel("Software Usage",
            layout_column_wrap(
              width = "250px",
              fill = TRUE,
              value_box(
                title = "Loqui: A Shiny app for Creating Automated Videos",
                value = NULL,
                a("https://loqui.fredhutch.org/", href = "https://loqui.fredhutch.org/", target = "_blank")
              ),
              value_box(
                title = "Unique Number of Loqui Users",
                value = "18",
                showcase = bsicons::bs_icon("people-fill")
              ),
              value_box(
                title = "Number of Videos made with Loqui",
                value = "486",
                showcase = bsicons::bs_icon("camera-video-fill")
              )
            ),
            navset_card_underline(
              height = 900,
              full_screen = TRUE,
              title = NULL,
              nav_panel(
                "CRAN Monthly Downloads",
                plotOutput("cran_download_stats"),
                br(),
                textOutput("cran_download_stats_caption")
              )
            )),
  nav_panel("Collaborations",
            navset_card_underline(
              height = 900,
              full_screen = TRUE,
              title = NULL,
              nav_panel(
                "All Collaborations",
                plotOutput("collaboration_all"),
                br(),
                textOutput("collaboration_all_caption")
              ),
              nav_panel(
                "ITCR Collaborations",
                plotOutput("collaboration_itcr"),
                br(),
                textOutput("collaboration_itcr_caption")
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
  # ITCR Slido Data
  itcr_slido_data <- reactiveFileReader(time_interval,
                                        NULL,
                                        "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/itcr_slido_data.csv",
                                        readr::read_csv) 
  
  # Workshops Data
  poll_data <- reactive({
    itcr_slido_data() %>%
      janitor::clean_names() %>% 
      mutate(merged_likely_rec = if_else(is.na(how_likely_would_you_be_to_recommend_this_workshop), how_likely_would_you_be_to_recommend_this_workshop_2, 
                                         how_likely_would_you_be_to_recommend_this_workshop))
  })
  
  poll_data_subset <- reactive({
    poll_data() %>%
      dplyr::filter(how_likely_are_you_to_use_what_you_learned_in_your_daily_work %in% c("Extremely likely", 
                                                                                         "Likely",
                                                                                         "Not very likely", 
                                                                                         "Somewhat likely", 
                                                                                         "Very likely")) %>% 
      mutate(how_likely_are_you_to_use_what_you_learned_in_your_daily_work = factor(how_likely_are_you_to_use_what_you_learned_in_your_daily_work,
                                                                                    levels = c("Not very likely", "Somewhat likely", "Likely", "Very likely", "Extremely likely")))
  })
  
  # Course Engagement by modality 
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
      mutate(modality = factor(modality, levels = c("website", "leanpub", "coursera")))%>%
      mutate(course_order = case_when(course_type == "Leadership" ~ 1,
                                      course_type == "New to data" ~ 2,
                                      course_type == "Software developers" ~ 3)) %>%
      rename("Target Audience" =  course_type)
  })
  
  
  
  
  results <- reactive({
    udpipe::udpipe_annotate(ud_model, x = poll_data()$what_did_you_like_most_about_the_workshop) %>%
      as.data.frame() %>%
      dplyr::filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
      mutate(lemma= tolower(lemma)) %>%
      count(lemma)
  })
  
  rec_results <- reactive({
    udpipe::udpipe_annotate(ud_model, x = poll_data()$please_share_any_recommendations_you_have_for_improvements) %>%
      as.data.frame() %>%
      filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
      mutate(lemma= tolower(lemma)) %>%
      count(lemma)
  }) 
  
  # ITCR Course data ----
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
  
  
  # CRAN Downloads ----
  cran_download_stats <-  reactiveFileReader(time_interval, 
                                             NULL,
                                             "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/cran_download_stats.csv",
                                             readr::read_csv)
  
  
  # ITCR GA Metrics ----
  ga_metrics <-  reactiveFileReader(time_interval, 
                                    NULL,
                                    "https://raw.githubusercontent.com/FredHutch/itn-dashboard/main/data/itcr_ga_metric_data.csv",
                                    readr::read_csv)
  
  user_totals <- reactive({
    ga_metrics() %>% 
      janitor::clean_names() %>% 
      select(website, active_users, average_session_duration) %>% 
      mutate(average_session_duration = round(average_session_duration, digits = 0))
  })
  
  user_engagement <- reactive({
    ga_metrics() %>% 
      janitor::clean_names() %>% 
      select(website, screen_page_views_per_user, 
             sessions, screen_page_views, engagement_rate) %>% 
      mutate(screen_page_views_per_user = round(screen_page_views_per_user, 0),
             engagement_rate = round(engagement_rate, 2))
  })
  
  
  # Collabs ----
  collabs_raw <-  reactiveFileReader(time_interval, 
                                     NULL,
                                     "https://docs.google.com/spreadsheets/d/1-8vox2LzkVKzhmSFXCWjwt3jFtK-wHibRAq2fqbxEyo/edit?usp=sharing",
                                     googlesheets4::read_sheet)
  
  collabs <- reactive({
    collabs_raw() %>% 
      separate_rows("Category", sep = ", ", ) %>% 
      mutate(Category = trimws(Category)) %>% 
      filter(Category != "?")
  })
  
  # Career Stage ----
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
    
    career_stage_processed$Trainee <- ifelse(career_stage_processed$Stage %in% c("Phd student", "postdoc", "Master's student", "Research tech", "undergrad"), "yes",
                                             "no")
    
    career_stage_processed
  })
  
  # Plots --------
  
  # Unique Visitors to Websites
  output$unique_visitor_website <- renderPlot({
    ggplot(itcr_course_data(), aes(x = reorder(website, -totalUsers), 
                                   y = totalUsers, 
                                   fill = target_audience)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = totalUsers), size = 4, vjust = - 1) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust=1),
            legend.position = c(0.85, 0.85),
            text = element_text(size = 17, family = "Arial")) +
      labs(x = NULL,
           y = "Total # of Visitors",
           fill = "Target Audience",
           title = "Visitor Distribution across Educational Resources") +
      scale_fill_manual(values=cbPalette)
  })
  # Caption
  output$unique_visitor_website_caption <- renderText({
    "Number of visitors for various educational resources, segmented by different target audience categories: everyone, leadership, new to data, and software developers."
  })
  
  # Course Engagement By Modality
  output$engagement_by_modality <- renderPlot({
    course_processed()  %>% 
      ggplot(aes(x = forcats::fct_reorder(course_name, course_order),
                 y = number_of_learners, fill = `Target Audience`,)) +
      geom_col() + 
      facet_wrap(~modality, nrow = 3, scales = "free_y")  + 
      scale_fill_manual(values=cbPalette) + 
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 17, family = "Arial")) + 
      labs(x = NULL, y = "Number of Learners")
  })
  # Caption
  output$engagement_by_modality_caption <- renderText({
    "The number of learners for each modality (website = bookdown website) for each course."
  })
  
  # Engagement Stats
  output$engagement_stat <- renderPlot({
    itcr_course_data() %>% 
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
  output$engagement_stat_caption <- renderText({
    "Comprehensive analysis of website engagement for various courses, measured by average session duration, engagement rate, event count per user, and screen page views per user, categorized by target audience groups such as everyone, leadership, new to data, and software developers."
  })
  
  
  # Learners by Modality
  output$learner_by_modality <- renderPlot({
    itcr_course_data_long() %>% 
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
  # Caption
  output$learner_by_modality_caption <- renderText({
    "Level of engagement in courses by modality, segmented by different target audience categories: everyone, leadership, new to data, and software developers. It contrasts the number of visitors/enrollees across website learners and total course enrollments."
  })
  
  
  # Learner by Course
  output$learner_by_course <- renderPlot({
    itcr_course_data_long() %>% 
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
  # Caption
  output$learner_by_course_caption <- renderText({
    "Total number of learners enrolled in each course, segmented by target audience categories."
  })
  
  
  
  # Coursera Learners
  output$coursera_learner <- renderPlot({
    ggplot(itcr_course_data() %>% filter(coursera_count > 0), 
           aes(x = reorder(website, -coursera_count), y = coursera_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
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
  # Caption
  output$coursera_learner_caption <- renderText({
    "Number of enrollments for various Coursera courses, delineating the popularity of courses among different target audiences, such as leadership, new to data, and software developers, with the 'Computing for Cancer Informatics' course showing the highest enrollment figures."
  })
  
  
  # Leanpub Learners
  output$leanpub_learner <- renderPlot({
    ggplot(itcr_course_data() %>% filter(leanpub_count > 0),
           aes(x = reorder(website, -leanpub_count), y = leanpub_count, fill = target_audience)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
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
  # Caption
  output$leanpub_learner_caption <- renderText({
    "Number of Leanpub course enrollments, highlighting interest across different subjects with 'Leadership in Cancer Informatics' attracting the most learners, categorized by target audiences like leadership, those new to data, and software developers."
  })
  
  # Learners by Launch Date
  output$learner_by_launch_date <- renderPlot({
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
  # Caption
  output$learner_by_launch_date_caption <- renderText({
    "How long a course has been out (in days) is plotted on the x-axis vs the number the popularity of the course on the y-axis. We measured the popularity by considering the number of unique bookdown views together with the number of Coursera and Leanpub enrollments."
  })
  
  # CRAN Download Stats
  output$cran_download_stats <- renderPlot({
    cran_download_stats() %>% 
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
            legend.position = c(0.05, 0.9)) + #clean up x-axis labels
      labs(x = NULL,
           y = "Monthly Downloads",
           color = "R Packages")
  })
  # Caption
  output$cran_download_stats_caption <- renderText({
    "Monthly downloads of four R packages from CRAN ('ari', 'conrad', 'ottrpal', 'text2speech') showing fluctuations over time and notable spikes or drops that could indicate changes in usage trends or updates affecting the packages' popularity."
  })
  
  
  # How Likely woud you be to recommend this workshop?
  output$recommend_workshop <- renderPlot({
    as.numeric(poll_data()$merged_likely_rec) %>%
      qplot(geom = "bar") +
      geom_bar(fill = "#CBC3E3") +
      theme_classic() +
      theme(text = element_text(size = 17, family = "Arial")) +
      labs(title = "How likely would you be to recommend this workshop?", 
           y = "count", 
           x = "rating")
  })
  # Caption
  output$recommend_workshop_caption <- renderText({ 
    "Responses from workshop attendees on their likelihood to recommend the workshop, rated on a scale from 1 (not likely at all) to 10 (very likely). The y-axis represents the count of respondents for each rating." 
  })
  
  # Workshop Relevance Feedback
  output$workshop_relevance_feedback <- renderPlot({
    ggplot(poll_data_subset(), aes(x = how_likely_are_you_to_use_what_you_learned_in_your_daily_work)) +
      geom_bar(stat = "count", fill = "#CBC3E3") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 17, family = "Arial")) +
      labs(title = "How likely are you to use what you learned in your daily work?",
           x = NULL)
    
  })
  # Caption
  output$workshop_relevance_feedback_caption <- renderText({
    "Feedback from workshop attendees on the relevance of the workshop content to their daily tasks and their likelihood of applying the learned material. The y-axis displays the count of respondents for each rating level."
  })
  
  output$workshop_career_stage <- renderPlot({
    ggplot(career_stage_processed(), aes(x=reorder(Stage, -count), y=count, fill=Trainee)) +
      geom_bar(stat = "identity") +
      xlab("Career stage") +
      ylab("Number of registrees") +
      ggtitle("Career Stage of Workshop Registrants") +
      theme_bw() + 
      theme(panel.background = element_blank(),
            panel.grid = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 17, family = "Arial")) +
      scale_fill_manual(values = c("#440154", "#28ae80"))
  })
  # Caption
  output$workshop_career_stage_caption <- renderText({
    "Distribution of workshop registrants across different career stages and a distinction between trainees and non-trainees indicated by color."
  })
  
  
  
  # What did you like most about workshop
  output$like_most_about_workshop <- renderPlot({
    wordcloud::wordcloud(words = results()$lemma, 
                         freq = results()$n,
                         colors = c("#98fb98", "#83D475", "#355E3B"),
                         min.freq = 3, scale = c(3, .4))
  })
  # Caption
  output$like_most_about_workshop_caption <- renderText({
    "This word cloud shows the most used terms in workshop reviews when asked 'what did you like most about the workshop?' Terms like 'interactive', 'hands-on', 'activity', and 'how' suggests workshop participants really enjoy the interactivity of the workshops."
  })
  
  
  # Recommendations for Improvements
  output$recommendation_improvement <- renderPlot({
    wordcloud::wordcloud(words = rec_results()$lemma, 
                         freq=rec_results()$n,
                         colors = c("#98fb98", "#83D475", "#355E3B"),
                         min.freq = 3, scale = c(4, .4))
  })
  # Caption
  output$recommendation_improvement_caption <- renderText({
    "Feedback themes for a workshop, with the most significant focus on 'more', 'how', 'time', and 'example', suggesting participants are interested in detailed explanations, practical examples, and perhaps more time or more workshop offerings. "
  })
  
  # Collaborations - All
  output$collaboration_all <- renderPlot({
    collabs() %>% 
      count(Category) %>% 
      ggplot(aes(y = n, x = reorder(Category,-n), fill = Category)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1), 
            strip.text.x = element_text(size = 6),
            legend.position="none", 
            text = element_text(size = 17, family = "Arial"),
            plot.margin = unit(c(.75,.5,.5,.5), "cm")) + 
      xlab(NULL) +
      ylab("N") +
      ggtitle("Collaborations with All Individuals")
  })
  # Caption
  output$collaboration_all_caption <- renderText({
    "This plot shows the large variety of collaborations and engagements with the ITN. Distribution of various academic and professional collaborations, such as manuscript authorship, course development, and networking events, showing how frequently each collaboration was completed."
  })
  
  # Collaborations - ITCR Funded
  output$collaboration_itcr <- renderPlot({
    collabs() %>% 
      filter(ITN_ITCR_or_external == "ITCR") %>%
      count(Category) %>%
      ggplot(aes(y =n, x=reorder(Category, -n), fill = Category )) +
      geom_bar(position="dodge", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1), strip.text.x = element_text(size=6),
            text = element_text(size = 17, family = "Arial"),
            legend.position = "none",
            plot.margin = unit(c(.75,.5,.5,.5), "cm")) +
      xlab(NULL) +
      ylab("N") +
      ggtitle("Collaborations with ITCR funded individuals only")
    
  })
  # Caption
  output$collaboration_itcr_caption <- renderText({
    "This plot also shows the number of collaborations but only collaborations that were completed with ITCR funded individuals. The number of individuals who have helped with each type of collaboration within the ITCR community. Most individuals were ITCR investigators but some were trainees or scientific staff."
  })
  
  
  
  # Tables --------
  
  # User Totals
  output$user_totals <- renderDT({
    datatable(
      user_totals(), 
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
      user_engagement(), 
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

