library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(scales)

#==========================================================
# 1. exams.csv  (Score distributions + averages)
#==========================================================
# First read of exams.csv (tidyverse) used for:
# - grade-level average scores
# - subject-level average scores
# - race-level averages
# - gender-level averages

exams <- read_csv("exams.csv", show_col_types = FALSE)

# Keep only numeric score rows (1–5) and standardize subject name
exams_clean <- exams %>%
  filter(Score %in% c("1", "2", "3", "4", "5")) %>%
  mutate(Score = as.numeric(Score)) %>%
  rename(Exam.Subject = `Exam Subject`)

#----------------- Grade-level averages (11th vs 12th) -----------------
grade_scores <- exams_clean %>%
  select(Score, `Students (11th Grade)`, `Students (12th Grade)`) %>%
  pivot_longer(
    cols = c(`Students (11th Grade)`, `Students (12th Grade)`),
    names_to = "Grade",
    values_to = "Count"
  ) %>%
  mutate(
    Grade = case_when(
      str_detect(Grade, "11th") ~ "11th Grade",
      str_detect(Grade, "12th") ~ "12th Grade"
    )
  )

# Weighted average score by grade (using student counts as weights)
grade_avg <- grade_scores %>%
  group_by(Grade) %>%
  summarise(
    Average.Score = weighted.mean(Score, Count, na.rm = TRUE),
    .groups = "drop"
  )

#----------------- Subject-level averages -----------------
subject_scores <- exams_clean %>%
  group_by(Exam.Subject) %>%
  summarise(
    Average.Score = weighted.mean(Score, `Students (11th Grade)`, na.rm = TRUE),
    .groups = "drop"
  )

#----------------- Race-level averages (from "Average" rows only) -----------------
# "Average" rows contain average score by subgroup (race, gender, etc.)
exams_avg <- exams[exams$Score == "Average", ]

# Columns 7–13 in exams are race-specific averages
exams_avgr <- exams_avg[, c(1, 7:13)]
colnames(exams_avgr) <- c(
  "Exam_Subject", "White", "Black", "Hispanic_Latino", "Asian",
  "Indian_Alaska_Native", "Hawaiian_Pacific_Islander", "Two_or_More_Races"
)

# Ensure race columns are numeric and replace NAs with 0
exams_avgr <- exams_avgr %>%
  mutate(across(-Exam_Subject, ~ as.numeric(as.character(.))))
exams_avgr[is.na(exams_avgr)] <- 0

# Long format for race-by-subject plot
race_score_long <- exams_avgr %>%
  pivot_longer(
    cols = c("White", "Black", "Hispanic_Latino", "Asian",
             "Indian_Alaska_Native", "Hawaiian_Pacific_Islander",
             "Two_or_More_Races"),
    names_to = "Race",
    values_to = "Avg_Score"
  ) %>%
  mutate(
    Exam_Subject = factor(Exam_Subject, levels = unique(Exam_Subject))
  )

#----------------- Gender-level averages -----------------
exams_avg_gender <- exams_avg[, c(1, 5:6)]  # Male/Female columns
colnames(exams_avg_gender) <- c("Exam_Subject", "Male", "Female")

gender_score_long <- exams_avg_gender %>%
  pivot_longer(
    cols = c("Male", "Female"),
    names_to = "Gender",
    values_to = "Avg_Score"
  ) %>%
  mutate(
    Exam_Subject = factor(Exam_Subject, levels = unique(Exam_Subject))
  )

#==========================================================
# 1b. exams.csv (second read for Conan's tabs: popularity & score)
#==========================================================
# NOTE:
# - The popularity and score-analysis tabs were written using the base
#   read.csv + make.names() version.
# - This re-reads exams and overwrites the exams object with a different
#   column naming scheme (e.g., All.Students..2016.).
# - We keep this as-is because those visuals depend on these exact names.

exams_raw <- read.csv("exams.csv", stringsAsFactors = FALSE, check.names = FALSE)
names(exams_raw) <- make.names(names(exams_raw))  # e.g., "All Students (2016)" -> "All.Students..2016."
exams <- exams_raw

# Column name for subject in this version
subject_col <- if ("Exam.Subject" %in% names(exams)) "Exam.Subject" else names(exams)[1]

# List of exam subjects for Conan's selectInputs
subjects <- sort(unique(exams[[subject_col]]), na.last = TRUE)

#==========================================================
# 2. students.csv  (Participation by race / grade)
#==========================================================
students_raw <- read_csv("students.csv") %>%
  rename(Exam.Subject = `Exam Subject`)  # standardize name

# Remove "ALL SUBJECTS" totals and clean "Students (X)" -> "X"
students1 <- students_raw %>%
  filter(!str_detect(Exam.Subject, "ALL SUBJECTS")) %>%
  rename_with(
    ~ str_replace_all(., c(
      "^Students \\(" = "",
      "\\)$"          = "",
      "\\s*/\\s*"     = "/"
    )),
    starts_with("Students (")
  )

race_cols <- c(
  "White", "Black", "Hispanic/Latino", "Asian",
  "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
  "Two or More Races", "Other Race", "Race Not Known"
)

grade_cols <- c(
  "9th Grade", "10th Grade", "11th Grade", "12th Grade",
  "Not High School", "> 9th Grade", "Grade Not Known"
)

# Total participation across grade columns (used to pick most popular subjects)
students2 <- students1 %>%
  mutate(total_grade_sum = rowSums(across(all_of(grade_cols)), na.rm = TRUE))

#==========================================================
# 3. UI
#==========================================================
ui <- dashboardPage(
  dashboardHeader(title = "2016 AP Exam Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Grade",              tabName = "grade",          icon = icon("graduation-cap")),
      menuItem("Score Analysis",     tabName = "scores",         icon = icon("bar-chart")),
      menuItem("Race",               tabName = "race",           icon = icon("users")),
      menuItem("Gender",  tabName = "gender",         icon = icon("venus-mars")),
      menuItem("Exam Popularity",    tabName = "popularity",     icon = icon("star"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #----------------------------------------------------
      # Grade Tab (Overall score by grade)
      #----------------------------------------------------
      tabItem(tabName = "grade",
              h2("Does Grade in School Affect Average AP Score?", style = "text-align:center;"),
              fluidRow(
                box(width = 12,
                    title = "Average Exam Score by Grade Level",
                    status = "primary",
                    solidHeader = TRUE,
                    checkboxGroupInput(
                      "grade_select",
                      "Select Grade(s):",
                      choices  = c("11th Grade", "12th Grade"),
                      selected = c("11th Grade", "12th Grade"),
                      inline   = TRUE
                    ),
                    plotlyOutput("gradePlot", height = "500px"),
                    p("No significant difference between the grade levels. Many AP classes are taken by 11th and 12th graders at the same time, so this is unsurprising.")
                )
              )
      ),
      
      #----------------------------------------------------
      # Score Analysis 
      #----------------------------------------------------
      tabItem(tabName = "scores",
              h2("What is the Average Score For Each Subject and the Percentage of Students That Are Above the Average Score?", style = "text-align:center;"),
              fluidRow(
                box(width = 12,
                    title = "Average Exam Score by Subject",
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput(
                      "subject_select",
                      "Select Subject(s):",
                      choices  = unique(subject_scores$Exam.Subject),
                      selected = unique(subject_scores$Exam.Subject)[1:10],
                      multiple = TRUE),
                    plotlyOutput("subjectPlot", height = "600px"),
                    p("Unexpectedly, some courses that may be percieved as easier showed lower scores than classes that are traditionally thought of as difficult (e.g. Art History has a lower average score than Physics C: Mechanics). Harder AP classes are typically taken by people who are very interested in the topic, and therefore put in a lot of effort into the subject. Conversely, subjects like AP Art History may be seen as an easier AP, and taken just to get credit, but as less effort is put into the class, the lower the overall score is.")
                )
              ),
              fluidRow(
                box(width = 12, 
                    status = "primary", 
                    solidHeader = TRUE,
                    title = "Interactive: Average Score and % of Students Above Average",
                    selectInput("score_subjects",
                                "Choose Exam Subject(s):",
                                choices  = subjects,
                                selected = intersect(c("BIOLOGY", "CALCULUS AB"), subjects),
                                multiple = TRUE),
                    plotOutput("score_plot_interactive", height = "420px")
                )
              ),
              fluidRow(
                box(width = 12, 
                    status = "info", 
                    solidHeader = TRUE,
                    title = "Static: Average Score and % of Students Above Average (All Subjects)",
                    plotOutput("score_plot_static", height = "520px")
                )
              )
      ),
      
      #----------------------------------------------------
      # Race & AP Score (exams.csv)
      #----------------------------------------------------
      tabItem(tabName = "race",
              h2("Does Race Affect Average AP Score and Participation?", style = "text-align:center;"),
              fluidRow(
                box(
                  width = 12,
                  title = "Interactive Race Plot: Average AP Score by Race",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "subject_race",
                    "Choose Exam Subject(s):",
                    choices  = sort(unique(race_score_long$Exam_Subject)),
                    selected = c("BIOLOGY", "CALCULUS AB"),
                    multiple = TRUE
                  ),
                  plotOutput("racePlot_interactive", height = "450px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Static Race Plot: Overall Race Trends",
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("racePlot_static", height = "450px"),
                  p("Races including Asian, White, and two or more races generally score higher than Hispanic/Latino, Black, Indian Alaska Native, and Hawaiian Pacific Islander.")
                )
              ),
              fluidRow(
                box(width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    title = "AP Subject Participation by Race",
                    selectInput("topN_race_students",
                                "Number of top subjects per race:",
                                choices  = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "30" = 30, "All Subjects" = 37),
                                selected = 10),
                    plotOutput("students_racePlot", height = "800px"),
                    p("Participation patterns differ strongly by race: English and U.S. History subjects attract broad participation across all groups, STEM subjects show noticeably higher representation from Asian students, and Spanish Language is heavily concentrated among Hispanic/Latino students. These trends suggest that course availability, school demographics, and academic tracking influence which AP subjects students take.")
                )
              )  
      ),
      
      
      #----------------------------------------------------
      # Gender & AP Score (exams.csv)
      #----------------------------------------------------
      tabItem(tabName = "gender",
              h2("Does Gender Affect Average AP Score?", style = "text-align:center;"),
              fluidRow(
                box(
                  width = 12,
                  title = "Average AP Score by Gender",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "subject_gender",
                    "Choose Exam Subject(s):",
                    choices  = sort(unique(gender_score_long$Exam_Subject)),
                    selected = c("BIOLOGY", "CALCULUS AB"),
                    multiple = TRUE
                  ),
                  plotOutput("genderPlot", height = "450px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Static Gender Plot: Overall Gender Trends",
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("GenderPlot_static", height = "450px"),
                  p("Males for most subjects score slightly higher than females. There are only 11/37 subjects where females score higher, four of them are language related, three are art related, and two are english and literature related.")
                )
              )
      ),
      
      
      #----------------------------------------------------
      # Exam Popularity (Conan's: exams.csv, second read)
      #----------------------------------------------------
      tabItem(tabName = "popularity",
              fluidRow(
                box(width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    title = "Static: Full Ranking",
                    plotOutput("pop_plot_static", height = "520px")
                )
              ), 
              h2("What AP Classes Are Most Popular?", style = "text-align:center;"),
              fluidRow(
                box(width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    title = "Interactive: AP Exams Student Count",
                    selectInput("pop_subjects",
                                "Choose Exam Subject(s):",
                                choices  = subjects,
                                selected = intersect(c("BIOLOGY", "CALCULUS AB"), subjects),
                                multiple = TRUE),
                    plotOutput("pop_plot_interactive", height = "420px")
                )
              ),
              fluidRow(
                box(width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    title = "AP Subject Grade Mix",
                    selectInput("topK_grade_students",
                                "Number of most popular subjects:",
                                choices  = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "30" = 30, "All Subjects" = 37),
                                selected = 10),
                    plotOutput("students_gradePlot", height = "520px"),
                    p("Upper-level AP courses such as Calculus AB, Biology, and U.S. Government are mostly taken by 11th and 12th graders, while entry-level courses like Human Geography and World History show large participation from 9th and 10th graders. This pattern reflects typical prerequisite pathways in U.S. schools and highlights when students gain access to advanced coursework.")
                )
              )
      )
    )
  )
)


#==========================================================
# 4. SERVER
#==========================================================
server <- function(input, output, session) {
  
  #----------------- exams.csv : Grade tab -----------------
  output$gradePlot <- renderPlotly({
    filtered <- grade_avg %>%
      filter(Grade %in% input$grade_select)
    
    p <- ggplot(filtered, aes(x = Grade, y = Average.Score, fill = Grade)) +
      geom_col() +
      labs(
        title = "Overall Average Exam Score by Grade",
        x = "Grade Level", y = "Average Score"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  #----------------- exams.csv : Subject tab ----------------
  output$subjectPlot <- renderPlotly({
    filtered <- subject_scores %>%
      filter(Exam.Subject %in% input$subject_select)
    
    p <- ggplot(filtered,
                aes(x = reorder(Exam.Subject, Average.Score),
                    y = Average.Score)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Average Exam Score by Subject",
        x = "Exam Subject", y = "Average Score"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  #----------------- exams.csv : Race tab ------------------
  # Interactive – selected subjects, race on x-axis
  output$racePlot_interactive <- renderPlot({
    dat_selected <- race_score_long %>%
      filter(Exam_Subject %in% input$subject_race) %>%
      group_by(Exam_Subject) %>%
      mutate(Race = reorder(Race, -Avg_Score)) %>%
      ungroup()
    
    ggplot(dat_selected, aes(x = Race, y = Avg_Score, fill = Race)) +
      geom_col() +
      facet_wrap(~ Exam_Subject, ncol = 2, scales = "free_y") +
      labs(
        title    = "Average AP Score by Race For Selected Subjects",
        x        = "Race (Ordered by Average Score)",
        y        = "Average AP Score",
        subtitle = "Data from College Board 2016"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  # Static – overall race trends across all subjects
  output$racePlot_static <- renderPlot({
    race_overall <- race_score_long %>%
      group_by(Exam_Subject) %>%
      summarise(mean_score = mean(Avg_Score, na.rm = TRUE)) %>%
      arrange(mean_score)
    
    race_score_long$Exam_Subject <- factor(
      race_score_long$Exam_Subject,
      levels = race_overall$Exam_Subject
    )
    
    ggplot(race_score_long, aes(x = Avg_Score, y = Exam_Subject, color = Race)) +
      geom_point(size = 2) +
      labs(
        title = "Average AP Score For Different Subjects Separated by Race",
        subtitle = "Data From College Scoreboard 2016 \nSubjects ordered by overall average AP score",
        x     = "Average AP Test Score",
        y     = "Exam Subject"
      ) +
      theme_bw()
  })
  
  #----------------- exams.csv : Gender tab ----------------
  # Interactive – selected subjects
  output$genderPlot <- renderPlot({
    dat_gender <- gender_score_long %>%
      filter(Exam_Subject %in% input$subject_gender) %>%
      group_by(Exam_Subject, Gender) %>%
      summarise(Avg_Score = mean(Avg_Score), .groups = "drop") %>%
      mutate(Gender = reorder(Gender, -Avg_Score))
    
    ggplot(dat_gender, aes(x = Gender, y = Avg_Score, fill = Gender)) +
      geom_col() +
      facet_wrap(~ Exam_Subject, ncol = 2) +
      scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
      labs(
        title = "Average AP Score by Gender (Selected Subjects)",
        subtitle = "Data From College Scoreboard 2016",
        x = "Gender", y = "Average AP Score"
      ) +
      theme_bw()
  })
  
  # Static – overall gender trends (subjects where females > males marked)
  output$GenderPlot_static <- renderPlot({
    female_higher <- gender_score_long %>%
      group_by(Exam_Subject) %>%
      summarise(
        female_mean   = mean(Avg_Score[Gender == "Female"]),
        male_mean     = mean(Avg_Score[Gender == "Male"]),
        female_higher = female_mean > male_mean
      )
    
    highlight_vec <- setNames(female_higher$female_higher, female_higher$Exam_Subject)
    
    label_fun <- function(subj_names) {
      sapply(subj_names, function(s) {
        if (highlight_vec[[s]]) paste0("* ", s) else s
      })
    }
    
    dat_order <- gender_score_long %>%
      group_by(Exam_Subject) %>%
      summarise(mean_score = mean(Avg_Score)) %>%
      arrange(mean_score)
    
    dat_plot <- gender_score_long %>%
      mutate(Exam_Subject = factor(Exam_Subject, levels = dat_order$Exam_Subject))
    
    ggplot(dat_plot, aes(x = Avg_Score, y = Exam_Subject, color = Gender)) +
      geom_point() +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
      scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5),
                         expand = expansion(mult = c(0.05, 0.1))) +
      scale_y_discrete(labels = label_fun) +
      labs(
        title = "Average AP Score by Gender (All Subjects)",
        subtitle = "Data From College Scoreboard 2016 (Ordered by Subject Average Score) \n * indicates subjects where Female average is more than Male average",
        x = "Average AP Score",
        y = "Exam Subject"
      ) +
      theme_bw()
  })
  
  #----------------- students.csv : Race tab ----------------
  output$students_racePlot <- renderPlot({
    race_long <- students1 %>%
      select(Exam.Subject, all_of(race_cols)) %>%
      pivot_longer(
        cols      = all_of(race_cols),
        names_to  = "Race",
        values_to = "Count"
      ) %>%
      mutate(
        Count = coalesce(Count, 0),
        Race  = factor(Race, levels = race_cols)
      ) %>%
      group_by(Race) %>%
      mutate(share = Count / sum(Count, na.rm = TRUE)) %>%
      ungroup()
    
    race_long_f <- race_long %>%
      group_by(Race) %>%
      slice_max(order_by = share,
                n = as.numeric(input$topN_race_students),
                with_ties = FALSE) %>%
      ungroup() %>%
      group_by(Exam.Subject) %>%
      mutate(max_share = max(share)) %>%
      ungroup() %>%
      mutate(Exam.Subject = fct_reorder(Exam.Subject, max_share))
    
    ggplot(race_long_f, aes(x = Race, y = Exam.Subject, fill = share)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_viridis_c(labels = scales::percent, option = "C") +
      scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
      labs(
        title = "AP Subject Participation by Race (Students)",
        subtitle = paste0("Top ", input$topN_race_students, " subjects per race"),
        x = "Race", y = "AP Subject", fill = "Share"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank()
      )
  })
  
  #----------------- students.csv : Grade tab ---------------
  output$students_gradePlot <- renderPlot({
    top_subjects <- students2 %>%
      slice_max(order_by = total_grade_sum,
                n = as.numeric(input$topK_grade_students),
                with_ties = FALSE) %>%
      pull(Exam.Subject)
    
    grade_long <- students2 %>%
      filter(Exam.Subject %in% top_subjects) %>%
      select(Exam.Subject, all_of(grade_cols)) %>%
      pivot_longer(
        cols      = all_of(grade_cols),
        names_to  = "Grade",
        values_to = "Count"
      ) %>%
      mutate(
        Count = coalesce(Count, 0),
        Grade = factor(Grade, levels = grade_cols)
      ) %>%
      group_by(Exam.Subject) %>%
      mutate(
        subject_total = sum(Count, na.rm = TRUE),
        pct           = if_else(subject_total > 0, Count / subject_total, 0)
      ) %>%
      ungroup() %>%
      mutate(Exam.Subject = fct_relevel(Exam.Subject, top_subjects))
    
    ggplot(grade_long, aes(x = Exam.Subject, y = pct, fill = Grade)) +
      geom_col(position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "AP Subject Grade Mix (Students)",
        subtitle = paste0("Top ", input$topK_grade_students, " subjects shown"),
        x = "AP Subject", y = "Share of test takers", fill = "Grade"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank())
  })
  
  #----------------- exams.csv : Exam Popularity ------------
  # Interactive Plot
  output$pop_plot_interactive <- renderPlot({
    pop_df <- exams %>%
      filter(Score == "All", `Exam.Subject` %in% input$pop_subjects) %>%
      group_by(`Exam.Subject`) %>%
      summarise(Total_Students = sum(`All.Students..2016.`)) %>%
      arrange(desc(Total_Students))
    
    ggplot(pop_df,
           aes(x = reorder(str_to_lower(`Exam.Subject`), Total_Students),
               y = Total_Students)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "AP Exam Student Count by Subject(s) (2016)",
        x = "AP Subject",
        y = "Total Number of Students"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # Static Plot
  output$pop_plot_static <- renderPlot({
    exam_counts <- exams %>%
      filter(Score == "All") %>%
      group_by(`Exam.Subject`) %>%
      summarise(Total_Students = sum(`All.Students..2016.`, na.rm = TRUE)) %>%
      arrange(desc(Total_Students))
    
    ggplot(exam_counts,
           aes(x = reorder(str_to_lower(`Exam.Subject`), Total_Students),
               y = Total_Students)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Most Taken AP Exams (2016)",
        x = "AP Subject",
        y = "Total Number of Students"
      ) +
      theme_minimal(base_size = 12)
  })
  
  #----------------- exams.csv : Score Analysis -------------
  # Interactive Plot
  output$score_plot_interactive <- renderPlot({
    
    # Average scores (from "Average" row)
    exam_avg <- exams %>%
      filter(Score == "Average") %>%
      transmute(Exam.Subject, Avg_Score = All.Students..2016.)
    
    # Total students (from "All" row)
    total_students <- exams %>%
      filter(Score == "All") %>%
      transmute(Exam.Subject, Total_Students = All.Students..2016.)
    
    # Students scoring above the average for their subject
    students_above <- exams %>%
      filter(Score %in% 1:5) %>%
      mutate(
        Score_num = as.numeric(Score),
        Count     = All.Students..2016.
      ) %>%
      left_join(exam_avg, by = "Exam.Subject") %>%
      filter(Score_num > Avg_Score) %>%
      group_by(Exam.Subject) %>%
      summarise(Students_Above = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      left_join(total_students, by = "Exam.Subject") %>%
      mutate(Pct_Above = 100 * Students_Above / Total_Students)
    
    # Combine averages + % above average
    subject_summary <- exam_avg %>%
      left_join(students_above, by = "Exam.Subject") %>%
      mutate(
        Students_Above = ifelse(is.na(Students_Above), 0, Students_Above),
        Pct_Above      = ifelse(is.na(Pct_Above), 0, Pct_Above)
      )
    
    df <- subject_summary
    if (!is.null(input$score_subjects) && length(input$score_subjects) > 0) {
      df <- df %>% filter(Exam.Subject %in% input$score_subjects)
    }
    
    ggplot(df,
           aes(x = reorder(str_to_lower(Exam.Subject), Avg_Score),
               y = Avg_Score)) +
      geom_col(aes(fill = Pct_Above), width = 0.7) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "% Above Avg") +
      geom_text(aes(label = sprintf("%.1f%%", Pct_Above)),
                hjust = -0.2, size = 3.3) +
      labs(
        title = "Average AP Exam Score and % of Students Above Average (2016)",
        x = "AP Subject",
        y = "Average Score (1–5)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # Static Plot
  output$score_plot_static <- renderPlot({
    
    # Average scores (from "Average" row)
    exam_avg <- exams %>%
      filter(Score == "Average") %>%
      transmute(Exam.Subject, Avg_Score = All.Students..2016.)
    
    # Total students (from "All" row)
    total_students <- exams %>%
      filter(Score == "All") %>%
      transmute(Exam.Subject, Total_Students = All.Students..2016.)
    
    # Students scoring above the average for their subject
    students_above <- exams %>%
      filter(Score %in% 1:5) %>%
      mutate(
        Score_num = as.numeric(Score),
        Count     = All.Students..2016.
      ) %>%
      left_join(exam_avg, by = "Exam.Subject") %>%
      filter(Score_num > Avg_Score) %>%
      group_by(Exam.Subject) %>%
      summarise(Students_Above = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      left_join(total_students, by = "Exam.Subject") %>%
      mutate(Pct_Above = 100 * Students_Above / Total_Students)
    
    # Combined summary for all subjects
    subject_summary <- exam_avg %>%
      left_join(students_above, by = "Exam.Subject") %>%
      mutate(
        Students_Above = ifelse(is.na(Students_Above), 0, Students_Above),
        Pct_Above      = ifelse(is.na(Pct_Above), 0, Pct_Above)
      )
    
    ggplot(subject_summary,
           aes(x = reorder(str_to_lower(Exam.Subject), Avg_Score),
               y = Avg_Score)) +
      geom_col(aes(fill = Pct_Above), width = 0.6) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "% Above Avg") +
      geom_text(aes(label = sprintf("%.1f%%", Pct_Above)),
                hjust = -0.2, size = 3.1) +
      labs(
        title = "Average AP Exam Score and % of Students Above Average (2016)",
        x = "AP Subject",
        y = "Average Score (1–5)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(size = 12),
        axis.text.y     = element_text(size = 7)
      )
  })
}

#==========================================================
# 5. Run App
#==========================================================
shinyApp(ui = ui, server = server)
