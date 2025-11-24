library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)

# Load Data
exams <- read_csv("exams.csv", show_col_types = FALSE)

exams_clean <- exams %>%
  filter(Score %in% c("1", "2", "3", "4", "5")) %>%
  mutate(Score = as.numeric(Score)) %>%
  rename(Exam.Subject = `Exam Subject`)

# Grade Plot Data (Overall)
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

grade_avg <- grade_scores %>%
  group_by(Grade) %>%
  summarise(Average.Score = weighted.mean(Score, Count, na.rm = TRUE), .groups = "drop")


# Subject Plot Data
subject_scores <- exams_clean %>%
  group_by(Exam.Subject) %>%
  summarise(Average.Score = weighted.mean(Score, `Students (11th Grade)`, na.rm = TRUE))

# ------------------------------------------------------------
# Race Data (dat_3)
# ------------------------------------------------------------

exams_avg <- exams[exams$Score == "Average", ]
exams_avgr <- exams_avg[, c(1, 7:13)]

colnames(exams_avgr) <- c("Exam_Subject", "White", "Black",
                          "Hispanic_Latino", "Asian",
                          "Indian_Alaska_Native",
                          "Hawaiian_Pacific_Islander",
                          "Two_or_More_Races")

# Convert all numeric-like race columns to numeric
exams_avgr <- exams_avgr %>%
  mutate(across(-Exam_Subject, ~as.numeric(as.character(.))))

# Replace NA with 0
exams_avgr[is.na(exams_avgr)] <- 0

# Long format for race plot
dat_3 <- exams_avgr %>%
  pivot_longer(
    cols = c("White", "Black", "Hispanic_Latino", "Asian",
             "Indian_Alaska_Native", "Hawaiian_Pacific_Islander",
             "Two_or_More_Races"),
    names_to = "Race",
    values_to = "Avg_Score"
  )

dat_3$Exam_Subject <- factor(dat_3$Exam_Subject, 
                             levels = unique(dat_3$Exam_Subject))


# ------------------------------------------------------------
# Gender Data (dat_4)
# ------------------------------------------------------------

exams_avg1 <- exams_avg[, c(1, 5:6)]  # Male/Female columns
colnames(exams_avg1) <- c("Exam_Subject", "Male", "Female")

dat_4 <- exams_avg1 %>%
  pivot_longer(
    cols = c("Male", "Female"),
    names_to = "Gender",
    values_to = "Avg_Score"
  ) %>%
  mutate(Exam_Subject = factor(Exam_Subject, 
                               levels = unique(Exam_Subject)))


# UI
ui <- dashboardPage(
  dashboardHeader(title = "2016 AP Exam Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Grade", tabName = "grade", icon = icon("graduation-cap")),
      menuItem("Subject", tabName = "subject", icon = icon("book")),
      menuItem("Race & AP Score", tabName = "race", icon = icon("users")),
      menuItem("Gender & AP Score", tabName = "gender", icon = icon("venus-mars")),
      menuItem("Race & AP Subject",  tabName = "students_race",  icon = icon("th")),
      menuItem("Grade & AP Subject", tabName = "students_grade", icon = icon("graduation-cap"))
    )
  ),
  dashboardBody(
    tabItems(
      # ---- Grade Tab (Overall) ----
      tabItem(tabName = "grade",
              fluidRow(
                box(width = 12,
                    title = "Average Exam Score by Grade Level",
                    status = "primary",
                    solidHeader = TRUE,
                    checkboxGroupInput(
                      "grade_select",
                      "Select Grade(s):",
                      choices = c("11th Grade", "12th Grade"),
                      selected = c("11th Grade", "12th Grade"),
                      inline = TRUE
                    ),
                    plotlyOutput("gradePlot", height = "500px")
                )
              )
      ),
      
      # ---- Subject Tab (Interactive) ----
      tabItem(tabName = "subject",
              fluidRow(
                box(width = 12,
                    title = "Average Exam Score by Subject",
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput(
                      "subject_select",
                      "Select Subject(s):",
                      choices = unique(subject_scores$Exam.Subject),
                      selected = unique(subject_scores$Exam.Subject)[1:10],
                      multiple = TRUE
                    ),
                    plotlyOutput("subjectPlot", height = "600px")
                )
              )
      ),
      
      # ---- Race Tab ----
      tabItem(tabName = "race",
              h2("Does Race Affect Average AP Score?", style = "text-align:center;"),
              fluidRow(
                box(
                  width = 12,
                  title = "Interactive Race Plot: Average AP Score By Race",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "subject_race",
                    "Choose Exam Subject(s):",
                    choices = sort(unique(dat_3$Exam_Subject)),
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
                  plotOutput("racePlot_static", height = "450px")
                )
              )
      ),
      
      # ---- Gender Tab ----
      tabItem(tabName = "gender",
              h2("Does Gender Affect Average AP Score?", style = "text-align:center;"),
              fluidRow(
                box(
                  width = 12,
                  title = "Average AP Score By Gender",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "subject_gender",
                    "Choose Exam Subject(s):",
                    choices = sort(unique(dat_4$Exam_Subject)),
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
                  plotOutput("GenderPlot_static", height = "450px")
                )
              )
      ),
      
      # ---- Students CSV: Race & AP Subject ----
      tabItem(tabName = "students_race",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Interactive: AP Subject Participation by Race",
                    selectInput("topN_race_students", "Number of top subjects per race:",
                                choices = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "30" = 30, "All Subjects" = 37),
                                selected = 10),
                    plotOutput("students_racePlot", height = "800px")
                )
              )
      ),
      
      # ---- Students CSV: Grade & AP Subject ----
      tabItem(tabName = "students_grade",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Interactive: AP Subject Grade Mix",
                    selectInput("topK_grade_students", "Number of most popular subjects:",
                                choices = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "30" = 30, "All Subjects" = 37),
                                selected = 10),
                    plotOutput("students_gradePlot", height = "520px")
                )
              )
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # ---- Main App: exams.csv ----
  
  # Grade Plot (Overall)
  output$gradePlot <- renderPlotly({
    filtered <- grade_avg %>%
      filter(Grade %in% input$grade_select)
    
    p <- ggplot(filtered, aes(x = Grade, y = Average.Score, fill = Grade)) +
      geom_col() +
      labs(title = "Overall Average Exam Score by Grade",
           x = "Grade Level", y = "Average Score") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Subject Plot (Overall)
  output$subjectPlot <- renderPlotly({
    filtered <- subject_scores %>%
      filter(Exam.Subject %in% input$subject_select)
    
    p <- ggplot(filtered, aes(x = reorder(Exam.Subject, Average.Score), y = Average.Score)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Average Exam Score by Subject",
           x = "Exam Subject", y = "Average Score") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Race Plots (exams.csv)
  output$racePlot_interactive <- renderPlot({
    dat_3b <- dat_3 %>%
      filter(Exam_Subject %in% input$subject_race) %>%
      group_by(Exam_Subject) %>%
      mutate(Race = reorder(Race, -Avg_Score)) %>%
      ungroup()
    
    ggplot(dat_3b, aes(x = Race, y = Avg_Score, fill = Race)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      facet_wrap(~ Exam_Subject, ncol = 2, scales = "free_y") +
      labs(title = "Average AP Score By Race For Selected Subjects",
           x = "Race (Ordered by Average Score)",
           y = "Average AP Score",
           subtitle = "Data from College Board 2016") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  # Gender Plots
  output$genderPlot <- renderPlot({
    dat_4b <- dat_4 %>%
      filter(Exam_Subject %in% input$subject_gender) %>%
      group_by(Exam_Subject, Gender) %>%
      summarise(Avg_Score = mean(Avg_Score), .groups = "drop") %>%
      mutate(Gender = reorder(Gender, -Avg_Score))
    
    ggplot(dat_4b, aes(x = Gender, y = Avg_Score, fill = Gender)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Exam_Subject, ncol = 2) +
      scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
      theme_bw()
  })
  
  output$GenderPlot_static <- renderPlot({
    female_higher <- dat_4 %>%
      group_by(Exam_Subject) %>%
      summarise(female_mean = mean(Avg_Score[Gender == "Female"]),
                male_mean = mean(Avg_Score[Gender == "Male"]),
                female_higher = female_mean > male_mean)
    
    highlight_vec <- setNames(female_higher$female_higher, female_higher$Exam_Subject)
    
    label_fun <- function(subj_names) {
      sapply(subj_names, function(s) {
        if (highlight_vec[[s]]) paste0("* ", s) else s
      })
    }
    
    dat_4_ordered <- dat_4 %>%
      group_by(Exam_Subject) %>%
      summarise(mean_score = mean(Avg_Score)) %>%
      arrange(mean_score)
    
    dat_4$Exam_Subject <- factor(dat_4$Exam_Subject, levels = dat_4_ordered$Exam_Subject)
    
    ggplot(dat_4, aes(x = Avg_Score, y = Exam_Subject, color = Gender)) +
      geom_point() +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
      scale_y_discrete(labels = label_fun) +
      theme_bw()
  })
  
  # ---- Students CSV Tabs ----
  # -----------------------------
  # Students CSV: Load and prepare data
  # -----------------------------
  students <- read_csv("students.csv") %>%
    rename(Exam.Subject = `Exam Subject`)  # rename immediately
  
  students1 <- students %>%
    filter(!str_detect(Exam.Subject, "ALL SUBJECTS")) %>%
    rename_with(~ str_replace_all(., c("^Students \\(" = "", "\\)$" = "", "\\s*/\\s*" = "/")),
                starts_with("Students ("))
  
  race_cols <- c(
    "White", "Black", "Hispanic/Latino", "Asian",
    "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
    "Two or More Races", "Other Race", "Race Not Known"
  )
  
  grade_cols <- c(
    "9th Grade", "10th Grade", "11th Grade", "12th Grade",
    "Not High School", "> 9th Grade", "Grade Not Known"
  )
  
  students2 <- students1 %>%
    mutate(total_grade_sum = rowSums(across(all_of(grade_cols)), na.rm = TRUE))
  
  # -----------------------------
  # Students: Race Interactive
  # -----------------------------
  output$students_racePlot <- renderPlot({
    race_long <- students1 %>%
      select(Exam.Subject, all_of(race_cols)) %>%
      pivot_longer(cols = all_of(race_cols), names_to = "Race", values_to = "Count") %>%
      mutate(
        Count = coalesce(Count, 0),
        Race = factor(Race, levels = race_cols)
      ) %>%
      group_by(Race) %>%
      mutate(share = Count / sum(Count, na.rm = TRUE)) %>%
      ungroup()
    
    race_long_f <- race_long %>%
      group_by(Race) %>%
      slice_max(order_by = share, n = as.numeric(input$topN_race_students), with_ties = FALSE) %>%
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
      theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid=element_blank())
  })
  
  # -----------------------------
  # Students: Grade Interactive
  # -----------------------------
  output$students_gradePlot <- renderPlot({
    top_subjects <- students2 %>%
      slice_max(order_by = total_grade_sum, n = as.numeric(input$topK_grade_students), with_ties = FALSE) %>%
      pull(Exam.Subject)
    
    grade_long <- students2 %>%
      filter(Exam.Subject %in% top_subjects) %>%
      select(Exam.Subject, all_of(grade_cols)) %>%
      pivot_longer(all_of(grade_cols), names_to = "Grade", values_to = "Count") %>%
      mutate(
        Count = coalesce(Count, 0),
        Grade = factor(Grade, levels = grade_cols)
      ) %>%
      group_by(Exam.Subject) %>%
      mutate(
        subject_total = sum(Count, na.rm = TRUE),
        pct = if_else(subject_total > 0, Count / subject_total, 0)
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
      theme_minimal(base_size = 13)+theme(panel.grid=element_blank())
  })
  
}


# Run App
shinyApp(ui = ui, server = server)
