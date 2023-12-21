# Aaron Smith
# 8.10.2023
#
# Task: Generate a bslib Dashboard.
#
################################################################################
# Data Prep ----
library(tidyverse)
library(janitor)
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)
library(png)

################################################################################
# Define UI ----
ui <- page_sidebar(
  theme = bs_theme(preset = "superhero", bg = "#660000", fg = "white"),
  
  
  title = "Dashboard",
  
  sidebar = sidebar(
    selectInput(
      inputId = "year",
      label = "Year",
      choices = names(year_options),
      selected = year_options$`2001-02`
    ),
    
    selectInput(
      inputId = "comparison_institution",
      label = "Select a Comparison Institution",
      choices = names(comparison_school_options),
      selected = comparison_school_options$`Comparison Institutional Average`
    ),
    
    width = 400
    
  ), 
  
  
  cards <- list(
    
    navset_card_tab(
      nav_panel(
        title = "Types & Salaries",
          layout_columns(
            card(
              full_screen = FALSE,
              card_header(textOutput(outputId = "Faculty_Pie_Text")),
              plotlyOutput(outputId = "Faculty_Graph"), 
              plotlyOutput(outputId = "Comparison_Faculty_Graph")
              
                ), 
            card(
              full_screen = FALSE,
              card_header(textOutput(outputId = "Faculty_Trends_Text")),
              plotlyOutput(outputId = "Faculty_Trends"), 
              plotlyOutput(outputId = "Comparison_Faculty_Trends")
            ), 
            
            card(
              full_screen = FALSE,
              card_header(textOutput(outputId = "Salary_Trends_Text")),
              plotlyOutput(outputId = "Salary_Trends") 
              
            )
          )
              ), 
      
      nav_panel(
        title = "Enrollment & Cost of Attendance", 
          layout_columns(
            card(
              full_screen = FALSE, 
              card_header(textOutput(outputId = "Enrollment_Trends_Text")), 
              plotlyOutput(outputId = "Enrollment_Trends")
            ), 
            card(
              full_screen = FALSE, 
              card_header(textOutput(outputId = "Student_Faculty_Ratio_Text")), 
              plotOutput(outputId = "Student_Faculty_Ratio"), 
              plotOutput(outputId = "Comparison_Student_Faculty_Ratio")
            ), 
            card(
              full_screen = FALSE,
              card_header(textOutput(outputId = "COA_Text")), 
              plotlyOutput(outputId = "Aid"), 
              plotlyOutput(outputId = "Comparison_Aid")
            )
          )
      )
      
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
# FACULTY
  # Filter data for the comparison institution
  yearly_comparison_faculty_data <- reactive({
  
    data <- excluded %>%
      filter(Year == year_options[[input$year]] 
              &  `Institution Name` == comparison_school_options[[input$comparison_institution]])
    
  })
  
  # Filter data for School A by year
  yearly_faculty_data <- reactive({
    
    data <- faculty_counts %>%
      filter(`Institution Name` == "School A" 
             & Year == year_options[[input$year]])
    
  })
  
  # Create a data frame for School A regardless of year
  trends_faculty_data <- reactive({
    
    data <- faculty_counts %>% filter(`Institution Name` == "School A")
    
  })
  
  # Create a data frame for Comparison Institutions regardless of year
  trends_comparison_faculty_data <- reactive({
    
    data <- faculty_counts %>% filter(`Institution Name` == comparison_school_options[[input$comparison_institution]])
    
  })
  
  
  # PIE CHARTS
  # Create the faculty graph title
  output$Faculty_Pie_Text <- renderText({

    
    faculty_text <- paste0("Faculty Type Distribution for School A vs. ", input$comparison_institution, " (", input$year, ")")
    faculty_text
  })
  
  # Generate the School A Faculty Pie Chart
  output$Faculty_Graph <- renderPlotly({

    faculty_pie <- plot_ly(yearly_faculty_data(), labels = ~Metric, values = ~Count, type = "pie", textposition = "inside",
                               insidetextfont = list(color = '#FFFFFF'),
                               marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
      
      layout(title = list(text = paste0("School A Faculty Type Distribution ", "(", input$year, ")"),
                          font = list(color = "white")),  # Set title text color to white
             scene = list(aspectmode = "cube"),
             margin = list(l = 50, r = 50, b = 50, t = 50),
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white")),  # Set legend text color to white
             paper_bgcolor = "black",  # Set the paper background color to black
             plot_bgcolor = "black")  
    
    faculty_pie

  })
  
  
  # Generate the Comparison Faculty Pie Chart
  output$Comparison_Faculty_Graph <- renderPlotly({
    
    comparison_faculty_pie <- plot_ly(yearly_comparison_faculty_data(), labels = ~Metric, values = ~Count, type = "pie", textposition = "inside",
                                      insidetextfont = list(color = '#FFFFFF'),
                                      marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
      
      layout(title = list(text = paste0(input$comparison_institution, " Faculty Type Distribution ", "(", input$year, ")"),
                          font = list(color = "white")),  # Set title text color to white
             scene = list(aspectmode = "cube"),
             margin = list(l = 50, r = 50, b = 50, t = 50),
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white")),  # Set legend text color to white
             paper_bgcolor = "black",  # Set the paper background color to black
             plot_bgcolor = "black")  
    
    comparison_faculty_pie
    
  })

  
  # TRENDS
  # Create the faculty trends title
  output$Faculty_Trends_Text <- renderText({
    faculty_trend_text <- paste0("Yearly Faculty Type Trends for School A vs. ", input$comparison_institution)
    faculty_trend_text
  })
  
  # Generate the Faculty Trends Graph
  output$Faculty_Trends <- renderPlotly({
    
    faculty_trends <- plot_ly(trends_faculty_data(), 
                                  x = ~as.character(Year), 
                                  y = ~Percent, 
                                  color = ~Metric, 
                                  mode = "lines+markers", 
                                  type = "scatter", 
                                  line = list(width = 2),  # Set the width of the lines to 2
                                  marker = list(size = 10),  # Set the size of the markers to 10
                                  text = paste0(trends_faculty_data()$Percent, "%", " (", input$year, ")"), 
                                  hoverinfo = "text") %>%
      layout(title = list(text = paste0("School A Faculty Type Trends"), 
                          font = list(color = "white", size = 20),  # Set title text color to white and increase the size
                          x = 0.5, y = 0.9, xanchor = "center", yanchor = "top"),  # Adjust title position
             xaxis = list(title = "Year", 
                          tickfont = list(color = "white", size = 14),  # Set x-axis text color to white and increase the size
                          color = "white"),  # Set x-axis line color to white
             yaxis = list(title = paste0("Percentage"), 
                          range = c(0, 100), 
                          tickfont = list(color = "white", size = 14),  # Set y-axis text color to white and increase the size
                          color = "white"),  # Set y-axis line color to white
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),  # Set legend text color to white and increase the size
             showlegend = TRUE,
             plot_bgcolor = "black",  # Set the plot background color to black
             paper_bgcolor = "black",  # Set the paper background color to black
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)  # Adjust the margins
      )
    
    faculty_trends
    
  })
  
  # Generate the Comparison Faculty Trends Graph
  output$Comparison_Faculty_Trends <- renderPlotly({
    
    comparison_faculty_trends <- plot_ly(trends_comparison_faculty_data(), 
                                         x = ~as.character(Year), 
                                         y = ~Percent, 
                                         color = ~Metric, 
                                         mode = "lines+markers", 
                                         type = "scatter", 
                                         line = list(width = 2),  # Set the width of the lines to 2
                                         marker = list(size = 10),  # Set the size of the markers to 10
                                         text = paste0(trends_comparison_faculty_data()$Percent, "%", " (", input$year, ")"), 
                                         hoverinfo = "text") %>%
      layout(title = list(text = paste0(input$comparison_institution, " Faculty Type Trends"), 
                          font = list(color = "white", size = 20),  # Set title text color to white and increase the size
                          x = 0.5, y = 0.9, xanchor = "center", yanchor = "top"),  # Adjust title position
             xaxis = list(title = "Year", 
                          tickfont = list(color = "white", size = 14),  # Set x-axis text color to white and increase the size
                          color = "white"),  # Set x-axis line color to white
             yaxis = list(title = paste0("Percentage"), 
                          range = c(0, 100), 
                          tickfont = list(color = "white", size = 14),  # Set y-axis text color to white and increase the size
                          color = "white"),  # Set y-axis line color to white
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),  # Set legend text color to white and increase the size
             showlegend = TRUE,
             plot_bgcolor = "black",  # Set the plot background color to black
             paper_bgcolor = "black",  # Set the paper background color to black
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)  # Adjust the margins
      )
    
    comparison_faculty_trends
    
  })
  
  
  
# SALARIES
  # Create a data frame for School A regardless of year
  trends_salary_data <- reactive({
    
    data <- all_salaries %>% filter(`Institution Name` %in% c(comparison_school_options[[input$comparison_institution]],
                                                              "School A"))
    
  })
  
  # TRENDS
  # Create the salary trends title
  output$Salary_Trends_Text <- renderText({
    salary_trends_text <- paste0("Yearly Salary Trends by Faculty Type for School A vs. ", input$comparison_institution)
    salary_trends_text
  })
  
  
  # Generate the Comparison Faculty Trends Graph
  output$Salary_Trends <- renderPlotly({
    
    salary_trends <- plot_ly(trends_salary_data(), 
                                         x = ~as.character(YEAR), 
                                         y = ~Salary, 
                                         color = ~`Institution Name`,
                                         colors = c("#660000", "white"),
                                         split = ~`Faculty Type`,
                                         mode = "lines+markers", 
                                         type = "scatter",
                                         symbol = ~`Faculty Type`,
                                         line = list(width = 2),  # Set the width of the lines to 2
                                         marker = list(size = 15),  # Set the size of the markers to 10
                                         text = paste0("$", trends_salary_data()$Salary, " (", input$year, ")"), 
                                         hoverinfo = "text") %>%
      layout(title = list(text = paste0("School A vs. ", input$comparison_institution, " Faculty Type Salary Trends"), 
                          font = list(color = "white", size = 15),  # Set title text color to white and increase the size
                          x = 0.5, y = 0.95, xanchor = "center", yanchor = "top"),  # Adjust title position
             xaxis = list(title = "Year", 
                          tickfont = list(color = "white", size = 14),  # Set x-axis text color to white and increase the size
                          color = "white"),  # Set x-axis line color to white
             yaxis = list(title = paste0("Salary ($)"), 
                          range = c(0, 140000), 
                          tickfont = list(color = "white", size = 14),  # Set y-axis text color to white and increase the size
                          color = "white"),  # Set y-axis line color to white
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),  # Set legend text color to white and increase the size
             showlegend = TRUE,
             plot_bgcolor = "black",  # Set the plot background color to black
             paper_bgcolor = "black",  # Set the paper background color to black
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)  # Adjust the margins
      )
    
    salary_trends
    
  })
  
  
# ENROLLMENT
  # Filter data for the comparison institution
  enrollment_data <- reactive({

    data <- all_students %>%
      filter(`Institution Name` %in% c(comparison_school_options[[input$comparison_institution]], "School A"))

  })

  # Create the enrollment trends text title
  output$Enrollment_Trends_Text <- renderText({
    enrollment_trends_text <- paste0("Yearly Enrollment by Gender for School A vs. ", input$comparison_institution)
    enrollment_trends_text
  })

  # Enrollment Trends
  output$Enrollment_Trends <- renderPlotly({

    enrollment_trends <- plot_ly(enrollment_data(),
                                 x = ~as.character(YEAR),
                                 y = ~`Gender Count`,
                                 color = ~`Institution Name`, 
                                 colors = c("#660000", "white"),
                                 split = ~`Gender Identifier`,
                                 mode = "lines+markers",
                                 type = "scatter",
                                 symbol = ~Gender,
                                 line = list(width = 2),
                                 marker = list(size = 15),
                                 text = paste0("Count: ", enrollment_data()$`Gender Count`, " (", input$year, ")"),
                                 hoverinfo = "text") %>%
      layout(title = list(text = paste0("School A vs. ", input$comparison_institution, " Enrollment Trends"),
                          font = list(color = "white", size = 15),
                          x = 0.5, y = 0.95, xanchor = "center", yanchor = "top"),
             xaxis = list(title = "Year",
                          tickfont = list(color = "white", size = 14),
                          color = "white"),
             yaxis = list(title = "Count",
                          range = c(0, 7500),
                          tickfont = list(color = "white", size = 14),
                          color = "white"),
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),
             showlegend = TRUE,
             plot_bgcolor = "black",
             paper_bgcolor = "black",
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)
      )

    enrollment_trends

  })

  # Create the Student-Faculty Ratio text title
  output$Student_Faculty_Ratio_Text <- renderText({
    student_faculty_ratio_text <- paste0("Student-to-Faculty Ratios for School A vs. ", input$comparison_institution, " (", input$year, ")")
    student_faculty_ratio_text
  })
  
  # Filter to Student/Faculty Ratio for a given year
  sf_data <- reactive({
    
    data <- all_students %>%
      select(YEAR, `Institution Name`, Student, Faculty) %>%
      filter(`Institution Name` == "School A" 
             & YEAR == year_options[[input$year]]) %>%
      distinct()
    
  })

  # Student-to-Faculty Ratio Graph 
  output$Student_Faculty_Ratio <- renderPlot({
    
    # Set the ratio values (adjust these values as needed)
    students <- sf_data()$Student
    faculty <- sf_data()$Faculty
    
    # Calculate the total number of individuals
    total_people <- students + faculty
    
    par(bg = "white")  # Set the background color


    # Create a blank plot
    plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, total_people), ylim = c(0, 1), axes = FALSE)

    # Function to plot images
    plot_people <- function(image_path, x, y, width = 0.2, height = 0.5) {
      img <- readPNG(image_path)
      rasterImage(img, x - width / 2, y - height / 2, x + width / 2, y + height / 2)
    }

    # Calculate the starting x-coordinate for centering the images
    start_x <- (total_people - (students + faculty - 1)) / 2

    # Plot students and faculty using images
    for (i in 1:students) {
      plot_people("Y:/", x = start_x + i - 1, y = 0.2, width = 1, height = 0.25)
    }

    # Calculate the x-coordinate for centering faculty images
    faculty_start_x <- total_people / 2 - (faculty / 2)

    # Plot faculty horizontally (centered)
    for (i in 1:faculty) {
      plot_people("Y:/", x = faculty_start_x + i - 1, y = 0.7, width = 2, height = 0.5)
    }
    
    # Add text displaying the total number of students
    text(x = -0.1, y = 0.6, 
         labels = paste0("Student/Faculty Ratio: ", students, "/1"),
         adj = 0, cex = 2)

    title(paste(input$year, "School A Student/Faculty Ratio"))


    
  })

  # Filter to Student/Faculty Ratio for a given year
  comparison_sf_data <- reactive({
    
    data <- all_students %>%
      select(YEAR, `Institution Name`, Student, Faculty) %>%
      filter(`Institution Name` == comparison_school_options[[input$comparison_institution]]
             & YEAR == year_options[[input$year]]) %>%
      distinct()
    
  })
  
  # Comparison Institution Student-to-Faculty Ratio Graph 
  output$Comparison_Student_Faculty_Ratio <- renderPlot({
    
    # Set the ratio values (adjust these values as needed)
    students <- comparison_sf_data()$Student
    faculty <- comparison_sf_data()$Faculty
    
    # Calculate the total number of individuals
    total_people <- students + faculty
    
    par(bg = "white")  # Set the background color
    
    # Create a blank plot
    plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, total_people), ylim = c(0, 1), axes = FALSE)
    
    # Function to plot images
    plot_people <- function(image_path, x, y, width = 0.2, height = 0.5) {
      img <- readPNG(image_path)
      rasterImage(img, x - width / 2, y - height / 2, x + width / 2, y + height / 2)
    }
    
    # Calculate the starting x-coordinate for centering the images
    start_x <- (total_people - (students + faculty - 1)) / 2
    
    # Plot students and faculty using images
    for (i in 1:students) {
      plot_people("Y:/", x = start_x + i - 1, y = 0.2, width = 1, height = 0.25)
    }
    
    # Calculate the x-coordinate for centering faculty images
    faculty_start_x <- total_people / 2 - (faculty / 2)
    
    # Plot faculty horizontally (centered)
    for (i in 1:faculty) {
      plot_people("Y:/", x = faculty_start_x + i - 1, y = 0.7, width = 2, height = 0.5)
    }
    
    # Add text displaying the total number of students
    text(x = -0.1, y = 0.6, 
         labels = paste0("Student/Faculty Ratio: ", students, "/1"),
         adj = 0, cex = 2)
    
    title(paste(input$year, input$comparison_institution, "Student/Faculty Ratio"))
    
  })
  
  # Create the Cost of Attendance text title
  output$COA_Text <- renderText({
    coa_text <- paste0("Yearly Overall Cost of Attendance for School A vs. ", input$comparison_institution)
    coa_text
  })

  # School A Aid data frame
  aid_data <- reactive({
    
    data <- cost_of_attendance %>%
      filter(`Institution Name` %in% "School A" 
             & YEAR == year_options[[input$year]])
      
  })
  
  # Aid Bar Graph
  output$Aid <- renderPlotly({

    aid_graph <- plot_ly(aid_data(), x = ~`Aid Type`, y = ~`Percent (%)`, type = "bar", color = ~`Aid Type`, 
                         text = paste0(aid_data()$`Percent (%)`, "%"), 
                         hoverinfo = "text") %>%
      layout(title = list(text = paste0("School A Financial Aid Distribution (", input$year, ")"), 
                          x = 0.5, y = 0.95, xanchor = "center", yanchor = "top",
                          font = list(size = 15, color = "white")),
             xaxis = list(title = "Aid Type", 
                          tickfont = list(size = 14, color = "white"), 
                          color = "white"),
             yaxis = list(title = "Percent (%)", 
                          tickfont = list(size = 14, color = "white"), 
                          color = "white", 
                          range = c(0, 100)), 
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),  # Set legend text color to white and increase the size
             showlegend = TRUE,
             plot_bgcolor = "black",  # Set the plot background color to black
             paper_bgcolor = "black",  # Set the paper background color to black
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4))  # Adjust the margins
    
    aid_graph
    
    
  })
  
  # Comparison Aid data frame
  comparison_aid_data <- reactive({
    
    data <- cost_of_attendance %>%
      filter(`Institution Name` %in% comparison_school_options[[input$comparison_institution]]
             & YEAR == year_options[[input$year]])
    
  })
  
  
  # Comparison Aid Bar Graph
  output$Comparison_Aid <- renderPlotly({
    
    comparison_aid_graph <- plot_ly(comparison_aid_data(), x = ~`Aid Type`, y = ~`Percent (%)`, type = "bar", color = ~`Aid Type`, 
                             text = paste0(comparison_aid_data()$`Percent (%)`, "%"), 
                             hoverinfo = "text") %>%
      layout(title = list(text = paste0(input$comparison_institution, " Financial Aid Distribution (", input$year, ")"), 
                          x = 0.5, y = 0.95, xanchor = "center", yanchor = "top",
                          font = list(size = 15, color = "white")),
             xaxis = list(title = "Aid Type", 
                          tickfont = list(size = 14, color = "white"), 
                          color = "white"),
             yaxis = list(title = "Percent (%)", 
                          tickfont = list(size = 14, color = "white"), 
                          color = "white", 
                          range = c(0, 100)), 
             legend = list(x = 1, y = 0.5, orientation = "v", font = list(color = "white", size = 14)),  # Set legend text color to white and increase the size
             showlegend = TRUE,
             plot_bgcolor = "black",  # Set the plot background color to black
             paper_bgcolor = "black",  # Set the paper background color to black
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4))  # Adjust the margins
    
    comparison_aid_graph
    
    
  })
  
}

# Running the app ----
shinyApp(ui = ui, server = server)