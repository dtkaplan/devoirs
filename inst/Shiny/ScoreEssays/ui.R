library(shiny)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

ui <- layout_sidebar(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    tags$h3("Score {devoirs} essays"),

    selectizeInput("sections", "Select class section",
                   choices = LETTERS, multiple = TRUE),
    checkboxInput("no_sections", "Disregard sections", value = FALSE),
    selectizeInput("student", "Select student",
                   choices = "", multiple = TRUE),

    selectInput("document", "Select Document", choices = c()),
    selectInput("item", "Select Item", choice = c()),
    tags$p(" "), tags$p(" "),
    #actionButton("do_update", "Close app and update submissions"),
    actionButton("just_close", "Close app.")
  ),
  checkboxInput("quick_score", label = "Jump to next student upon scoring."),
  tags$p(" "),
  splitLayout(cellWidths = c(50, 100, 50, 200),
    actionButton(inputId ="prev_student", label = icon("arrow-left")),
    textOutput("number_of_essays", inline=TRUE),
    actionButton(inputId ="next_student", label = icon("arrow-right")),
    textOutput("current_student", inline=TRUE)
  ),
  tags$p(" "),
  radioButtons("item_score", "Score assigned",
               choiceValues = c(99, 0:6),
               choiceNames = paste0("<-", c("none", 0:6), "  "), inline = TRUE,
               selected = 99),
  tags$p(" "),
  htmlOutput("essay")
  )
