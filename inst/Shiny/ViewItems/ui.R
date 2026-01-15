library(shiny)
library(shinyTime)
# library(shinyDirectoryInput) # from wleepang/shiny-directory-input
library(devoirs)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} document reporting",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    "Hello",
    selectizeInput("sections",
                   "Select class section",
                   choices = LETTERS, multiple = TRUE),
    "You must select at least one section to see reports.",
    selectInput("document", "Select Document",
                choices = c(), selected = character(0)),
    dateInput("deadline_date", "Deadline date:", value = Sys.Date()),
    timeInput("deadline_time", "Deadline time:", value = "23:00:00",
                        seconds = FALSE),
    tags$p("Generate CSV total score reports for each selected section. They will be in the <REPORTS>
                     directory, whence you can upload them to your gradesheet."),
    actionButton("write_reports", "Write reports", width = 200),
    actionButton("just_close", "Close App.", width = 200, style="color: red;")

    ),
  nav_panel("[Essays]",
            selectizeInput("item", "Select Items", choice = c(),
                           multiple = TRUE, selected = character(0)),
            "Please select one or more items to see the essays.",
            actionButton("save_quarto", "Save essays to Quarto file for easy reading."),
            uiOutput("essay_report")
  ),
  nav_panel("[MC scores]",
            "Summarizes by student the MC scores for all items in the selected document.",
            tableOutput("MCsummary")
  ),
  nav_panel("[MC items]",
            "Shows class performance for each MC item",
            tableOutput("MCitems")
  ),
  nav_panel("[Raw MC]",
            "Shows scores for all multiple-choice items in the selected document",
            tableOutput("MCscores")
  ),


)
