library(shiny)
library(shinyTime)
# library(shinyDirectoryInput) # from wleepang/shiny-directory-input
library(devoirs)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} document reporting",
  theme = bslib::bs_theme(bootswatch = "flatly"),

  nav_panel("[Select]",
            #textOutput("valid_home_dir"),
            fluidRow(
              column(4,
                     selectizeInput("sections",
                                    "Select class section",
                                    choices = LETTERS, multiple = TRUE)),
              column(4,
                     selectInput("document", "Select Document",
                                 choices = c(), selected = character(0))),
              column(4,
                     selectizeInput("item", "Select Items", choice = c(),
                                    multiple = TRUE, selected = character(0))),
              textOutput("how_many_essays", inline = TRUE),
              tags$hr(),
              tags$p("Generate CSV overall-score reports for this document for
                     each of the selected sections. They will be in the <REPORTS>
                     directory, whence you can upload them to your gradesheet."),
              actionButton("write_reports", "Write reports"),
                column(6,
                       dateInput("deadline_date", "Deadline date:", value = Sys.Date())
                       ),
                column(6, timeInput("deadline_time", "Deadline time:", value = "23:00:00",
                        seconds = FALSE))
              )
            ),
  nav_panel("[View essays]",
            actionButton("save_quarto", "Save to Quarto file."),
            uiOutput("essay_report")
  ),
  nav_panel("[MC summary]",
            "Summarizes the MC scores for all items in the selected document.",
            tableOutput("MCsummary")
  ),
  nav_panel("[MC item summary]",
            "Shows class performance for all MC items (across all sections)",
            tableOutput("MCitems")
  ),
  nav_panel("[Multiple Choice]",
            "Shows scores for all multiple-choice items in the selected document",
            tableOutput("MCscores")
  ),

  nav_panel("[Close & Update]",
            actionButton("do_update", "Close and Update submissions"),
            actionButton("just_close", "Close without Updating submissions.")
  )

)
