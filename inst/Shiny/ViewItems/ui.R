library(shiny)
library(shinyDirectoryInput) # from wleepang/shiny-directory-input
library(devoirs)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} document reporting",
  theme = bslib::bs_theme(bootswatch = "flatly"),

  nav_panel("[Select]",
            fluidRow(
              column(6,
                     directoryInput("home_dir", label = "Grading directory"))
              ),
            textOutput("valid_home_dir"),
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
              textOutput("how_many_essays", inline = TRUE)
            )),
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
  )

)
