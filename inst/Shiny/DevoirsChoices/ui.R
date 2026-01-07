library(shiny)
library(shinyDirectoryInput) # from wleepang/shiny-directory-input
library(devoirs)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} reporting",
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
                                    multiple = TRUE, selected = character(0)))
            )),
  nav_panel("[View essays]",
            actionButton("save_quarto", "Save to Quarto file."),
            uiOutput("essay_report")
  )

)
