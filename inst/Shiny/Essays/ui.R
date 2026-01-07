library(shiny)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} essay grading",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  nav_panel("[Essays]",
            fluidRow(
              column(5, selectizeInput("sections", "Select class section",
                                    choices = LETTERS, multiple = TRUE)),
              column(5, selectizeInput("student", "Select student", choices = "", multiple = TRUE))
              ),
            fluidRow(
              column(5, selectInput("document", "Select Document", choices = c())),
              column(5, selectInput("item", "Select Item", choice = c()))
            ),
            fluidRow(
              column(1, actionButton(inputId ="prev_student", label = icon("arrow-left"))),
              column(1, actionButton(inputId ="next_student", label = icon("arrow-right"))),
              column(4, textOutput("current_student", inline=TRUE)),
              column(6, textOutput("number_of_essays", inline=TRUE))
            ),
            radioButtons("item_score", "Score",
                         choiceValues = c(99, 0:6),
                         choiceNames = c("none", 0:6), inline = TRUE,
                         selected = 99),
            htmlOutput("essay")
  ),
  nav_panel("[Close & Update]",
            actionButton("do_update", "Close and Update submissions"),
            actionButton("just_close", "Close without Updating submissions.")
  )
)
