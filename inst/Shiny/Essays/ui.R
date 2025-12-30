library(shiny)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "{devoirs} grading",
  nav_panel("[Essays]",
            selectizeInput("sections", "Select class section", choices = LETTERS, multiple = TRUE),
            selectizeInput("student", "Select student", choices = "", multiple = TRUE),
            selectInput("document", "Select Document", choices = c()),
            selectInput("item", "Select Item", choice = c()),
            layout_columns(
              textOutput("current_student"),
              actionButton(inputId ="prev_student", label = icon("arrow-left")),
                          # style = "width: 50px; padding: 10px; font-size: 150%;"),
              actionButton(inputId ="next_student", label = icon("arrow-right")),
                          # style = "width: 50px; padding: 10px; font-size: 150%;"),
              col_widths = c(3, 1, 1)
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
