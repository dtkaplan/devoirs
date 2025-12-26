library(shiny)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

ui <- page_navbar(
  nav_panel("[Select document]",
            actionButton("do_update", "Update database"),
            selectInput("document", "Select Document", choices = "NONE"),
            textOutput("nsubmissions", inline=FALSE),
            selectInput("item", "Select Item", choices = "NONE")
  ),
  nav_panel("[Essays]",
            checkboxInput("all_students", label="All students?", value = TRUE),
            selectizeInput("student", "Select student", choices = "", multiple = TRUE),
            actionButton("prev_student", "Previous"), actionButton("next_student", "Next"),
            textOutput("current_student"),
            tableOutput("current_essay")
  )
)
