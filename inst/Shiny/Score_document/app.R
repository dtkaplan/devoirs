# Shiny app to coordinate scoring a document

library(shiny)
library(shinyDirectoryInput)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

# Define UI for application that draws a histogram
ui <- page_navbar(

  # Application title
  #titlePanel("Devoirs Document Grader"),

  # Sidebar with a slider input for number of bins
  nav_panel("[Select document]",
            titlePanel("Select document to grade"),
            "Remember to save scores before choosing a new document!",
            directoryInput('directory',
                           label = 'Select course grading folder',
                           value = '~/Packages/devoirs/inst/Grading-example/'),
            span("Course name:     ", textOutput("course_name", inline=TRUE)),
            selectInput("document", "Select Document", choices = "NONE")
  ),

  # Show a plot of the generated distribution
  nav_panel("[score MC]",
            span("Document:", textOutput("doc_label_MC_score", inline=TRUE)),
            titlePanel("MC student by student"),
            rHandsontableOutput("MC_handson_mc", width = "100%" )
  ),
  nav_panel("[MC diagnostics]",
            span("Document:", textOutput("doc_label_MC_diag", inline=TRUE)),
            titlePanel("Performance question by question"),
            dataTableOutput("MC_diagnostics"),
  ),
  nav_panel("[Essays]",
            span("Document:", textOutput("doc_label_essay", inline=TRUE)),
            titlePanel("Essays"),
            rHandsontableOutput("essays_handson")
            ),
  nav_panel("[R Chunks]",
            span("Document:", textOutput("doc_label_R", inline=TRUE)),
            titlePanel("R Chunks"),
            rHandsontableOutput("R_handson")
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  values <- reactiveValues(course_name = "NO COURSE SELECTED YET")
  # choose a directory
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))

        # update the widget value
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )

  # change the working directory
  observeEvent(
    ignoreNULL = TRUE,
    input$directory,
    {
      values$directory <- readDirectoryInput(session, "directory")
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    values$directory,
    updateSelectInput(session, "document",
                      choices = c("NONE", document_names(values$directory)$docid),
                      selected = "NONE")
  )

  observeEvent(
    ignoreNULL = TRUE,
    input$document,
    {
      if (input$document == "NONE") { # nothing to do

        values$doc_summary <- NULL
      } else {
          values$doc_summary <-summarize_document(values$directory,
                                                  docid=input$document)
          values$document <- input$document
          values$MC_info <- score_MC(values$doc_summary$MC)
          output$MC_handson_mc <- renderRHandsontable(handsontableMC(values$doc_summary, home=values$directory))
          output$MC_diagnostics <- renderDataTable(values$MC_info$items)
          output$essays_handson <- renderRHandsontable(handsontableEssays(values$doc_summary))
          output$R_handson <- renderRHandsontable(handsontableR(values$doc_summary))
      }

    }
  )
  output$course_name <- renderText({values$course_name})

  output$doc_label_essays <- output$doc_label_MC_diag <-
    output$doc_label_MC_score <- output$doc_label_essay <-
    output$doc_label_R <-
    renderText({
      gsub("\\.(rmarkdown)$", "", values$document)
  })

  output$documentlist <- renderText({
    params <- get_course_params(values$directory)
    if (is.null(params)) return("Not in valid directory")
    paste(document_names(values$directory)$docid, collapse = "...")
    })

  observeEvent(
    values$directory,
    {
      params <- get_course_params(values$directory)
      if (is.null(params)) return("Not in valid directory")
      else values$course_name <- params$`course-name`
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
