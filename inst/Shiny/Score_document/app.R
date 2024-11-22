# Shiny app to coordinate scoring a document

library(shiny)
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
            titlePanel("Select course and document"),
            span("Course name:     ", textOutput("course_name", inline=TRUE)),
            actionButton("select_dir", "Select grading directory."),
            "Remember to save scores before choosing a new document!",
            selectInput("document", "Select Document", choices = "NONE")
  ),

  # Show a plot of the generated distribution
  nav_panel("[score MC]",
            span("Document:", textOutput("doc_label_MC_score", inline=TRUE)),
            titlePanel("MC student by student"),
            sliderInput("min_frac", "Min fraction", 0, 1, 0, step = 0.1),
            actionButton("save_MC", "Save scores"),
            rHandsontableOutput("MC_handson", width = "100%" )
  ),
  nav_panel("[MC diagnostics]",
            span("Document:", textOutput("doc_label_MC_diag", inline=TRUE)),
            titlePanel("Performance question by question"),
            dataTableOutput("MC_diagnostics"),
  ),
  nav_panel("[Essays]",
            span("Document:", textOutput("doc_label_essay", inline=TRUE)),
            titlePanel("Essays"),
            actionButton("save_Essays", "Save scores"),
            rHandsontableOutput("essays_handson")
            ),
  nav_panel("[R Chunks]",
            span("Document:", textOutput("doc_label_R", inline=TRUE)),
            titlePanel("R Chunks"),
            actionButton("save_R", "Save scores"),
            rHandsontableOutput("R_handson")
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(course_name = "Not in valid grading directory",
                           valid_directory = FALSE)
  # was a directory passed as an argument (named "cwd")?
  observe( {
    values$directory <- getShinyOption("cwd", ".")
    if (devoirs::is_valid_directory(home = values$directory))
      values$valid_directory <- TRUE
    cat("directory is ", values$directory, "\n")
  })


  # Respond to select directory button event
  observeEvent(
    ignoreNULL = TRUE,
    input$select_dir,
    {
      values$directory <-
        rstudioapi::selectDirectory(
          caption="Select the appropriate grading directory.")
      values$valid_directory <- devoirs::is_valid_directory(values$directory)
    }
  )


  # Display available documents for the current directory
  observeEvent(
    ignoreNULL = TRUE,
    values$directory,
    {
      if (values$valid_directory) {
        document_choices <- devoirs::document_names(values$directory)$docid
        document_choices <- gsub("\\.rmarkdown$", "", document_choices) |>
          sort()
        updateSelectInput(session, "document",
                          choices = c("NONE", document_choices),
                          selected = "NONE")
      } else {
        updateSelectInput(session, "document",
                          choices = c("NONE"),
                          selected = "NONE")
      }
    }
  )

  # Handle selection of document
  observeEvent(
    ignoreNULL = TRUE,
    c(values$valid_directory, input$document),
    {
      if (!values$valid_directory) return()

      if (input$document == "NONE") { # nothing to do
        values$doc_summary <- NULL
      } else if (devoirs::is_valid_directory(values$directory)) {
          values$doc_summary <- summarize_document(values$directory,
                                                   docid = input$document)
          values$document <- input$document
          # Read in any previous scores on this document
          old_scores <- new.env()
          score_name <- glue::glue("{values$directory}/Score_files/{values$document}-scores.rda")
          if (file.exists(score_name)) {
            load(score_name, envir = old_scores)
          } else {
            old_scores$MC <- old_scores$Essays <- old_scores$RC <- NULL
          }
          values$old_scores <- old_scores
      }
    },
    priority = 10
  )

  # Process the MC submissions
  observeEvent(
    ignoreNULL = TRUE,
    values$doc_summary,
    {
      Tmp <- score_MC(values$doc_summary$MC, min_frac = input$min_frac, document = values$document)
      values$MC_students <- Tmp$students |>
        dplyr::mutate(itemid = values$document)  # just to setup for merge_scores()
      values$MC_items <- Tmp$items

      old_scores <- values$old_scores$MC
      if (!is.null(old_scores)) {
        old_scores <- old_scores |>
          dplyr::mutate(itemid = values$document) # just to setup for merge_scores()
        old_scores <- old_scores |>
          dplyr::select(-n_correct, -weighted_correct, -raw_count)
      }
      Merged <- merge_scores(old = old_scores, new = values$MC_students)
      Merged <- Merged |>
        dplyr::select(n_correct, weighted_correct, raw_count, `0`, `1`, `2`, `3`, email)
      if (nrow(Merged) > 0) {
        forDisplay <- suppressWarnings(
          rhandsontable(Merged) |>
            hot_cols(columnSorting = TRUE)
        )
        output$MC_handson <- renderRHandsontable(forDisplay)
        output$MC_diagnostics <- renderDataTable(values$MC_items)
      } else {
        output$MC_handson <- NULL
        output$MC_diagnostics <- NULL
      }
    }
  )

  # Process the Essay submissions
  observeEvent(
    ignoreNULL = TRUE,
    values$doc_summary,
    {
      old_scores <- values$old_scores$Essays
      if (!is.null(old_scores)) {
        old_scores <- old_scores |> dplyr::select(-contents)
      }
      Merged <- merge_scores(old = old_scores,
                             new = values$doc_summary$Essays)
      # put in an empty table if <Merged> is empty

      if (nrow(Merged) != 0) {
        Merged <- Merged |>
          dplyr::select(itemid, contents, `0`, `1`, `2`, `3`, email)
        forDisplay <- suppressWarnings(
          rhandsontable(Merged) |>
            hot_cols(columnSorting = TRUE)
        )
        output$essays_handson <- renderRHandsontable(forDisplay)
      } else {
        output$essays_handson <- NULL
      }
    }

  )

  # Check if we are in a valid grading directory
  valid_directory <- reactive(devoirs::is_valid_directory(values$directory))
  course_params <- reactive({
    devoirs::get_course_params(values$directory, silent = TRUE)
  })
  course_name <- reactive({
    if (valid_directory()) {
        course_params()$`course-name`
      } else {
        "Not in valid grading directory"
      }
  })

  # Process the R submissions
  observeEvent(
    ignoreNULL = TRUE,
    values$doc_summary,
    {
      old_scores <- values$old_scores$RC
      if (!is.null(old_scores)) {
        old_scores <- old_scores |> dplyr::select(-code)
      }
      Merged <- merge_scores(old = old_scores,
                             new = values$doc_summary$R)
      if (nrow(Merged) != 0) {

        Merged <- Merged |>
          dplyr::select(itemid, code, `0`, `1`, `2`, `3`, email)
        forDisplay <- suppressWarnings(
          rhandsontable(Merged) |>
            hot_cols(columnSorting = TRUE)
        )
        output$R_handson <- renderRHandsontable(forDisplay)
      } else {
        output$R_handson <- NULL
      }
    }
  )

  # Put the name of the current document in each of the
  # scoring displays
  output$doc_label_essays <- output$doc_label_MC_diag <-
    output$doc_label_MC_score <- output$doc_label_essay <-
    output$doc_label_R <-
    renderText({
      gsub("\\.(rmarkdown)$", "", values$document)
  })

  output$documentlist <- renderText({
    if (valid_directory()) {
      paste(document_names(values$directory)$docid, collapse = "...")
    } else {
      return("NONE")
    }
  }
)
  observeEvent(
    ignoreNULL = TRUE,
    c(input$save_MC, input$save_Essays, input$save_R),
    {
      if (is.null(values$document)) return()
      MC <- hot_to_r(input$MC_handson)
      if (!is.null(MC)) {
        MC <- MC |>
          dplyr::mutate(score = ifelse(`3`, 3,
                                       ifelse (`2`, 2,
                                               ifelse (`1`, 1, 0)))) |>
          dplyr::select(-`0`, -`1`, -`2`, -`3`)
      }
      Essays <- hot_to_r(input$essays_handson)
      if (!is.null(Essays)) {
        Essays <- Essays |>
          dplyr::mutate(
            score = ifelse(`3`, 3,
                           ifelse (`2`, 2,
                                   ifelse (`1`, 1, 0)))) |>
          dplyr::select(-`0`, -`1`, -`2`, -`3`)
      }
      RC <- hot_to_r(input$R_handson)
      if (!is.null(RC)) {
        RC <- RC |>
          dplyr::mutate(score = ifelse(`3`, 3,
                                       ifelse (`2`, 2,
                                               ifelse (`1`, 1, 0)))) |>
          dplyr::select(-`0`, -`1`, -`2`, -`3`)
      }
      store_name <- paste0(values$directory, "/Score_files/", values$document, "-scores.rda")
      save(MC, Essays, RC, file = store_name)
    }
  )

  # Display the course name
  output$course_name <- renderText(course_name())
}

# Run the application
shinyApp(ui = ui, server = server)
