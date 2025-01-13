# Shiny app to coordinate scoring a document

library(shiny)
library(devoirs)
library(rhandsontable)
library(bslib)
library(DT)

### FOR DEBUGGING


# Define UI for application that draws a histogram
ui <- page_navbar(

  # Sidebar with a slider input for number of bins
  nav_panel("[Select document]",
            textOutput("course_name", inline=FALSE),
            selectInput("document", "Select Document", choices = "NONE"),
            textOutput("nsubmissions", inline=FALSE),
            checkboxInput("update_submissions", "Get new submissions", value=FALSE)
  ),

  # Show a plot of the generated distribution
  nav_panel("[score MC]",
            span("Document:", textOutput("doc_label_MC_score", inline=TRUE)),
            titlePanel("MC student by student"),
            sliderInput("min_frac", "Min fraction", 0, 1, 0, step = 0.1),
            actionButton("save_MC", "Save scores"),
            rHandsontableOutput("MC_handson", width = "100%")
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
            rHandsontableOutput("essays_handson", height = 500)
            ),
  nav_panel("[R Chunks]",
            span("Document:", textOutput("doc_label_R", inline=TRUE)),
            titlePanel("R Chunks"),
            actionButton("save_R", "Save scores"),
            rHandsontableOutput("R_handson")
  )
)

server <- function(input, output, session) {

  STATE <- reactiveValues() # place to store current document summary, submissions, etc.

  homedir <- reactive({getShinyOption("cwd", "~UATX/GRADING/QR2-W25") })
  course_name <- reactive({get_course_params(homedir())$course_name})
  valid_dir <- reactive({devoirs::is_valid_directory(homedir())})
  submissions <- reactive({STATE$submissions})
  doc_summary <- reactive({STATE$doc_summary})
  observe({
    STATE$submissions <- devoirs::get_historic_data(homedir())
  }, priority = 100)
  submissions <- reactive({ STATE$submissions })

  output$course_name <- renderText(
    glue::glue("{{devoirs}} graphical grader. Course: {course_name()}")
  )

  output$nsubmissions <- renderText({
    tmp <- document_data()
    if (input$document == "NONE") {
      results <- tmp |> dplyr::summarize(count = sum(count),
                              earliest = min(earliest),
                              latest = max(latest),
                              ndocuments = dplyr::n())
      glue::glue("{results$ndocuments} documents altogether.\n Total submissions: {results$count} \nfrom {results$earliest} \nto {results$latest}")

    } else {
      results <- tmp |> dplyr::filter(docid == input$document)
      glue::glue("Total submissions: {results$count} from {results$earliest} to {results$latest}")

    }

  })

  document_data <- reactive({
    if (nrow(submissions()) == 0) return(tibble::tibble())
    submissions() |>
        # Only keep those that pass the test for validity
        dplyr::filter(devoirs:::submission_valid_contents(contents)) |>
        # Construct a summary
        dplyr::summarize(
          count = dplyr::n(),
          earliest = min(Timestamp),
          latest = max(Timestamp),
          .by = docid) |>
        dplyr::mutate(docid = gsub("\\.rmarkdown$", "", docid)) |>
        dplyr::arrange(desc(docid))
  })

  observeEvent(
    ignoreNULL = TRUE,
    input$update_submissions,
    {
      if (valid_dir()) {
        STATE$submissions <- update_submissions(homedir())
      } else stop("Not in valid directory")
    }
  )

  # Display available documents for the current directory
  observe({
    if (nrow(document_data()) > 0) { # are there any submissions
      document_choices <- document_data()$docid |> sort()
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
  observe({
      if (!valid_dir()) return()

      if (input$document == "NONE") { # nothing to do
        shiny::isolate(STATE$doc_summary <- NULL)
      } else {
          shiny::isolate(STATE$doc_summary <- summarize_document(homedir(),
                                                  docid = input$document))
          # Read in any previous scores on this document
          old_scores <- new.env()
          score_name <- glue::glue("{homedir()}/Score_files/{input$document}-scores.rda")
          if (file.exists(score_name)) {
            load(score_name, envir = old_scores)
          } else {
            old_scores$MC <- old_scores$Essays <- old_scores$RC <- NULL
          }
          shiny::isolate(STATE$old_scores <- old_scores)
      }
    },
    priority = 10
  )

  # Process the MC submissions
  observeEvent(
    ignoreNULL = TRUE,
    STATE$doc_summary,
    {
      Tmp <- score_MC(STATE$doc_summary$MC, min_frac = input$min_frac, document = STATE$document)
      STATE$MC_students <- Tmp$students |>
        dplyr::mutate(itemid = STATE$document)  # just to setup for merge_scores()
      STATE$MC_items <- Tmp$items

      old_scores <- STATE$old_scores$MC
      if (!is.null(old_scores)) {
        old_scores <- old_scores |>
          dplyr::mutate(itemid = STATE$document) # just to setup for merge_scores()
        old_scores <- old_scores |>
          dplyr::select(-n_correct, -weighted_correct, -raw_count)
      }
      Merged <- merge_scores(old = old_scores, new = STATE$MC_students)
      if (nrow(Merged) > 0) {
        Merged <- Merged |>
                dplyr::select(n_correct, weighted_correct, raw_count, `0`, `1`, `2`, `3`, email)

        forDisplay <- suppressWarnings(
          rhandsontable(Merged) |>
            hot_cols(columnSorting = TRUE)
        )
        output$MC_handson <- renderRHandsontable(forDisplay)
        output$MC_diagnostics <- renderDataTable(STATE$MC_items)
      } else {
        output$MC_handson <- NULL
        output$MC_diagnostics <- NULL
      }
    }
  )

  # Process the Essay submissions
  observeEvent(
    ignoreNULL = TRUE,
    STATE$doc_summary,
    {
      old_scores <- STATE$old_scores$Essays
      if (!is.null(old_scores)) {
        old_scores <- old_scores |> dplyr::select(-contents)
      }
      Merged <- merge_scores(old = old_scores,
                             new = STATE$doc_summary$Essays)
      # put in an empty table if <Merged> is empty

      if (nrow(Merged) != 0) {
        Merged <- Merged |>
          dplyr::select(itemid, contents, `0`, `1`, `2`, `3`, email) |>
          dplyr::mutate(contents = stringr::str_wrap(contents, width = 80, exdent=3)) |>
          dplyr::mutate(contents = gsub("  ", "  ", contents))
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





  # Process the R submissions
  observeEvent(
    ignoreNULL = TRUE,
    STATE$doc_summary,
    {
      old_scores <- STATE$old_scores$RC
      if (!is.null(old_scores)) {
        old_scores <- old_scores |> dplyr::select(-code)
      }
      Merged <- merge_scores(old = old_scores,
                             new = STATE$doc_summary$R)
      if (nrow(Merged) != 0) {

        Merged <- Merged |>
          dplyr::select(itemid, code, `0`, `1`, `2`, `3`, email)
        Merged <- Merged |> dplyr::mutate(code = ifelse(nchar(code) > 100, substr(code,1,100), code))
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
      gsub("\\.(rmarkdown)$", "", STATE$document)
  })

  output$documentlist <- renderText({
    if (valid_dir()) {
      paste(document_names(homedir())$docid, collapse = "...")
    } else {
      return("NONE")
    }
  }
)
  observeEvent(
    ignoreNULL = TRUE,
    c(input$save_MC, input$save_Essays, input$save_R),
    {
      if (is.null(STATE$document)) return()
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
      store_name <- paste0(homedir(), "/Score_files/", STATE$document, "-scores.rda")
      save(MC, Essays, RC, file = store_name)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
