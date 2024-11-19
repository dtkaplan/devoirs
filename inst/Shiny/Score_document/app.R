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
    { document_choices <- document_names(values$directory)$docid
      document_choices <- gsub("\\.rmarkdown$", "", document_choices) |>
        sort()
      updateSelectInput(session, "document",
                        choices = c("NONE", document_choices),
                        selected = "NONE")
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    input$document,
    {
      if (input$document == "NONE") { # nothing to do
        values$doc_summary <- NULL
      } else {
          values$doc_summary <- summarize_document(values$directory,
                                                   docid = input$document)
          values$document <- input$document
          old_scores <- new.env()
          score_name <- glue::glue("{values$directory}Score_files/{values$document}-scores.rda")
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


          # output$MC_handson <- renderRHandsontable(handsontableMC(values$doc_summary, home=values$directory))
          # output$MC_diagnostics <- renderDataTable(values$MC_info$items)
          # output$essays_handson <- renderRHandsontable(handsontableEssays(values$doc_summary))


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
      store_name <- paste0(values$directory, "Score_files/", values$document, "-scores.rda")
      save(MC, Essays, RC, file = store_name)
    }
  )

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
