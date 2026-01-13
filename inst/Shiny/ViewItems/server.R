
library(shiny)

function(input, output, session) {
  HOME <- reactive({getShinyOption("cwd", "/Users/kaplan/UATX/GRADING/QR-A-W26") })
  report_text <- reactiveVal("Will appear when you select document and items.")
  PARAMS <- reactive({
    if (is_valid_directory(HOME()))
      devoirs::get_course_params(HOME())
    else list()
  })

  output$essay_report <- renderUI({
    shiny::markdown(report_text())
  })

  valid_dir <- reactive({
    is_valid_directory(HOME())
  })


  ## Replace devoirs:::essay_summary with a reactive???

  observe({
    if (nchar(input$document) >= 1 &&
        length(input$item) >= 1 &&
        length(input$sections) >=1) {
      tmp <- devoirs:::essay_summary(
        HOME(),
        doc_name = input$document,
        item_name = input$item,
        sections = input$sections,
        silent = TRUE)
      tmp <- devoirs:::essays_to_markdown(tmp)
    } else {
      tmp <- "Make selections for your report."
    }
    report_text(tmp)
  })

  ITEMS <- reactive({
    if (valid_dir()) devoirs:::get_old_ITEMS(HOME())
    else tibble::tibble() # empty
    })

  DOCUMENTS <- reactive({
    if (valid_dir() && nrow(ITEMS() > 0)) {
      c("All", ITEMS() |> dplyr::pull(docid) |> unique())
    } else
      character(0)
  })

  SECTIONS <- reactive({
    if (valid_dir() && length(PARAMS()) > 0) {
      c("All", PARAMS()$section$section |> unique(), "[-Unregistered-]")
    } else
      character(0)
  })

  observe({
    updateSelectizeInput(session, "sections", choices = SECTIONS(), selected = character(0))
  })

  observe({
    if (nrow(ITEMS()) > 0) {
      docs <- ITEMS() |> dplyr::pull(docid) |> unique()
      updateSelectInput(session, "document", choices = docs, selected = character(0))
      updateSelectizeInput(session, "item", selected = character(0))
    }
  })

  # Total MC scores for each student
  output$MCsummary <- renderTable({
    if (nchar(input$document) > 0) {
      tmp <- MC_doc_summary(home = HOME(),
                           docid = input$document,
                           Items = ITEMS(),
                           sections = input$sections)
      tmp
    } else tibble::tibble()
  })
  # Performance (across all students) of each item in the document
  output$MCitems <- renderTable({
    this_doc_items <- ITEMS() |>
      dplyr::filter(docid == input$document)
    if (nrow(this_doc_items) == 0) tibble::tibble()
    else MC_items(HOME(), Items = this_doc_items)
  })



  # MC scores broken down by student and item
  output$MCscores <- renderTable({
    if (nchar(input$document) > 0) {
    tmp <- report_doc_MC(home = HOME(),
                  docid = input$document,
                  Items = ITEMS(),
                  sections = input$sections)
    tmp
    } else tibble::tibble()
  })

  observe({
      if (nrow(ITEMS()) > 0 && !is.null(input$document)) {
        items <- ITEMS() |>
          dplyr::filter(docid == input$document) |>
          dplyr::pull(itemid) |> unique()
        updateSelectizeInput(session, "item", choices = items, selected = character(0))
      }
    }
  )

  quarto_file_name <- reactive({
    paste0(input$document, "-", Sys.Date(), ".qmd")
  })

  observe({
    name <- quarto_file_name()
    report_text() # for the dependency
    if (nchar(name) > 1) {
      updateActionButton(session, "save_quarto",
                         label = paste("Save report to", name))
    }
  })

  # To save the report to a quarto file
  observeEvent(input$save_quarto, {
    name <- quarto_file_name()
    cat(report_text(),
        file = paste0(HOME(), "/REPORTS/", name))
    updateActionButton(session, "save_quarto",
                       label = paste("Saved to", name))
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE
  )

  # To select the directory
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$home_dir
    },
    handlerExpr = {
      if (input$home_dir > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = ".") #readDirectoryInput(session, 'home_dir'))

        # update the widget value
        updateDirectoryInput(session, 'home_dir', value = path)
        HOME(path)
      }
    }
  )

  output$how_many_essays <- renderText({
    if (length(input$item) == 0) return("No item selected.")
    tmp <-
    if (any(nchar(input$item) == 0)) c()
    else {devoirs:::essay_summary(
      HOME(),
      doc_name = input$document,
      item_name = input$item,
      sections = input$sections,
      silent = TRUE) |>
      dplyr::filter(!is.na(contents)) |>
      dplyr::pull(student) |>
      unique()
    }


    if (length(tmp) > 0) {
      glue::glue("{length(tmp)} essays available. View in Essays tab.")
    } else {
      "No essays available for this item."
    }
  })

  output$valid_home_dir <- renderText({
    if (!is_valid_directory(HOME())) "ERROR: Must select valid grading directory."
    else "Proceed"
  })

  # set up to close the App when button pushed
  observeEvent(input$do_update,
               {devoirs:::update_items(HOME())
                 stopApp()},
               ignoreInit = TRUE)

  observeEvent(input$just_close,
               { stopApp() },
               ignoreInit = TRUE)

  # Write score reports into the REPORTS directory.
  observeEvent(input$write_reports,
               {score_document(home = HOME(),
                               docid = input$document,
                               sections = input$sections,
                               write = TRUE)},
               ignoreInit = TRUE,
               ignoreNULL = TRUE)

}
