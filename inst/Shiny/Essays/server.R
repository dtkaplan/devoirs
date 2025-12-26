#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)




# Define server logic required to draw a histogram
function(input, output, session) {
  HOME <- reactive({getShinyOption("cwd", "/Users/kaplan/UATX/GRADING/QR-A-W26") })
  ITEMS <- reactiveVal()
  SCORES <- reactiveVal()
  STUDENTS <- reactiveVal() # am I using this
  current_n <- reactiveVal(1) # the index of the current student being graded
  # Start up---read in the items and scores
  initial_read <- reactive({
    if (!is_valid_directory(HOME())) stop("Not in a {devoirs} grading directory.")
    items <- devoirs:::get_old_items(HOME())
    scores <- devoirs:::read_score_keeper(HOME())
    if (nrow(items) == 0)
      stop("Cannot read items")
    ITEMS(items) # place into the reactive store
    SCORES(scores) # place into the reactive store
  })

  isolate(initial_read())

  observeEvent(input$do_update,
               {devoirs:::update_items(HOME())
                initial_read()},
               ignoreInit = TRUE)

  get_essay_documents_item <- reactive({
    ITEMS() |>
      dplyr::filter(is.na(correct)) |> # just the essays
      dplyr::select(docid, itemid) |>
      unique()
  })

  observe({
    tmp <- get_essay_documents_item() |> dplyr::select(docid) |> unique()
    updateSelectInput(session, "document",
                      choices = tmp$docid)
  })

  observe({
    tmp <- get_essay_documents_item() |>
      dplyr::filter(docid == input$document)
    updateSelectInput(session, "item",
                      choices = unique(tmp$itemid))
  })


  # Get all the students in the document essays so that
  # the "student" selectizeInput doesn't change as the
  # user jumps from one essay item to the next.
  get_students <- reactive({
    tmp <- ITEMS() |>
      dplyr::filter(docid == input$document) |>
      dplyr::select(student) |>
      unique()
    c(tmp$student, "debug1", "debug2", "debug3")
  })

  observe({
    updateSelectizeInput(session, "student",
                      choices = c(get_students(), "debug1", "debug2", "debug3"))
  })

  # Get the list of students currently selected
  selected_students <- reactive({
    if (input$all_students) get_students()
    else input$student
  })

  observeEvent(input$prev_student,
               { if (current_n() > 1) current_n(current_n() - 1)})

  observeEvent(input$next_student,
               { if (current_n() < length(selected_students())) current_n(current_n() + 1)})

  current_student <- reactive({
    selected_students()[current_n()]
  })

  current_essay <- reactive({
    # The essay and non-redundant copies for one student
    tmp <- ITEMS() |>
      dplyr::filter(
        docid == input$document,
        itemid == input$item,
        student == current_student()
      ) |>
      dplyr::arrange(desc(timestamp))
    repeats <- duplicated(tmp$contents)
    tmp <- tmp[!repeats, ]

    # Need to work in the timestamp and a score
    tmp |> dplyr::select(contents, timestamp)
  })

  output$current_student <- renderText(
    current_student()
  )

  output$current_essay <- renderTable(
    current_essay()
  )
}
