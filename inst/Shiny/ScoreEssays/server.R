library(shiny)
library(devoirs)
library(dplyr)

function(input, output, session) {
  HOME <- reactive({getShinyOption("cwd", "/Users/kaplan/UATX/GRADING/QR-A-W26") })
  ITEMS <- reactiveVal()
  SCORES <- reactiveVal()
  PARAMS <- reactiveVal() # course parameters
  score_flag <- reactiveVal(FALSE)
  STUDENT <- reactiveVal()
  student_n <- reactiveVal(1) # the index of the current student being graded

  # Start up---read in the items and scores
  initial_read <- reactive({
    if (!is_valid_directory(HOME())) stop("Not in a {devoirs} grading directory.")
    items <- devoirs:::get_old_ITEMS(HOME())
    scores <- devoirs:::read_score_keeper(HOME())
    # just get the most recent scores
    scores <- scores |>
      dplyr::arrange(desc(timestamp)) |>
      dplyr::filter(dplyr::row_number() == 1, .by = c(student, docid, itemid))
    if (nrow(items) == 0) {
      message("No items found in stores. Trying to update from collection site ....")
      from_collection <- devoirs:::update_items(HOME())
      if (nrow(from_collection) == 0) {
        message("There are no items yet at the collection site.\nRe-try when some items have been submitted.\n")
        stopApp()
      } else {
        message(paste0("succeeded with ", nrow(from_collection),  "items.\n Proceeding\n"))
        # start off with the ones that are already there.
        items <- from_collection
      }

    }
    ITEMS(items) # place into the reactive store
    SCORES(scores) # place into the reactive store
    tmp <- get_course_params(HOME())
    PARAMS(tmp)
    # fill in the sections choices
    updateSelectizeInput(session, "sections",
                         choices = c("All", sort(PARAMS()$sections$section)),
                         selected = "All")
    # Make a list of all students involved, registered or not
    tmp <- ITEMS() |> # Comprehensive: who submitted an item
      dplyr::filter(docid == isolate(input$document)) |>
      dplyr::select(student)
    students <- unique(c(tmp$student, PARAMS()$class_list))
    updateSelectizeInput(session, "student", choices = students,
                         selected = students[1])
  })

  # force initialization
  isolate(initial_read())

  # set up to close the App when button pushed
  observeEvent(input$do_update,
               {devoirs:::update_items(HOME())
                stopApp()},
               ignoreInit = TRUE)

  observeEvent(input$just_close,
               { stopApp() },
               ignoreInit = TRUE)
  # Doesn't change during run of app, just for initialization
  get_all_essays <- reactive({
    ITEMS() |>
      dplyr::filter(is.na(correct)) |> # just the essays
      dplyr::select(docid, itemid, contents, student) |>
      unique()
  })

  observe({
    tmp <- get_all_essays() |>
      dplyr::pull(docid) |>
      unique()
    updateSelectInput(session, "document", choices = tmp)
  })

  observeEvent(input$document, {
    tmp <- get_all_essays() |>
      dplyr::filter(docid == input$document) |>
      dplyr::pull(itemid) |>
      unique()
    if (length(tmp) > 0) {
      updateSelectInput(session, "item", choices = tmp, selected = tmp[1])
    }
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)


  observeEvent(input$item, {
    tmp <- STUDENTS_for_ITEM()
    student_n(1)
    STUDENT(tmp[student_n()])
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)

  output$number_of_essays <- renderText({
    input$item # for the dependency
    n <- length(STUDENTS_for_ITEM())
    this_one <- student_n()
    glue::glue("essay {this_one} of {n}")
  })

  # just those students who have been selected and answered the item
  STUDENTS_for_ITEM <- eventReactive(input$item, {
    doc <- req(isolate(input$document))
    item <- req(input$item)
    tmp <- get_all_essays() |>
      dplyr::filter(docid == doc, itemid == item) |>
      dplyr::pull(student) |>
      unique()
    # This should never be empty
    stu <- STUDENTS_SELECTED()
    tmp <- intersect(tmp, stu) # a vector
    if (length(tmp) == 0) stop("Expectation violated")
    # Reinitialize first student
    student_n(1)
    STUDENT(tmp[1])
    this_essay() # cause it to be displayed

    tmp
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)


  # When the choice for sections and/or student changes,
  # remake the STUDENTS_SELECTED() vector
  STUDENTS_SELECTED <- reactive(
    {
      secs <- input$sections
      # No sections selected, not even "All"
      # Just get the list from input$student
      if (is.null(secs)) return(input$student)

      give_back <-
        if ("All" %in% secs) {
          c(isolate(PARAMS()$sections$email),
            input$student)
        } else {
          tmp <- isolate(PARAMS()$sections) |>
            dplyr::filter(section %in% secs) |>
            dplyr::pull(email)
          c(tmp, input$student)
        }

      return(unique(give_back))
    })

### STUDENTS and SCORES
  # When the current score changes due to a STUDENT change,
  # change the radio buttons accordingly
  observeEvent(this_essay(), {
    score <- current_score()
    updateRadioButtons(session, "item_score", selected = score)
    score_flag(FALSE) # don't issue a new score item
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE
  )

  output$current_student <- renderText({
    req(STUDENT())
  })

# Get the current score
  current_score <- reactive({
      if (is.null(STUDENT())) {
        tmp <- tibble::tibble()
      } else {
        tmp <- isolate(SCORES()) |>
          dplyr::filter(
            docid == input$document,
            itemid == input$item,
            student == STUDENT()
          )
      }

      if (nrow(tmp) == 0) return(99) # flag for no score recorded
      # otherwise, there are some entries
      tmp <- tmp |>
        dplyr::mutate( # make NA timestamps deep in the past.
          timestamp =
            ifelse(is.na(timestamp),
                   Sys.time() - 1e10, timestamp)) |>
        dplyr::arrange(dplyr::desc(timestamp))

      tmp$score[1] # this is the latest one
    })

  observe({
    foo <- input$item_score
    new_score() # Will get intercepted if this is a new STUDENT()
  })

  # Store the newly changed value on score radio buttons
  new_score <- eventReactive(input$item_score, {
    val <- input$item_score |> as.numeric()

    if (score_flag()) {
      # ignore if the event came internally from a student change,
      # rather than from an instructor's click for a
      # new score.
      this_one <- devoirs:::add_new_score(val, input$document, STUDENT(), input$item, HOME())
      # enter it into the Scores() reactive so that it will be found the
      # next time we visit the essay
      isolate(SCORES(dplyr::bind_rows(isolate(SCORES()), this_one)))
      # If quick grading is on, move to the next student
      if (isolate(input$quick_score)) {
        jump_to_next()
      }
    } else {
      score_flag(TRUE)
    }
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)





  # Move display to the next student
  jump_to_next <- reactive({
    if (student_n() < length(STUDENTS_for_ITEM())) {
      student_n(student_n() + 1)
      STUDENT(STUDENTS_for_ITEM()[student_n()])
    }
  })
  # Move display to the previous student
  jump_to_previous <- reactive({
    if (student_n() > 1) {
      student_n(student_n() - 1)
      STUDENT(STUDENTS_for_ITEM()[student_n()])
    }
  })

  # Navigate between students with left-right buttons
  observeEvent(input$prev_student, {
    jump_to_previous()
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)

  observeEvent(input$next_student, {
    jump_to_next()
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE)

  # Get the current essay
  this_essay <- reactive({
      # Get the essay for one student, including any previous
      # non-redundant submissions. The most recent is on top.
    if (is.null(STUDENT())) {
      # There is no essay to get
      tmp <- tibble::tibble()
    } else {
      tmp <- ITEMS() |>
        dplyr::filter(
          docid == isolate(input$document),
          itemid == isolate(input$item),
          student == STUDENT()
        ) |>
        dplyr::filter(nchar(contents) > 0) |>
        dplyr::arrange(desc(timestamp))
      repeats <- duplicated(tmp$contents)
      tmp <- tmp[!repeats, ]
    }
      # Return what we found out
    Res <-
      if (nrow(tmp) == 0) {
        # no submissions from this student for this essay
        score <- 99 # no score given
        tibble::tibble(contents = "No essay submitted",
                       timestamp = Sys.time() - 1e10)

      } else {
        score <- isolate(current_score())
        tmp |> dplyr::select(contents, timestamp)
      }
    updateRadioButtons(session, "item_score", selected = score)
    score_flag(FALSE)

    Res
    })

  # format the essay and display it
  output$essay <- renderUI({
    tmp <- this_essay()
    str <- paste("<div>", tmp$timestamp, "::", "<p>", tmp$contents, "<p></div>")

    HTML(paste0(str, collapse="\n"))
  })



}
