

# text_block questions from https://github.com/dtkaplan/etude
# Needed to convert these functions so that they are always correct, in order to make the progress bar work
question_ui_initialize.text_block <-
  function(question, value, ...) {
    textAreaInput(
      question$ids$answer,
      label = question$question,
      placeholder = question$options$placeholder,
      value = value
    )
  }
#' @export
question_is_correct.text_block <- function(question, value, ...) {
  return(learnr::mark_as(TRUE, messages = NULL))
}

#' @export
question_ui_try_again.text_block <-
  function(question, value, ...) {
    disable_all_tags(
      question_ui_initialize.text_block(question, value, ...)
    )
  }

question_ui_completed.text_block <- question_ui_try_again.text_block

question_is_correct.always_correct <- function(question, value, ...) {
  return(learnr::mark_as(TRUE, messages = NULL))
}


##### Sortable Rank Questions #####
question_rank <- function(..., random_answer_order = TRUE, options = sortable::sortable_options()) {
  learnr::question(
    ...,
    random_answer_order = random_answer_order,
    type = "sortable_rank",
    options = options
  )
}

question_ui_initialize.sortable_rank <- function(question, value, ...) {

  if (!is.null(value)) {
    # if an answer exists already, it should be displayed as is
    labels <- value
  } else {
    # get the first answer
    labels <- question$answers[[1]]$option

    # if we should randomize the order
    if (
      isTRUE(question$random_answer_order)
    ) {
      # shuffle the options
      labels <- sample(labels, length(labels))
    }
  }

  # return the sortable htmlwidget
  sortable::rank_list(
    text = question$question,
    input_id = question$ids$answer,
    labels = labels,
    options = question$options
  )
}

rank_ex <- sortable::question_rank(
  "Sort the first 5 letters",
  learnr::answer(LETTERS[1:5], correct = TRUE),
  learnr::answer(rev(LETTERS[1:5]), correct = FALSE, "Other direction!")
)

