#' @importFrom learnr question_ui_initialize
#' @importFrom learnr question_ui_completed
#' @importFrom learnr question_ui_try_again
#' @importFrom learnr question_is_valid
#' @importFrom learnr question_is_correct
#' @importFrom learnr mark_as
#' @importFrom learnr disable_all_tags
NULL

##### Text Block Questions #####

#' Text block question
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
question_text_block <- function(...) {
  learnr::question(
    ...,
    type = "text_block"
  )
}

#' @export
#' @seealso question_text_block
question_ui_initialize.text_block <-
  function(question, value = NULL, ...) {
    if (!is.null(value)) {
      # if an answer exists already, it should be displayed as is
      value <- value
    } else {
      # get the first answer
      value <- ""
    }
    textAreaInput(
      question$ids$answer,
      label = question$question,
      placeholder = question$options$placeholder,
      value = value
    )
  }

#' @export
#' @seealso question_text_block
question_is_correct.text_block <- function(question, value, ...) {
  return(learnr::mark_as(TRUE, messages = NULL))
}

#' @export
#' @seealso question_text_block
question_ui_try_again.text_block <-
  function(question, value, ...) {
    disable_all_tags(
      question_ui_initialize.text_block(question, value, ...)
    )
  }

#' @export
#' @seealso question_text_block
question_ui_completed.text_block <-
  function(question, value, ...) {
    disable_all_tags(
      question_ui_initialize.text_block(question, value, ...)
    )
  }

#' @export
#' @seealso question_text_block
question_is_correct.always_correct <- function(question, value, ...) {
  return(learnr::mark_as(TRUE, messages = NULL))
}


# nocov start

.onLoad <- function(...) {

  as_character_vector <- function(x) {
    # works for both x = NULL and x = list()
    if (length(x) == 0) {
      return(character(0))
    }
    unlist(x)
  }
}

# nocov end

