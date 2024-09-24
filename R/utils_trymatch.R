#' Error handling with regex matching on condition message
#'
#' @description
#' Evaluate an expression and, if it throws an error, attempts to match it
#' to one or more options provided to `...`..
#'
#' @inherit stopWithMatch details
#' @inherit stopWithMatch examples
#' @param expr Expression to evaluate
#' @param ... One or more named character vectors. Can be atomic, must be named.
#' @param .parent Logical, whether to show the parent call instead of the actual
#' call passed to tryMatch. Default TRUE.
#'
#' @return The value of `eval(expr)` if no error, otherwise thows a formatted
#' error
tryMatch <- function(expr, ..., .parent = TRUE) {
  # Defuse the expression so it can be deparsed into a string
  defused_expr <- rlang::enquo(expr)
  if (.parent){
    exprstr <- deparse1(sys.call(1))
  } else {
    exprstr <- deparse1(rlang::quo_get_expr(defused_expr))
  }
  # browser()
  tryCatch(
    rlang::eval_tidy(defused_expr),
    error = \(e) {


      stopWithMatch(e, ..., "exprstr" = exprstr)
      stop(e)
    }
  )
}

#' Augment error message
#'
#' Match an error message to arbitrary possibilities
#'
#' @details
#' The names of `...` correspond to strings that should be matched against the
#' condition message from the error. If there's a match for any particular
#' message, then display the corresponding value as an (i) bullet.
#' If an element of `...` is a character vector with length > 1, then the first
#' element will show a (i) bullet and the subsequent elements will appear on
#' their own lines with an indent instead of an (i).
#'
#' `stopWithmatch()` should be used instead of `tryMatch()` if you need to
#' also run additional code when handling the error. `tryMatch()` is a drop-in
#' replacement for `tryCatch()` iff all you need is to augment error messages.
#' See examples for more information.
#'
#' `exprstr` will be truncated if it is longer than 64 characters so that it
#' fits on a line of 80 characters.
#'
#'
#' @param e simpleError object
#' @param ... Series of named character vectors
#' @param exprstr Default `deparse1(sys.call(1))`, a string of the expression
#' to be evaluated to print for debugging purposes.
#'
#' @return Nothing, throws an error with the matches. When using this, the
#' original calling function should be followed by a `stop()` call in case there
#' are no matches.
#'
#' @examples
#' try(contrastable:::tryMatch(a + 1,
#'                             "not found" = "there is no a object!",
#'                             "'a' not" = c("oh no!", "how sad!")))
#'
#' foo <- function(a) {
#' tryCatch(a, error = \(e) {
#'   set.seed(111)
#'   message("An error! have a random number: ", rnorm(1))
#'   contrastable:::stopWithMatch(e, "not found" = "how sad!")
#'  })
#' }
#'
#' foo2 <- function(a) {
#'   contrastable:::tryMatch(a, "not found" = "how sad!")
#' }
#'
#' try(foo(a + 1))
#' try(foo2(a + 1))
stopWithMatch <- function(e, ..., exprstr = deparse1(sys.call(1))) {
  if (nchar(exprstr) > 64L)
    exprstr <- paste0(substr(exprstr, 1, 64), "...")

  exprstr <- paste0("In `", exprstr, "`: ")

  err <- conditionMessage(e)
  msgs <- rlang::dots_list(...)

  # If no messages are specified, then this acts as a normal tryCatch
  if (length(msgs) > 0L) {
    # Compare the names of ... to the error message; if there are any
    # matches, then format them as cli bullets and throw the error
    search_msg <- names(msgs)
    msg_match <- vapply(search_msg, \(x) grepl(x, err), logical(1))

    if (any(msg_match)) {
      show_msgs <- msgs[msg_match]

      # First element of each match should use i, subsequent elements
      # should just be indented
      bullet_types <-
        unlist(lapply(show_msgs, \(l) c("i", rep(" ", length(l)-1L))))
      show_msgs <- unlist(show_msgs)
      names(show_msgs) <- bullet_types

      stop(paste(exprstr,
                 err,
                 cli::format_error(show_msgs),
                 sep = "\n"),
           call. = FALSE)
    }
  }
}
