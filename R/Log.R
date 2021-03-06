#' @title Learner Output Log
#'
#' @description
#' Object which stores the output of the `train` or `predict` step of an [Experiment].
#'
#' @section Usage:
#' ```
#' l = Log$new(log = NULL)
#' l$is_empty
#' l$has_condition(cl)
#'
#' l$format()
#' l$print()
#' ```
#'
#' @section Arguments:
#' * `log` (`data.table()`):
#'   Object as returned by [evaluate::evaluate()].
#'
#' * `cl` (`character(1)`):
#'   Class of a condition. One of "output", "message", "warning", or "error".
#'
#' @section Details:
#' `$new(log)` parses the object returned by [evaluate::evaluate()] and creates a new [Log].
#'
#' `$is_empty` is `TRUE` if there has been no log captured, either because logging was turned off or there was not a single line out output.
#'
#' `$has_condition(cl)` returns `TRUE` if at least on message of class `cl` is logged.
#'  Possible conditions are "output", "message", "warning", and "error".
#'
#' `format()` and `print()` are for formatting and printing via [base::format()] or [base::print()], respectively.
#'
#' @name Log
#' @examples
#' # Create a simple experiment and extract the train log:
#' e = Experiment$new(
#'  task = mlr_tasks$get("sonar"),
#'  learner = mlr_learners$get("classif.crashtest")
#' )
#' e$train(ctrl = exec_control(use_evaluate = TRUE))
#' log = e$data$train_log
#'
#' log$is_empty
#' log$has_condition("error")
#' log$print()
Log = R6Class("Log", cloneable = FALSE,
  public = list(
    is_empty = NULL,
    messages = NULL,
    initialize = function(log = NULL) {
      if (length(log) <= 1L) {
        self$is_empty = TRUE
        self$messages = data.table(msg = character(0L), class = factor(character(0L), levels = reflections$log_classes))
      } else {
        self$is_empty = FALSE
        self$messages = parse_evaluate(log)
      }
    },

    format = function() {
      sprintf("[%s] %s", self$messages$class, self$messages$msg)
    },

    print = function() {
      n = nrow(self$messages)
      catf("<Log> with %i message%s:", n, plural(n))
      if (n > 0L)
        catf(strwrap(paste0(seq_len(n), ": ", format(self)), exdent = nchar(n) + 2L))
    },

    has_condition = function(cl) {
      assert_choice(cl, reflections$log_classes)
      !self$is_empty && self$messages[list(cl), .N, on = "class", nomatch = 0L] > 0L
    }
  )
)

parse_evaluate = function(log) {
  translate_class = function(x) {
    if (is.character(x))
      return("output")
    if (inherits(x, "message"))
      return("message")
    if (inherits(x, "warning"))
      return("warning")
    if (inherits(x, "error"))
      return("error")
    stop("Unknown log class while parsing log")
  }

  log = log[-1L] # remove $src
  data.table(
    msg = vcapply(log, function(x) trimws(if (is.character(x)) x else x$message)),
    class = factor(vcapply(log, translate_class), levels = reflections$log_classes)
  )
}
