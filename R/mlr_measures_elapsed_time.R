#' @include Measure.R
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function(id = NA_character_, parts) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE
      )
      self$parts = assert_subset(parts, c("train", "predict"), empty.ok = FALSE)
    },

    calculate = function(e) {
      sum(e$timings[self$parts], na.rm = TRUE)
    }
  )
)

MeasureTimeTrain = R6Class("MeasureTimeTrain", inherit = MeasureElapsedTime,
  public = list(initialize = function(id = "time_train") super$initialize(id, "train"))
)

MeasureTimePredict = R6Class("MeasureTimePredict", inherit = MeasureElapsedTime,
  public = list(initialize = function(id = "time_predict") super$initialize(id, "predict"))
)

MeasureTimeBoth = R6Class("MeasureTimeBoth", inherit = MeasureElapsedTime,
  public = list(initialize = function(id = "time_both") super$initialize(id, c("train", "predict")))
)

#' @include mlr_measures.R
mlr_measures$add("time_train", MeasureTimeTrain)
mlr_measures$add("time_predict", MeasureTimePredict)
mlr_measures$add("time_both", MeasureTimeBoth)
