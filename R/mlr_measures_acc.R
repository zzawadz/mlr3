#' @include MeasureClassif.R
MeasureClassifACC = R6Class("MeasureClassifACC",
  inherit = MeasureClassif,
  public = list(
    initialize = function(id = "acc") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      Metrics::accuracy(actual = p$truth, predicted = p$response)
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("acc", MeasureClassifACC)
