#' @include MeasureClassif.R
MeasureClassifMMCE = R6Class("MeasureClassifMMCE",
  inherit = MeasureClassif,
  public = list(
    initialize = function(id = "mmce") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      Metrics::ce(actual = p$truth, predicted = p$response)
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("mmce", MeasureClassifMMCE)
