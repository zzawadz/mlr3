#' @include LearnerClassif.R
LearnerClassifRpart = R6Class("LearnerClassifRpart", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.rpart") {
      super$initialize(
        id = id,
        packages = "rpart",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        par_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
            ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L), ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
            ParamInt$new(id = "xval", default = 10L, lower = 0L)
          )
        ),
        properties = c("twoclass", "multiclass", "missings")
      )
    },

    train = function(task, ...) {
      rpart::rpart(task$formula, task$data(), ...)
    },

    predict = function(model, task, ...) {
      newdata = task$data(cols = task$feature_names)
      response = as.character(predict(model, newdata = newdata, type = "class"))
      prob = if (self$predict_type == "response") NULL else predict(model, newdata = newdata, type = "prob")

      PredictionClassif$new(task, response, prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.rpart", LearnerClassifRpart)
