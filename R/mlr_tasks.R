#' @title Dictionary of example learning tasks
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_tasks
#' @family Dictionary
#' @family Task
#' @examples
#' mlr_tasks$keys()
#' as.data.table(mlr_tasks)
#' mlr_tasks$get("iris")
#' head(mlr_tasks$get("iris")$data())
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' b = DataBackendDataTable$new(data)
#' task = TaskClassif$new("iris.binary", b, target = "Species")
#' mlr_tasks$add("iris.binary", task)
#' mlr_tasks$keys()
#' mlr_tasks$get("iris.binary")
#' mlr_tasks$remove("iris.binary")
NULL

#' @include Dictionary.R
DictionaryTask = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)


#' @export
mlr_tasks = DictionaryTask$new()


#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(rbindlist(lapply(x$keys(), function(id) {
    t = x$get(id)
    data.table(id = id, type = t$task_type, nrow = t$nrow, ncol = t$ncol)
  })), "id")[]
}

load_dataset = function(id, package, keep.rownames = FALSE) {
  if (!length(find.package(package, quiet = TRUE)))
    stopf("Please install package '%s' for data set '%s'", package, id)
  ee = new.env(parent = emptyenv())
  data(list = id, package = package, envir = ee)
  if (!keep.rownames)
    rownames(ee[[id]]) = NULL
  ee[[id]]
}

mlr_tasks$add("iris", function() {
  b = DataBackendDataTable$new(data = load_dataset("iris", "datasets"))
  TaskClassif$new("iris", b, target = "Species")
})

mlr_tasks$add("sonar",  function() {
  b = DataBackendDataTable$new(data = load_dataset("Sonar", "mlbench"))
  TaskClassif$new("sonar", b, target = "Class", positive = "M")
})

mlr_tasks$add("bh",  function() {
  b = DataBackendDataTable$new(data = load_dataset("BostonHousing2", "mlbench"))
  TaskRegr$new("boston_housing", b, target = "medv")
})

mlr_tasks$add("pima", function() {
  b = DataBackendDataTable$new(data = load_dataset("PimaIndiansDiabetes2", "mlbench"))
  TaskClassif$new("pima_indians", b, target = "diabetes", positive = "pos")
})

mlr_tasks$add("zoo", function() {
  b = DataBackendDataTable$new(data = load_dataset("Zoo", "mlbench", keep.rownames = TRUE))
  TaskClassif$new("zoo", b, target = "type")
})

mlr_tasks$add("spam", function() {
  b = DataBackendDataTable$new(data = load_dataset("spam", "kernlab"))
  TaskClassif$new("spam", b, target = "type", positive = "spam")
})
