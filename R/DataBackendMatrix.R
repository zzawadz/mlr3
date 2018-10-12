#' @include DataBackend.R
#' @export
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(data) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      assert_names(colnames(data), type = "unique")

      rn = rownames(data)
      if (is.null(rn)) {
        self$primary_key = "<integer>"
      } else {
        assert_names(rownames(data), type = "unique")
        self$primary_key = "<rownames>"
      }
      private$.data = data
    },

    data = function(rows, cols, format = "sparse") {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, c("sparse", "data.table"))

      data = private$.data[rows, cols, drop = FALSE]
      if (format == "sparse") data else sparse_to_dt(data)
    },

    head = function(n = 6L) {
      n = min(n, nrow(private$.data))
      sparse_to_dt(private$.data[seq_len(n),, drop = FALSE])
    },

    distinct = function(cols) {
      cols = intersect(cols, colnames(private$.data))
      setNames(lapply(cols, function(col) distinct(private$.data[, col])), cols)
    }
  ),

  active = list(
    rownames = function() {
      if (self$primary_key == "<integer>") seq_row(private$.data) else rownames(private$.data)
    },

    colnames = function() {
      colnames(private$.data)
    },

    nrow = function() {
      nrow(private$.data)
    },

    ncol = function() {
      ncol(private$.data)
    }
  ),

  private = list(
    .data = NULL
  )
)

sparse_to_dt = function(x) {
  as.data.table(as.matrix(x))
}

if (FALSE) {
  data = Matrix::Matrix(0, nrow = 10, ncol = 100, sparse = TRUE)
  colnames(data) = sprintf("cn%04i", seq_len(ncol(data)))

  self = DataBackendMatrix$new(data)
  private = private(self)
  self$nrow
  self$ncol
  self$rownames
  self$colnames

  self$head()
  rows = 1:5
  cols = c("cn0001", "cn0002")
  self$data(rows, cols)
  self$data(rows, cols, format = "data.table")
}
