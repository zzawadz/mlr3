rcbind = function(x, y) {
  if (ncol(x) == 0L)
    return(y)

  if (ncol(y) == 0L)
    return(x)

  if (nrow(x) != nrow(y))
    stopf("Tables have different number of rows (%s: %i, %s: %i)",
      deparse(substitute(x)), nrow(x), deparse(substitute(y)), nrow(y))

  ii = wf(names(x) %in% names(y))
  if (length(ii))
    stopf("Duplicated names: %s", names(x)[ii])

  x[, names(y) := y]
}

# assumes that foreach col in cols: each row is a named list of scalar values
# these lists get rbindlist()-ed and cbind()-ed to x
# the original columns are removed
flatten = function(x, cols) {
  for (col in cols) {
    tmp = rbindlist(x[[col]], fill = TRUE)
    x[, names(tmp) := tmp]
    x[, (col) := NULL]
  }
  x[]
}
