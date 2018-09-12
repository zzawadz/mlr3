stri_list = function(initial, str, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

stri_peek = function(str, sep = " ", collapse = ", ", n = 10L) {
  x = paste(head(str, n), sep = sep, collapse = collapse)
  if (length(str) > n)
    x = paste(paste(x, collapse = collapse), "[...]")
  x
}

stri_suggest = function(str, candidates = character(0L), n = 3L) {
  n = min(n, length(candidates))
  if (n == 0L)
    return(character(0L))

  ii = order(adist(str, candidates, ignore.case = TRUE)[1L, ])
  candidates[head(ii, n)]
}