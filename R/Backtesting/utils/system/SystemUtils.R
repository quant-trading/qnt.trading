indexExists <- function(ind, form) {
  if (ind[[1]] > length(form) || ind[[1]] < 1)
    return(FALSE)
  dim <- 1
  while (dim + 1 <= length(ind)) {
    if (ind[dim + 1] > length(form[[ ind[1:dim] ]] ) || ind[dim + 1] < 1)
      return(FALSE)
    dim <- dim + 1
  }
  return(TRUE)
}