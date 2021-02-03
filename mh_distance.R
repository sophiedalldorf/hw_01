mh_distance <- function(x, y) {
  if (typeof(x) != typeof(y))
  {warning("x and y are not both logical, numeric, or character")
    return(-1)}
  if ((is.infinite(x) == TRUE) || (is.infinite(y) == TRUE) || 
      (is.nan(x) == TRUE) || (is.nan(y) == TRUE) ||
      (is.na(x) == TRUE) || (is.na(y) == TRUE))
  {warning("x or y take an Na, NaN, Inf, or -Inf")
    return(-1)}
  if ((nchar(x) != nchar(y)) & (typeof(x) != "logical"))
  {warning("x and y do not have the same number of digits or letters")
    return(-1)}
  if ((is.numeric(x) == TRUE) & (is.numeric(y) == TRUE))
    if ((x %% 1 != 0) || (y %% 1 != 0))
    {warning("x or y contains decimal values")
      return(-1)}
  if (typeof(x) == "character")
  {xasalist = strsplit(x, "")
  yasalist = strsplit(y, "")
  xasavector = unlist(xasalist)
  yasavector = unlist(yasalist)
  numtruefalse = xasavector != yasavector
  return(sum(numtruefalse, na.rm = TRUE))}
  if (typeof(x) == "double")
  {xchar = toString(x)
  ychar = toString(y)
  xasalist = strsplit(xchar, "")
  yasalist = strsplit(ychar, "")
  xasavector = unlist(xasalist)
  yasavector = unlist(yasalist)
  numtruefalse = xasavector != yasavector
  return(sum(numtruefalse, na.rm = TRUE))}
  if (typeof(x) == "logical")
  {if (((x == TRUE) & (y == TRUE)) || ((x == FALSE) & (y == FALSE)))
  {return (0)}
    return(1)}
}