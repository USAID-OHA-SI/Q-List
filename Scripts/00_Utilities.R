# Utilities Functions

fy_months <- function(start = 10) {
  if(!start %in% 1:12) return(NULL)

  c(start:12, 1:(start-1))
}

