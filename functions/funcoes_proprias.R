#| label: fucoes-proprias
# A function to scale input to 0-1
scale_01 <- function(x){
	(x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
}

myfun <- function() {
	cat("hello!  \n")
	cat(c("one" = 1, "two" = 2))
}