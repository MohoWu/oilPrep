#' Initialise a data pre-processing recipe
#'
#' @param data Data frame.
#' @param y The response variable.
#'
#' @importFrom tibble tibble
#'
#' @return A object of class 'rec'. The object is a list that contains a data
#'  frame containing the meta data of the transformation and the results of some
#'  statistical tests. A data frame containing the actual transformed data. And
#'  the response variable.
#'
#'  The columns in the meta data include:
#'   * id: The sequence id of the transformation
#'   * transformation: The transformation type
#'   * original_vars: The original variable(s) involved in the transformation
#'   * new_var: The name of the new variable as a result of the transformation
#'   * parameter: Any parameter used in the transformation, e.g. rolling window,
#'    leading, lagging order
#'   * norm_test: p-value from the Shapiro-Wilk’s normality test
#'   * station_test: p-value from the Ljung-Box test for stationarity
#'   * cor_resp: correlation coefficient with the target value.
#'
#'
#' @export
#'

rec <- function(data, y) {

  meta <- tibble(
    id = integer(),
    transformation = character(),
    original_vars = character(),
    new_var = character(),
    parameter = double(),
    norm_test = double(),
    station_test = double(),
    cor_resp = double()
  )

  data_t <- data

  output <- list(data = data_t,
                 meta = meta,
                 y = y)

  class(output) <- "rec"

  invisible(output)


}

#' Pseudo-function to re-export \strong{magrittr}'s pipe.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL
