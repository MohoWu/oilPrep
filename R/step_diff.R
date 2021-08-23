#' Calculate the difference within a variable
#'
#' @param rec A rec object
#' @param o_var Old variable to transform
#' @param n_var New variable name
#'
#' @return A rec object
#' @export
#'
#' @details Difference for a variable is defined as the nth value - the (n-1)th value

step_diff <- function(rec, o_var, n_var) {

  # making sure the only rec is passed into the function
  stopifnot(class(rec) == "rec")

  # get data
  data_t <- rec$data

  # calc differencing
  data_t[[n_var]] <- data_t[[o_var]] - dplyr::lag(data_t[[o_var]], 1)

  # calculate stats
  norm_p_value <- shapiro.test(data_t[[n_var]])$p.value
  station_p_value <- Box.test(data_t[[n_var]])$p.value
  cor_resp <- cor(x = data_t[[n_var]], y = data_t[[rec$y]],
                  use = "complete.obs")

  # record meta
  meta <- tibble(
    id = max(rec$meta$id, 0) + 1,
    tranformation = "differencing",
    original_vars = o_var,
    new_var = n_var,
    paramter = 1,
    norm_test = norm_p_value,
    station_test = station_p_value,
    cor_resp = cor_resp
  )

  meta <- rbind(rec$meta, meta)

  output <- list(
    meta = meta,
    data = data_t,
    y = rec$y
  )

  class(output) <- "rec"

  invisible(output)


}
