#' Calculate lag and lead of a variable
#'
#' @param rec A rec object
#' @param o_var Old variable to transform
#' @param n_var New variable name
#' @param type Lag of lead
#' @param shift_window The shifting window (integer)
#'
#' @return A rec object
#' @export
#'
step_shift <- function(rec, o_var, n_var, type = c("lag", "lead"), shift_window) {

  # making sure the only rec is passed into the function
  stopifnot(class(rec) == "rec")

  # get data
  data_t <- rec$data

  # shift variable
  if (type == "lag") {
    data_t[[n_var]] <- dplyr::lag(data_t[[o_var]],
                                  shift_window)
  }

  if (type == "lead") {
    data_t[[n_var]] <- dplyr::lead(data_t[[o_var]],
                                   shift_window)
  }

  # calculate stats
  norm_p_value <- shapiro.test(data_t[[n_var]])$p.value
  station_p_value <- Box.test(data_t[[n_var]])$p.value
  cor_resp <- cor(x = data_t[[n_var]], y = data_t[[rec$y]],
                  use = "complete.obs")

  # record meta
  meta <- tibble(
    id = max(rec$meta$id, 0) + 1,
    tranformation = type,
    original_vars = o_var,
    new_var = n_var,
    paramter = shift_window,
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
