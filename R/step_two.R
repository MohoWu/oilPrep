#' Calculate the spread, ratio and product of two variable
#'
#' @param rec A rec object
#' @param o_vars A character vector of length 2, specifying the two variables to
#'  include in the calculation.
#' @param n_var The name of the new variable
#' @param type Which calculation to perform, "spread", "ratio" or "product"
#'
#' @return A rec object
#' @export
#'

step_two<- function(rec, o_vars, n_var, type = c("spread", "ratio", "product")) {

  # making sure the only rec is passed into the function
  stopifnot(class(rec) == "rec")

  # making sure o_vars contains two variables
  stopifnot(length(o_vars) == 2)

  # get data
  data_t <- rec$data

  # calculate two variable results
  if (type == "spread") {
    data_t[[n_var]] <- data_t[[o_vars[1]]] - data_t[[o_vars[2]]]
  }

  if (type == "ratio") {
    data_t[[n_var]] <- data_t[[o_vars[1]]] / data_t[[o_vars[2]]]
  }

  if (type == "product") {
    data_t[[n_var]] <- data_t[[o_vars[1]]] * data_t[[o_vars[2]]]
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
    original_vars = paste(o_vars, collapse = ";"),
    new_var = n_var,
    paramter = NA_real_,
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
