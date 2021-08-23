#' Calculate rolling standard deviation
#'
#' @param rec A rec object
#' @param o_var Old variable to transform
#' @param n_var New variable name
#' @param roll_window The rolling window as an integer.
#' @param type Mean or standard deviation
#'
#' @return A rec object
#' @export
#'
step_roll <- function(rec, o_var, n_var, type = c("mean", "sd"), roll_window) {

  # making sure the only rec is passed into the function
  stopifnot(class(rec) == "rec")

  # get data
  data_t <- rec$data

  # calculate rolling stats
  if (type == "mean") {
    data_t[[n_var]] <- roll::roll_mean(data_t[[o_var]],
                                       roll_window,
                                       complete_obs = TRUE)
  }

  if (type == "sd") {
    data_t[[n_var]] <- roll::roll_sd(data_t[[o_var]],
                                     roll_window,
                                     complete_obs = TRUE)
  }


  # calculate stats
  norm_p_value <- shapiro.test(data_t[[n_var]])$p.value
  station_p_value <- Box.test(data_t[[n_var]])$p.value
  cor_resp <- cor(x = data_t[[n_var]], y = data_t[[rec$y]],
                  use = "complete.obs")

  # record meta
  meta <- tibble(
    id = max(rec$meta$id, 0) + 1,
    tranformation = paste0("roll_", type),
    original_vars = o_var,
    new_var = n_var,
    paramter = roll_window,
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


