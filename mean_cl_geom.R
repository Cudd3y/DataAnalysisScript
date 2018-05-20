# a helper function for calculating geometric mean
mean_cl_geom <- function(x) {
  log_x <- log(x)
  t_result_log <- t.test(log_x)
  tibble(
    y = exp(t_result_log$estimate),
    ymin = exp(t_result_log$conf.int[1]),
    ymax = exp(t_result_log$conf.int[2]))
}
