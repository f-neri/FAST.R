get_dim <- function(dims_plot, dim_value, version_value) {
  dims_plot %>% dplyr::filter(dim == dim_value, version == version_value) %>% .$value
}