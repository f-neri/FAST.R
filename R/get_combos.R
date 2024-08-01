get_combos <- function(input_features, special = NULL, permute_combn = "combn") {
  if (permute_combn == "combn") {
    combo_list <- utils::combn(input_features, 2, simplify = FALSE)
  } else {
    stop("Invalid permute_combn value.")
  }
  combo_list
}