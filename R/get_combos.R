get_combos <- function(input_features, special = NULL, permute_combn = "combn") {
  # regular combinations
  combo_list <- list()
  
  if(is.null(special) && permute_combn == "combn") {
    combo_list <- utils::combn(input_features, 2)
  }
  
  combo_list
}