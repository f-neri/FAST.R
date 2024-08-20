. <- NULL # prevents R CMD note

get_feature_channels <- function(Input_files, input_morphological_feature_list, input_feature_list) {

  for (i in seq_len(nrow(Input_files))) {
    feature_list <- c()
    # Check Lengths
    if(length(input_morphological_feature_list) == 0) {
      print("ERROR: Input a morphological feature")
    } else if (length(input_morphological_feature_list) == 1) {
      # Take input from RShiny
      feature_list[[input_morphological_feature_list]] <- 0
      
    } else {
      print("ERROR: Too many morphological features")
    }
    
    if(length(input_feature_list) < 4 && length(input_feature_list) > 0) {
      for(ii in seq_along(input_feature_list)) {
        feature <- input_feature_list[ii]
        channel_name = paste0(feature, "_channel_number")
        feature_list[[feature]] <- ii #+ length(input_morphological_feature_list)
      }
    }
  }
  # return feature_list
  feature_list
}