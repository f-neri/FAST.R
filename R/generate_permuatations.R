generate_permutations <- function(features) {
  feat1 <- features[1]
  feat2 <- features[2]
  
  # Generate all unique permutations of positive/negative
  permutations <- c(
    paste0(feat1, "_positive_", feat2, "_positive"),
    paste0(feat2, "_positive_", feat1, "_negative"),
    paste0(feat2, "_negative_", feat1, "_positive"),
    paste0(feat1, "_negative_", feat2, "_negative")
  )
  unique(permutations)
}