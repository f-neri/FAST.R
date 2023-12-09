generate_example_single_cell_data <- function() {
  set.seed(123)  # For reproducibility
  
  # Function to generate data for a single plate
  generate_plate_data <- function(plate_id) {
    # Generate sample wells for a 96-well plate
    wells <- expand.grid(
      Letter = LETTERS[1:8],
      Number = sprintf("%02d", 1:12)
    )
    well_ids <- apply(wells, 1, paste, collapse = "")
    
    # Expand the well IDs to have multiple cells per well
    well_ids_expanded <- rep(well_ids, each = 200)
    
    # Generate unique cell IDs for each well
    cell_ids <- unlist(lapply(1:96, function(x) 1:200))
    
    # Define base conditions and additional variables
    base_conditions <- c("CTL", "IR", "Exp")
    additional_var1_types <- c("Type1", "Type2", "Type3")
    additional_var2_groups <- c("GroupA", "GroupB", "GroupC")
    
    # Create all combinations of base conditions and additional variables
    condition_combinations <- expand.grid(
      Condition = base_conditions,
      AdditionalVar1 = additional_var1_types,
      AdditionalVar2 = additional_var2_groups,
      stringsAsFactors = FALSE
    )
    
    # Duplicate combinations for background variants
    background_combinations <- transform(condition_combinations, Condition = paste(Condition, "background", sep = "_"))
    all_combinations <- rbind(condition_combinations, background_combinations)
    
    # Assign combinations to wells
    assigned_combinations <- all_combinations[rep(1:nrow(all_combinations), each = 8), ]
    
    # Sample data for continuous variables
    nuclear_area <- stats::runif(length(well_ids_expanded), min = 10, max = 100)
    dapi <- stats::runif(length(well_ids_expanded), min = 0, max = 50)
    edu <- stats::runif(length(well_ids_expanded), min = 0, max = 50)
    sabgal <- stats::runif(length(well_ids_expanded), min = 0, max = 50)
    
    # Combine into a dataframe
    data.frame(
      plate = rep(plate_id, length(well_ids_expanded)),
      well = well_ids_expanded,
      cell_ID = cell_ids,
      condition = assigned_combinations$Condition,
      Additional_Var1 = assigned_combinations$AdditionalVar1,
      Additional_Var2 = assigned_combinations$AdditionalVar2,
      Nuclear_Area = nuclear_area,
      DAPI = dapi,
      EdU = edu,
      SABGal = sabgal
    )
  }
  
  # Generate data for two plates
  plate1_data <- generate_plate_data("Plate1")
  plate2_data <- generate_plate_data("Plate2")
  
  # Combine data from both plates
  df_single_cell_data <- rbind(plate1_data, plate2_data)
  
  # Return df
  df_single_cell_data
}
