# Read data
data_wide_read <- read_csv2("test_selection_scheme_wide.csv")

# Define decision node variables and names
mmd_decision_node_var <- c("DVn", "DVt", "IVn", "IVt", "IVc", "Samp", "PT", "NPT")
decision_cols <- c("DV number", "DV type", "IV number", "IV type", "IV categories", "Sampled")
decision_labels <- c("{DV<br>type}", "{#IV}", "{IV<br>type}", "{#Cat}", "{Samp}")

# Function to clean strings for mermaid node IDs
clean_str <- function(x) gsub(" |\\(|\\)", "", x)

# Generate mermaid flowchart edges
edges <- list()

for (i in 1:nrow(data_wide_read)) {
  row <- data_wide_read[i, ]
  
  # Start from root
  current_node <- "DVn"
  current_label <- "{#DV}"
  
  # Build the cumulative node path
  node_path <- "DVn"
  last_col_value <- NA  # Track last non-NA value for edge label to PT
  
  # Iterate through decision columns
  for (j in 1:length(decision_cols)) {
    col_value <- row[[decision_cols[j]]]
    
    # Skip if NA - don't create edge, but accumulate path
    if (is.na(col_value) || col_value == "NA") {
      node_path <- paste0(node_path, "NA")
      next
    }
    
    # Store this as the last valid value
    last_col_value <- col_value
    
    # Build next node ID using accumulated path + current value
    next_node <- paste0(node_path, clean_str(col_value))
    
    # Create edge from last valid node to this node
    edge <- sprintf("  %s%s -->|%s| %s%s\n", 
                    current_node, current_label, col_value, 
                    next_node, decision_labels[j])
    edges[[length(edges) + 1]] <- edge
    
    # Update for next iteration
    current_node <- next_node
    current_label <- ""
    node_path <- next_node
  }
  
  # Connect last valid node to parametric test
  pt_value <- row[["Parametric Test"]]
  if (!is.na(pt_value) && pt_value != "NA") {
    pt_node <- paste0("PT", clean_str(pt_value))
    
    # Use last_col_value as the edge label
    edge_pt <- sprintf('  %s -->|%s| %s["%s"]\n', 
                       current_node, last_col_value, pt_node, pt_value)
    edges[[length(edges) + 1]] <- edge_pt
    
    # Add non-parametric test if not NA
    npt_value <- row[["Non-parametric test"]]
    if (!is.na(npt_value) && npt_value != "NA") {
      npt_node <- paste0("NPT", clean_str(npt_value))
      edge_npt <- sprintf('  %s --> %s["%s"]\n', pt_node, npt_node, npt_value)
      edges[[length(edges) + 1]] <- edge_npt
    }
  }
}

# Get unique edges
unique_edges <- unique(unlist(edges))

# Write to file
cat("flowchart LR\n", file = "output2.mmd")
cat(unique_edges, sep = "", file = "output2.mmd", append = TRUE)