# Read data
data_wide_read <- read.csv2("test_selection_scheme_wide.csv")
data_wide_read <- cbind(DVn = "DVn", data_wide_read)

col_names <- names(data_wide_read)

# Creat variables for mermaid syntax for flowchart decision nodes
mmd_decision_node_var <- c("DVn", "DVt", "IVn", "IVt", "IVc", "Samp", "PT", "NPT")

# Creat shortened mermaid syntax for flowchart decision nodes
mmd_decision_node_names <- c("#DV", "DV<br>type", "#IV", "IV<br>type", "#Cat", "Samp", "PT", "NPT")

# Get number of columns and rows in the data frame
n.coll <- ncol(data_wide_read)
n.row  <- nrow(data_wide_read)

data_wide_read_vars  <- data_wide_read
data_wide_read_names <- data_wide_read

# Make unique values in each column factors with labels A, B, C, etc. and concatenate them with the previous column to create unique identifiers for each row
for (c in 1:n.coll){
  data_wide_read_vars[,c] <- factor( data_wide_read_vars[,c], 
                                     labels = LETTERS[1:length(unique(data_wide_read_vars[,c]))] )
  if (c >= 2){
    data_wide_read_vars[,c] <- paste0(data_wide_read_vars[,c-1], data_wide_read_vars[,c])
  }
}

data_wide_read_vars$DVn <- as.character(data_wide_read_vars$DVn)

# Replace empty strings with NA
data_wide_read_vars[data_wide_read == ""] <- NA

# Write to file
# file.name <- "looped_Output_Paired.mmd"
# cat("flowchart LR\n\n", file = file.name)

results <- vector()

# Crawl through the data frame to crate mermaid syntax for flowchart decision nodes
for (c in 1:(n.coll - 2)){
    pairs <- data.frame(matrix(NA, nrow = n.row, ncol = 2))
    for (r in 1:n.row){
    first  <- data_wide_read_vars[r,c]
    second <- data_wide_read_vars[r,c+1]
    # print( is.na(cbind(first, second)) )
    # Check if first is False and second is True, if so, find the next row where first is True and second is False
      if (!is.na(first) & is.na(second)) {
        # Search for next value that is not NA
        next.col <- c + 1
        while (is.na(data_wide_read_vars[r, next.col])) { next.col = next.col + 1;  second <- data_wide_read_vars[r, next.col] }
        pairs[r,] <- c(first, second)

      }
      
        
    # Check if both are not NA
      if (!is.na(first) & !is.na(second)) {
        # Search for pairs that are not NA

        pairs[r,] <- c(first, second)
      }
      
        pairs <- pairs[complete.cases(pairs), ]
        # Only further process if there are pairs found
        if(nrow(pairs) != 0) {
          new_value <- unique( sprintf('  %s{"%s"} -->|"%s"| %s\n', 
                                       pairs[,1],
                                       mmd_decision_node_names[c], 
                                      #  data_wide_read_names[r,c],
                                       pairs[,2], 
                                       pairs[,2] ) )
        
          results <- append(results, new_value)
        }

    }
        
} # Stop looping through columns

results <- unique(results)

cat("flowchart LR\n",
    results,
    sep = "",
    file = "outputPaired.mmd",
    append = FALSE)

