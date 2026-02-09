# Read data
data_wide_read <- read.csv2("test_selection_scheme_wide.csv")
data_wide_read <- cbind(DVn = "DVn", data_wide_read)

col_names <- names(data_wide_read)

# Creat variables for mermaid syntax for flowchart decision nodes
mmd_decision_node_var <- c("DVn", "DVt", "IVn", "IVt", "IVc", "Samp", "PT", "NPT")

# Creat shortened mermaid syntax for flowchart decision nodes
mmd_decision_node_names <- c("","#DV", "DV<br>type", "#IV", "IV<br>type", "#Cat", "Samp", "PT", "NPT")

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

# PT --> NPT
paste0(data_wide_read_vars[, n.coll-1], '["', data_wide_read_names[, n.coll-1],'"]', 
       ifelse(!is.na(data_wide_read_vars[, n.coll]), sprintf(' --> %s["%s"]', data_wide_read_vars[, n.coll], data_wide_read_names[, n.coll]), ""),
       "\n") -> pt2npt

results <- append(results, pt2npt)

# X --> PT
pt <- n.coll - 1

# Get column numbers of the first previwous column with non-NA value
second <- data_wide_read_vars[, pt]
coll.numbers <- vector()
coll.vars    <- vector()
coll.names   <- vector()
before.prev.var <- vector()
before.prev.col <- vector()
for (r in 1:n.row) {
prev.col <- pt - 1 # Start with the column before PT
# Find previous
while (is.na(data_wide_read_vars[r, prev.col])) { prev.col = prev.col - 1;  }

# print(c(r, prev.col))
coll.numbers[r] <- prev.col;
coll.vars[r] <- data_wide_read_vars[r, prev.col]   # First col variables
coll.names[r] <- data_wide_read_names[r, prev.col] # First col names

prev.prev.col <- prev.col - 1 # Start with the column before the previous
# Find one before previous
while (is.na(data_wide_read_vars[r, prev.prev.col ])) { prev.prev.col = prev.prev.col - 1;  }
before.prev.var[r] <- data_wide_read_vars[r, prev.prev.col]
before.prev.col[r] <- prev.prev.col
}

prev2pt <- paste0(before.prev.var, '{"',mmd_decision_node_names[coll.numbers],'"} -->|"', coll.names, '"| ', data_wide_read_vars[, 8], "\n")
prev2pt <- unique(prev2pt)

results <- append(results, prev2pt)

# # X - 1 # -------------------------------------------------- #



xmin1 <- paste0(coll.vars, '{"',mmd_decision_node_names[coll.numbers],'"} -->|"', coll.names, '"| ', before.prev.var, "\n")
xmin1 <- unique(xmin1)

# results <- append(results, xmin1)

# Crawl through the data frame to crate mermaid syntax for flowchart decision nodes
# for (c in 1:(n.coll - 1)){
#     # Set data frame to store pairs of values for the current column and the next column
#     # pairs <- cbind(character(), character())
#     for (r in 1:n.row){
#     first  <- data_wide_read_vars[r,c]
#     second <- data_wide_read_vars[r,c+1]
#     # print( is.na(cbind(first, second)) )
#     # Check if first is False and second is True, if so, find the next row where first is True and second is False
#       if (!is.na(first) & is.na(second)) {
#         # Search for next value that is not NA
#         next.col <- c + 1
#         while (is.na(data_wide_read_vars[r, next.col])) { next.col = next.col + 1;  second <- data_wide_read_vars[r, next.col] }
        
#         pairs <- c(first, second)
#         label.col <- next.col
#       }      
        
#       # Check if both are not NA
#         if (!is.na(first) & !is.na(second)) {
#           # Search for pairs that are not NA

#           pairs <- c(first, second)
#           label.col <- c
#         }
      
#       # pairs <- unique(pairs) # Remove duplicate rows from the pairs data frame
      

#         findLabel <- function(value) {
#           find <- data_wide_read_names[data_wide_read_vars == value]
#           return(find[!is.na(find)])
#         }

#       if (c < (n.coll-1)) { 
#           new_value <- sprintf('  %s{"%s"} -->|"%s"| %s\n', 
#                                       pairs[1],
#                                       mmd_decision_node_names[c], 
#                                       findLabel(pairs[2]),
#                                       pairs[2] )
#       } 
#       # if (c == (n.coll-3)) { 
#       #     new_value <- sprintf('  %s{"%s"} --> %s\n', 
#       #                                 pairs[1],
#       #                                 mmd_decision_node_names[c], 
#       #                                 pairs[2] )
#       # } 
#       # if (c == n.coll-1)  {
#       #     new_value <- sprintf('  %s["%s"] --> %s["%s"]\n', 
#       #                                 pairs[1],
#       #                                 findLabel(pairs[1]), 
#       #                                 pairs[2],
#       #                                 findLabel(pairs[2]) )
#       # }
      
      
#       results <- append(results, new_value)
      

#     } # stop looping through rows
#   results <- unique(results)
# } # Stop looping through columns

# results <- unique(results)

cat("flowchart LR\n",
    results,
    sep = "",
    file = "outputBackwardPaired.mmd",
    append = FALSE)

# find replace unique ids with original labels
# find <- data_wide_read_names[data_wide_read_vars == "AABBCBC"]
# find <- find[!is.na(find)]
