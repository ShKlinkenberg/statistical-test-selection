# Read data
data_wide_read <- read.csv2("test_selection_scheme_wide.csv")
data_wide_read <- cbind("DVn", data_wide_read)

col_names <- names(data_wide_read)

# Creat variables for mermaid syntax for flowchart decision nodes
mmd_decision_node_var <- c("DVn", "DVt", "IVn", "IVt", "IVc", "Samp", "PT", "NPT")

# Creat shortened mermaid syntax for flowchart decision nodes
mmd_decision_node_names <- c("DV#", "DV<br>type", "#IV", "IV<br>type", "#Cat", "Samp")

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

# Replace empty strings with NA
data_wide_read_vars[data_wide_read == ""] <- NA

# Write to file
file.name <- "looped_Output.mmd"
cat("flowchart LR\n\n", file = file.name)

# Crawl through the data frame to crate mermaid syntax for flowchart decision nodes
for (r in 1:n.row){
  for (c in 2:n.coll){
    if (!is.na(data_wide_read_vars[r,c])){ # Skip NA values
      if (r == 1 & c < n.coll - 1)  { cat( sprintf("%s -->|%s| ", data_wide_read_vars[r,c-1], data_wide_read_vars[r,c]) ,sep = "", file = file.name, append = TRUE) }
      # If this node is the same as the node in the previous row, skip it
      if (c > 1 & c < n.coll - 1) {
      right     <- data_wide_read_vars[r  ,c]
      right.top <- data_wide_read_vars[r-1,c]
      # If cel to the righ is NA, print the node
      # If cel to the right is text compare to cell above. If different, print the node
      if (ifelse(is.na(right), TRUE, ifelse(is.na(right.top), TRUE, right != right.top)) & r > 1){
        # not for last 2 colums and not if privious node is NA
        if (c < n.coll - 1 & !is.na(data_wide_read_vars[r,c-1]))  { cat( sprintf("%s -->|%s| ", data_wide_read_vars[r,c-1], data_wide_read_vars[r,c]) ,sep = "", file = file.name, append = TRUE) }
      }
      }
      if (c == n.coll - 1) { cat( sprintf("%s",        data_wide_read_vars[r,c]) ,sep = "", file = file.name, append = TRUE) }
      if (c == n.coll)     { cat( sprintf(" --> %s",   data_wide_read_vars[r,c]) ,sep = "", file = file.name, append = TRUE) }
      }
  }
      cat( "\n", sep = "", file = file.name, append = TRUE)
}



# cat(DVn2DVt, DVt2IVn, IVn2IVt, IVt2IVc, IVc2IVs, IVs2PT, PT2NPT,
#     "\nsubgraph Parametric Tests\n",
#     paste0(data_wide_read$PT,collapse = "\n"),
#     "\nend\n",
#     "subgraph Non Parametric Tests\n",
#     paste0(grep("NPTNA", data_wide_read$NPT, value = TRUE, invert = TRUE), collapse = "\n"), 
#     "\nend\n"
#     ,sep = "", file = file.name, append = TRUE)


# cbind(paste0(data_wide_read_vars$IV.number, "-->", ifelse(!is.na(data_wide_read_vars$IV.type), paste0("|",data_wide_read_vars$IV.type,"| "), "" )),

# ifelse(!is.na(data_wide_read_vars$IV.type), data_wide_read_vars$IV.type, ""))


# number --> conditional on




