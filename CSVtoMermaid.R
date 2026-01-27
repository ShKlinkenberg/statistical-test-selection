library(tidyverse)

# Read the CSV file
tree_data <- read.csv(file = "test selection scheme.csv", header = TRUE, sep = ";")

# Create Mermaid flowchart syntax
mermaid_code <- tree_data |>
  mutate(
    # Create the connection syntax: parent --> child
    connection = paste0("    ", parent_id, " --> ", id)
  ) |>
  pull(connection) |>
  paste(collapse = "\n")

# Define the nodes with their labels
node_definitions <- tree_data |>
  mutate(
    node_def = paste0("    ", id, '["', name, '"]')
  ) |>
  pull(node_def) |>
  paste(collapse = "\n")

# Wrap in Mermaid flowchart structure
mermaid_output <- paste0(
  "graph LR\n",
  node_definitions, "\n",
  mermaid_code
)

# Print the result
cat(mermaid_output)

# Optionally, write to a file
writeLines(mermaid_output, "flowchart.mmd")