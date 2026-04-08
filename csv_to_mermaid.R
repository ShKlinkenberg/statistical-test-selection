library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Read the data
df <- read_delim(
  "test_selection_scheme_wide.csv",
  delim = ";",
  col_types = cols(.default = "c"),
  na = c("", "NA")
)

names(df) <- c("#DV",
               "DV<br>type",
               "#IV",
               "IV<br>type",
               "IV<br>cat",
               "Samp",
               "Parametric Test",
               "Non-parametric test")

# Identify decision columns (all except the last two) and result columns
col_names <- names(df)
n_cols <- length(col_names)
decision_cols <- col_names[1:(n_cols - 2)]
parametric_col <- col_names[n_cols - 1]
nonparametric_col <- col_names[n_cols]

# Helper: create a safe node id from a label
make_id <- function(label) {
  label |>
    str_replace_all("[^A-Za-z0-9]", "_") |>
    str_replace_all("_+", "_") |>
    str_remove("_$")
}

# For each row, build the full path of (column, value) pairs for non-NA decisions.
# The node ID encodes the full path context so that the same column name
# appearing in different branches becomes a unique node.

edges <- tibble(
  from_id = character(), from_label = character(), from_shape = character(),
  to_id = character(), to_label = character(), to_shape = character(),
  edge_label = character()
)

for (i in seq_len(nrow(df))) {
  row <- df[i, ]

  # Get non-NA decision columns and their values for this row
  decision_values <- map_chr(decision_cols, ~ as.character(row[[.x]]))
  names(decision_values) <- decision_cols
  non_na_idx <- which(!is.na(decision_values))

  if (length(non_na_idx) < 1) next

  path_cols <- decision_cols[non_na_idx]
  path_vals <- decision_values[non_na_idx]

  # Build context-aware node IDs:
  # The first decision node has no prefix context.
  # Each subsequent decision node's ID includes the path of edge labels leading to it.
  # This ensures that e.g. "Sampled" reached via different paths gets different IDs.

  # context_parts accumulates the edge labels taken so far
  context_parts <- character(0)

  for (j in seq_along(path_cols)) {
    # Build "from" node ID from context
    from_id <- make_id(paste(c(context_parts, path_cols[j]), collapse = "_"))
    from_label <- path_cols[j]
    edge_label <- path_vals[j]

    # Update context with this edge label for the next node
    new_context <- c(context_parts, path_cols[j], path_vals[j])

    if (j < length(path_cols)) {
      # "to" is the next decision node, with updated context
      to_id <- make_id(paste(c(new_context, path_cols[j + 1]), collapse = "_"))
      to_label <- path_cols[j + 1]
      to_shape <- "decision"
    } else {
      # "to" is the parametric test result
      param_val <- as.character(row[[parametric_col]])
      if (is.na(param_val)) next
      to_id <- make_id(param_val)
      to_label <- param_val
      to_shape <- "box"
    }

    edges <- edges |>
      add_row(
        from_id = from_id, from_label = from_label, from_shape = "decision",
        to_id = to_id, to_label = to_label, to_shape = to_shape,
        edge_label = edge_label
      )

    context_parts <- new_context
  }

  # Edge from parametric test to non-parametric test
  param_val <- as.character(row[[parametric_col]])
  nonparam_val <- as.character(row[[nonparametric_col]])
  if (!is.na(param_val) && !is.na(nonparam_val)) {
    edges <- edges |>
      add_row(
        from_id = make_id(param_val), from_label = param_val, from_shape = "box",
        to_id = make_id(nonparam_val), to_label = nonparam_val, to_shape = "box",
        edge_label = ""
      )
  }
}

# Deduplicate edges
edges <- edges |> distinct()

# Collect all unique nodes
nodes_from <- edges |> select(id = from_id, label = from_label, shape = from_shape) |> distinct()
nodes_to <- edges |> select(id = to_id, label = to_label, shape = to_shape) |> distinct()
nodes <- bind_rows(nodes_from, nodes_to) |> distinct(id, .keep_all = TRUE)

# Identify parametric and non-parametric test node IDs
parametric_node_ids <- edges |>
  filter(from_shape == "decision", to_shape == "box") |>
  pull(to_id) |>
  unique()

nonparametric_node_ids <- edges |>
  filter(from_shape == "box", to_shape == "box") |>
  pull(to_id) |>
  unique()

# Set the yaml header for mermaid configuration
mermaid_lines <- c("---","config:","  look: handDrawn", "  theme: neutral", "---\n")

# Build mermaid output
mermaid_lines <- c(mermaid_lines, "graph LR")

# Decision node definitions
decision_defs <- nodes |>
  filter(shape == "decision") |>
  mutate(def = paste0('    ', id, '{"', label, '"}')) |>
  pull(def)

mermaid_lines <- c(mermaid_lines, decision_defs)

# Parametric tests subgraph
parametric_nodes <- nodes |> filter(id %in% parametric_node_ids)
param_defs <- parametric_nodes |>
  mutate(def = paste0('        ', id, '["', label, '"]')) |>
  pull(def)

mermaid_lines <- c(
  mermaid_lines,
  '    subgraph parametric_tests["Parametric Tests"]',
  param_defs,
  '    end'
)

# Non-parametric tests subgraph
nonparam_nodes <- nodes |> filter(id %in% nonparametric_node_ids)
nonparam_defs <- nonparam_nodes |>
  mutate(def = paste0('        ', id, '["', label, '"]')) |>
  pull(def)

mermaid_lines <- c(
  mermaid_lines,
  '    subgraph nonparametric_tests["Non-Parametric Tests"]',
  nonparam_defs,
  '    end'
)

# Edge definitions — quote edge labels to handle special characters
edge_defs <- edges |>
  mutate(def = if_else(
    edge_label == "" | is.na(edge_label),
    paste0("    ", from_id, " --> ", to_id),
    paste0('    ', from_id, ' -->|"', edge_label, '"| ', to_id)
  )) |>
  pull(def)



mermaid_lines <- c(mermaid_lines, edge_defs,
                  '',
                  '    style parametric_tests fill:#cce5ff,stroke:#333',
                  '    style nonparametric_tests fill:#d4edda,stroke:#333')

# Write to file
mermaid_output <- paste(mermaid_lines, collapse = "\n")
writeLines(mermaid_output, "test_selection_flowchart.mmd")

mermaid_output