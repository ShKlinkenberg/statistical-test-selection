library(jsonlite)

# Build adjacency list from edges
build_tree <- function(node_id, edges, nodes) {
  node_info <- nodes |> dplyr::filter(id == node_id)
  label <- node_info$label[1]
  shape <- node_info$shape[1]

  # Find children edges

  child_edges <- edges |> dplyr::filter(from_id == node_id)

  children <- purrr::map(seq_len(nrow(child_edges)), function(i) {
    child <- build_tree(child_edges$to_id[i], edges, nodes)
    child$edge_label <- child_edges$edge_label[i]
    child
  })

  result <- list(
    id = node_id,
    label = label,
    shape = shape
  )

  if (length(children) > 0) {
    result$children <- children
  }

  result
}

# Find the root node (appears as from but never as to)
root_id <- setdiff(edges$from_id, edges$to_id)

tree_data <- build_tree(root_id, edges, nodes)

writeLines(
  toJSON(tree_data, auto_unbox = TRUE, pretty = TRUE),
  "test_selection_tree.json"
)