# Create mermaid and JSON sources from CSV file defining the decision tree structure
source("csv_to_mermaid.R")
source("mermaid_to_json.R")

# Update all the .qmd files that use the sources
# run separate lines for compiling quarto

# quarto::quarto_render("interactive_test_selection_table.qmd")
# quarto::quarto_render("mermaid_flowchart.qmd")
# quarto::quarto_render("observable_interactive_graph.qmd")
# quarto::quarto_render("table_typst.qmd")