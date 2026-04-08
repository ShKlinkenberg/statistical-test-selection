# Statistical Test Selection

This repository generates various statistical test selection schemes based on different criteria and assumptions. The source for all visualizations is the `test_selection_scheme_wide.csv` file.

The following steps are taken to generate the visualizations.

1. `test_selection_scheme_wide.csv` is the source for the decision table
2. Mermaid syntax is generated from this table by `csv_to_mermaid.R`
3. The mermaid syntax is converted to JSON by `mermaid_to_json.R`
4. `interactive_test_selection_table.qmd` uses `test_selection_scheme_wide.csv` to visualize a `DT` table.
5. `mermaid_flowchart.qmd` uses `test_selection_flowchart.mmd` to visualise a static mermaid `SVG` flowchart.
6. `observable_interactive_graph.qmd` uses `test_selection_tree.json` to create an interactive graph.
7. `table_typst.qmd` uses `test_selection_scheme_wide.csv` to create a PDF table.

The different visualizations can be seen on the pages below:

* [Interactive test selection table](https://shklinkenberg.github.io/statistical-test-selection/interactive_test_selection_table.html)
* [Mermaid flowchart for statistical test selection](https://shklinkenberg.github.io/statistical-test-selection/mermaid_flowchart.html)
* [Observable interactive graph for statistical test selection](https://shklinkenberg.github.io/statistical-test-selection/observable_interactive_graph.html)
* [PDF table for statistical test selection](https://shklinkenberg.github.io/statistical-test-selection/table_typst.html)

