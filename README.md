# LIME Diagnostics Paper (Short and Sweet Version)

Repository for short and sweet ASA data science journal paper on diagnostics for LIME

## Changes to make to limeaid

Explanation scatterplot:

- add bin indicator variable lines
- change name of function to explain_scatter
- add line legend
- extend to the density simulation case

Feature heatmap:

- remove factor variable if does not change across facets
- figure out issue with colors
- join factor facets into one variable for easier printing (with slash n between each)

Assessment metric plot:

- add variability to average R2 and average fidelity
- change legend to points (turn rank to a factor, I think)