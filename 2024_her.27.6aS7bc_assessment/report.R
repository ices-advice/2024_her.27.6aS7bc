## Prepare plots and tables for report

## Before: length_values.csv, length_frequency.csv, settings.Rdata (output)
## After:  summary.csv, length_frequency_plots.png (report)

library(icesTAF)
library(rmarkdown)

mkdir("report")

sourceTAF("report_plotstables.R")
render("report.Rmd")
