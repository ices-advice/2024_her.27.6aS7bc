## Extract results of interest, write TAF output tables

## Before: length_values.csv, length_frequency.csv, settings.Rdata (model)
## After:  length_values.csv, length_frequency.csv, settings.Rdata (output)

library(icesTAF)

mkdir("output")

## Copy DLS results to output directory
cp("model/length_values.csv", "output")
cp("model/length_frequency.csv", "output")
cp("model/settings.Rdata", "output")
