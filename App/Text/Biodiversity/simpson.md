
## Gini-Simpson Index

We use this index on the KFMP percent cover data for 21 taxa algae and benthic animals. A single percent cover value is estimated for the site by counting the number of times it appears in the data, dividing by 600, and multiplying by 100 to get a percentage. These estimated percentages are then placed into a data matrix with each column representing a taxa, and each row an observation of every site and survey year combination. The resulting matrix is processed in R using the vegan package's diversity function with the index argument set to 'simpson'. This calculates the index value and returns a data frame with a single value for each site and survey year combination.

We use this index on KFM percent cover data collected during random point contacts (RPCs). We use a single measure of the Gini-Simpson index because the list of indicator species monitored by RPCs has not changed appreciably since its inception. The few changes which have occurred have been excluded or manipulated to insure backwards compatibility across all years. 
