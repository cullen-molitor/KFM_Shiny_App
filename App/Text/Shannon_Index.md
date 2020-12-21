

## **Count Data**

There are several metrics for quantifying diversity. The Shannon-Wiener diversity index is a measure which combines species richness (the number of species present in the dataset) with their relative abundance. Higher numbers in this index correspond to higher diversity. These numbers are derived from a select list of indicator species which have count data. This does not account for the true biodiversity but rather quantifies the relative diversity as all sites share the same list of indicator species. The Shannon-Wiener index is one of the most commonly used metrics of diversity in ecological datasets. 

We use this index on the KFMP count data. We do not count all the individuals at a site, but rather a subset which are found in our various monitoring protocols. To get an estimated site level count, we fist calculate a mean density (#/m²) and multiply by 2,000 m². These numbers are then collected from all the different monitoring protocols and placed into a single data matrix with each column representing a species, and each row an observation of every site and survey year combination. The resulting matrix is processed in R using the vegan package's diversity function. This calculates the index value and returns a data frame with a single value for each site and survey year combination.
