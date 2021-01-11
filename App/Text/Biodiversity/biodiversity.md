
# Biodiversity

There are several metrics for quantifying diversity. In this application a user can choose between three options, species richness, the Shannon-Wiener index, or the Gini-Simpson index. Each metric uses a different data summary statistic. Richness uses the number of species observed, Shannon-Wiener uses count data, and Gini-Simpson uses percent cover. 

## Richness

Species richness is the simplest measure of diversity and the least informative. To put it simply, it is the number of species observed. Used alone, there is not much information to be gleaned from richness. For instance, you can have 991 individuals of one species, and one of 9 additional species to get a richness value of 10. On the other hand if you have 100 individuals of 10 species each, you have a richness value of 10. In both cases there are 1,000 individuals present, and a richness value of 10. Intuitively, we think of the latter situation as being more diverse and the former as less diverse. This is because the former example is dominated by a single species. In the kelp forest ecosystem, this is the difference between a site dominated by sea urchins, and a healthy kelp forest with a wide array of species in relatively even abundance across all trophic levels. 

The number of species observed is usually limited by the target list of species. For the KFMP, we use two different measures of richness depending on how we summarize the data. The same list of invertebrates and algae is for both measures. For fish to be consistent across all years, we must use our Visual Fish Transect (VFT) data which is limited to 13 species. When looking at the MPA reference sites, we instead incorporate Roving Diver Fish Count (RDFC) data which counts all species of fish. We do this because RDFC was implemented in the 1990's and further modified in the early 2000's therefore it cannot be used to count species back to the beginning of monitoring. Since 2004, RDFC has been consistent enough to provide accurate counts for all species of fish which were observed in a given site. 2005 provides a nice cutoff for this summary due to the 16 sites which were added then.

## Shannon-Wiener Index

The Shannon-Wiener diversity index combines species richness (the number of species present in the dataset) with their relative abundance. Higher numbers in this index correspond to higher diversity. These numbers are derived from a select list of indicator species which have count data. This does not account for the true biodiversity but rather quantifies the relative diversity as all sites share the same list of indicator species. The Shannon-Wiener index is one of the most commonly used metrics of diversity in ecological datasets.

We use this index on the KFMP count data from our the 1 m² quadrats, 5 m² quadrats, band transects, and either visual fish transects or roving diver fish counts (depending on the data summary). We do not count all the individuals at a site, but rather a subset which are found in our various monitoring protocols. To get an estimated site level count, we fist calculate a mean density (#/m²) and multiply by 2,000 m². These numbers are then collected from all the different monitoring protocols and placed into a data matrix with each column representing a species, and each row an observation of every site and survey year combination. The resulting matrix is processed in R using the vegan package's diversity function with the index argument set to 'shannon'. This calculates the index value and returns a data frame with a single value for each site and survey year combination.

## Gini-Simpson Index

The Gini-Simpson diversity index...

We use this index on the KFMP percent cover data from the random point contacts (RPCs) protocol. The RPC protocol uses 600 random point to survey the benthos for algae and benthic animals. A single percent cover value is estimated for the site by counting the number of times it appears in the data, dividing by 600, and multiplying by 100 to get a percentage. These estimated percentages are then placed into a data matrix with each column representing a taxa, and each row an observation of every site and survey year combination. The resulting matrix is processed in R using the vegan package's diversity function with the index argument set to 'simpson'. This calculates the index value and returns a data frame with a single value for each site and survey year combination.