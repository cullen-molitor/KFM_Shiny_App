## Species Biomass

Biomass is a summary statistic which measures the mass of a species or taxa per a given area. In ecology, this is generally standardized to the mass (g) of a species or taxa per square meter (g/m²). Biomass can be difficult to imagine unless one is familiar with typical weight classes of various sized individuals of the species in question. This is rare, so biomass is generally a better comparative metric to see how much of one species there is relative to another year or another species. Biomass is also very the best method for tracking energy flow through a food web. This is because biomass takes into account the size distribution and the density of the population. 

To calculate biomass for KFM data, we must first find the mean densities of the species of interest. We take our raw data from the database and make it 'tidy.' Tidy data refers to tabular data which is stored as one observation per row and one variable per column. Once it is tidy, we then group by the variables 'survey year', 'species', and 'site'. With the data grouped we can calculate the mean density and store the values in a summary data frame that has a single mean density value for every combination of year, site, and species. KFM does not size all the species we count so we further filter the summary data to only those species which we measure. 

Next we must make our size frequency distribution data tidy as well. With this data clean and tidy, we join our density data to our size frequency data into a single data frame. This will include many repeated density values because many individuals are typically measured for each species every year. 

We know the length to weight conversion equations for many species which we monitor. This means that we can estimate how much an individual weighs at any given size. We calculate this mass and store those numbers as a new column in our data frame.

We then find the total number of individuals at each size class and divide by the total number of individuals measured. This divides the sample we measured into proportions which when added together equal 1. We multiply each proportions by the estimated mass of the animals at that size class. This gives a proportionally corrected mass value. We then take the sum of these corrected masses and multiply by the mean density of the animals that year. This gives a density corrected mean biomass for each species at each site for every year we monitored.

There are some cases where natural habitat size frequencies were not conducted for a species in a given year at a particular site. This leaves missing values where we have a mean density, but not a mean biomass. We solve this problem by modeling the relationship between mean density and mean biomass for every species and reserve status combination. The model produced is linear and follows the classic *y = mx + b* formula where:

+ y is the mean biomass
+ x is the mean density
+ m is the slope of the line which models the relationship
+ b is the y intercept of the line which models the relationship

Knowing this, we can plug in the mean density and get an estimated mean biomass for all missing values. This is particularly useful for our fish data as we did not begin conducting fish size frequencies until 2007. We back-fill the missing values for 2004, 2005, and 2006 with our linear regression model.

This data is collected and stored in a summary data frame. This summary data is what is shown in the 'time series' and 'map bubble' tabs.

The ratio tab include a further summary of the biomass data by creating a single ratio value for every survey year and species combination. The ratio represents the mean biomass of a species inside an SMR divided by the mean biomass outside. Because there are many sites and some are inside and some outside, we use a method called bootstrapping to find the distribution of ratios for all possible site and year combinations. Bootstrapping allows us to randomly sample our mean biomass values for the possible combinations and return a ratio. We do this random sampling 1,000 times and find the distribution of the values which. This distribution of ratio values tells us our sample mean ratio and an interval for which we are 95% confident the population mean ratio falls into.  

A ratio value close to 1 means that the biomass is similar inside and outside of a SMR. A value greater than 1 means there was more biomass inside a SMR than outside. Less than 1 means there was more outside than inside. The lines that extend out from either side of the ratio value represent the 95% confidence interval for the ratio. If this confidence interval does not overlap with the center line (a value of 1) then the ratio is significant. In other words, it is highly likely there was higher biomass inside or outside depending on the side the ratio falls on. 






