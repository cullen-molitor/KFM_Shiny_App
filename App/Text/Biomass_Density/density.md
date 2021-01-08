## Species Density

Density is a summary statistic which measures the number of individuals per a given area. In ecology, this is generally standardized to the number of individuals per square meter (#/m²). There is no single best units of measure, but generally people can imagine what a 1 m² box looks like and so long as they know the usual size of an individual, they can picture how many there are in a given area. This does not necessarily scale well, so often times larger units of area are reported to give a sense of larger scales such as number per acre or square miles. These metrics can be harder to imagine but do a good job at conveying gross amounts and provide a different perspective which helps inform conclusions on resource availability and species dispersion.

To calculate density for KFM data, we first take our raw data from the database and make it 'tidy.' Tidy data refers to tabular data which is stored as one observation per row and one variable per column. Once it is tidy, we then group by the variables 'survey year', 'species', and 'site'. With the data grouped we can perform the proper summary statistics such as the mean density, standard deviation, and standard error. These statistics are collected into a summary data frame. This summary data is what is shown in this section.

Mean density is an excellent metric for tracking species abundance over time. The first tab over, 'Time Series', is a classic way to track mean density data over time. These plots will show the relative abundance from year to year and will allow a user to compare sites. Be sure to check the y axis scale as the default is set to 'free' scales which only goes to the max value for that island or site. You can set the scale to 'fixed' which will allow you to compare the relative abundances across islands as well as sites. Further the default line type 'smooth' uses locally weighted scatter plot smoothing (LOESS) to show the trends over time rather than the inter-year variation. The line type can be changed to 'sharp' for the 'all sites' or 'individual site' data summary options to show the inter-year trend which in some cases can be quite dramatic.

The lines plotted here also show as either dotted or solid. Solid lines are site which are inside a State Marine Reserve (SMR) and dotted lines are outside. Some of the original sites switched from being outside a reserve to inside a reserve in 2003 and these plots reflect that change by breaking and switching line types from dotted to dashed between 2002 and 2003. In 2005, 16 new sites were added and these sites will begin in 2005 and will only have a single line type as they have only ever been inside or outside from the time we began monitoring them. 

Text about density ratios...

Map bubbles...

