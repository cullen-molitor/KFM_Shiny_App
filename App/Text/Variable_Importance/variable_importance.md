
## Identifying Important Species

There are many considerations about which species should be monitored when designing a long term monitoring program. There are simply too many species in the kelp forest to count with a limited budget and limited survey season. Furthermore, many species have look-alikes with very subtle differences. The KFM program choose a select list of taxa from all trophic levels, and included algae, invertebrates, and fish. This wide range of species gives us a detailed look at the kelp forest community dynamics. It also allows us to further identify species with relationships to certain islands or marine protection status. These are the "important species" this section aims to identify. It is not that these species are more important to the kelp forest ecosystem, but rather that they are important to indicating which species have the strongest relationship with which island, islands, or marine protection status. 

### Reserve Effects 

The species identified with a strong relationship to reserve status will be considered to have a strong reserve effect. These effects can indicate that a species does better inside or outside of a reserve. For instance, warty sea cumbers (*Parastichopus parvimensis*) are a heavily fished species which have higher biomass inside the protection of a marine reserve. On the hand, Kellet's whelks (*Kelletia kellettii*) are only fished incidentally to trap fisheries and so are better suited to unprotected areas where there is less competition and fewer predators.

### Island Effects

The species identified with a strong relationship to an island or islands will be considered to have a strong biogeographic effect. These effects can indicate that a species does better in colder water (Oregonian province) or warmer water (Californian province). For instance, white-spotted rose anemones prefer the cold waters of the Oregonian province and it is rare to find them outside of San Miguel and Santa Rosa Islands. 

### Site Effects

The species identified with a strong relationship to a site or several similar sites will be considered to have a strong localized effect. These effects can indicate that a species does better in certain biogeographic provinces and likely have a preference for certain oceanographic conditions. For instance, one species may prefer the cold waters of the Oregonian province coupled with strong ocean currents, while another may not be capable of surviving in those conditions. This is why one reason why California hydrocoral (*Stylaster californicus*) is present at Gull Island South but is not present at any other KFM site. 

### Random Forest Classification Modeling

Random forest is a common machine learning algorithm that uses decision trees to perform either regression analysis or classification analysis. We use classification to identify the variables that are important to the classes we are interested in (islands and reserve status). So what is a random forest really?

#### Training a Diver

Well to answer that, we will use a thought experiment involving a diver who is lost and needs to perform KFM protocols in order to guess at where they are. This diver is blindfolded, taken out to an island, and pushed off the boat. When they go underwater, they are able to take off their blindfold and begin performing KFM protocols. At the end of the dive, they are re-blindfolded and asked which island they are at and whether they are inside of a reserve or outside, based on the data they collected. They are then told told the correct answers and brought to a new site to begin all over again. 

Initially this diver is very disoriented and the guesses are bad. However, this diver has an excellent memory and begins to learn which species are at which island, and what is a normal amount for inside a reserve vs outside. After 3,000 dives, this diver seldom gets the answer wrong. At the end of this, they are asked to rank the species which helped them make correct guesses. They are then asked to note the values which indicate whether they are in a reserve or outside.

#### Training a Machine

Instead of taking our computer diving, we feed it the KFM data that has been collected since 1982. Once the computer has the data in a nice format where every row consists of summarized data from all site and survey year combinations, and each column is a variable such as the site, year, or species. We ask it to take a random row from that data and sample 8 species and guess at the same questions we asked our diver. Each 8 variable combination of a row is a decision tree. Each variable is then a branch on that tree. At every variable it makes a decision about which island or reserve status and checks to see if it is right. It can then learn from its mistakes the next time it encounters this variable and make a better guess at every tree. It then ranks these variables by which ones it was able to guess correctly most often.  



