Impact of Countries’ Wealth on Social Outcomes: How GDP per Capita Relates to Murder Rates, Life Expectancy, Population Size, & Fertility (2020)
================
by Team MBK

## Summary

Summary:
While economic disparities between nations are oftentimes noticeable, the specific relationship between national wealth and quality of life is quite nuanced. Our project “Impact of countries’ wealth on social outcomes” investigates how a country's economic success, designated by GDP per capita, correlates with social issues and living conditions, specifically murder rates, life expectancy, population size, and fertility rates. Using 2020 data from the World Bank's Gapminder dataset, we examined these relationships in all measured nations worldwide in order to understand if wealth singularly decides social outcomes, or if other factors play significant roles as well. Our research stemmed from first observing the extreme contrast between the wealthiest and poorest nations in the world: Monaco, with a GDP per capita of $207,844.70, and South Sudan, with a GDP per capita of just $386.68. These nations exhibit extreme differences in life expectancy (80.06 years in Monaco compared to 63.25 years in South Sudan), fertility rates (2.38 in Monaco versus 4.16 children per woman in South Sudan), and murder rates (0.13 and 1718.31 per 100,000 people). We immediately were inclined to pursue the connection between GDP per capita and these statistics and understand if these correlations were representative of the rest of the world. 

Methods:
We used the Gapminder dataset provided by the World Bank, which compiles an all-encompassing array of social and economic statistics for countries worldwide. We focused on data from the year 2020 because it was the most recent year with complete data available. In the dataset, GDP per capita was measured in US dollars, and population, life expectancy at birth, fertility rates (number births per woman during her lifetime), and intentional homicide rates were measured for every 100,000 individuals. Next, we used scatterplots to convey any relationships that were present between GDP per capita and each variable. Because there was such a wide range of GDP values and we wanted our graphs to be as convenient to interpret as possible, we applied log scaling to GDP per capita on the X axis. Additionally, countries were grouped by color into four wealth categories based on their corresponding GDP per capita: low GDP as orange, lower-middle GDP as orange-yellow, upper-middle GDP as light blue, and High GDP as dark blue. This helped us to identify patterns across significant economic tiers while also looking at broad trends within the relationships between GDP and the measured social statistics.

Analysis:
Our analysis revealed nuanced relationships between the GDP of countries and quality of life. Our visualizations of the relationship between GDP per capita and murder rates and GDP per Capita and population size clearly reflected a lack of any noticeable correlations. Some low-GDP countries experienced extremely high homicide rates while others exhibited some of the lowest. Similarly, population is related in no discernible way to economy size. These findings suggest that there are ulterior factors different from economic wealth that fully contribute to social outcomes. These could include the type of government, effectiveness of police, political stability, among others. However, GDP per capita exhibited a direct relationship with both life expectancy and fertility rates. As national wealth increases, life expectancy rises in all income categories. Wealthier nations had clusters of representation around 75-85 years of life expectancy, while poorer countries typically range between 50-70 years. This can lead us to assume that heightened economic success allows for better healthcare and eating habits.In the visualization of fertility rates and GDP, an inverse relationship was easily visible. As countries become wealthier, birthing rates declined markedly. The highest GDP nations commonly had rates between 1.5-2.5 children per woman, while the countries with the lowest GDPs often had more than 4-6 children per woman. This pattern reflects the demographic transition model. Wealthier countries provide heightened access to birth control, education, and access to the workforce for women. As countries transition out of the first stages of the model they gain access to these assets, and this was clearly seen in our visualization.

Conclusion:
Our findings emphasize that while development of the economy influences life expectancy and rates of babies born, it does not have much influence on every aspect of social wellbeing. Violence and population size depend heavily on other factors. Throughout the process, the biggest limitation we faced was merging the datasets effectively. We overcame this by using full_join commands to assign each variable to each individual country. We could extend our research by incorporating more social outcomes to determine the effect of GDP per capita at a broader scale. It also could be interesting to use different measures of wealth outcomes to determine that GDP per capita isn’t the only measure driving our results. 

## Handout

Our handout can be found [here](handout/final_handout.pdf). 

## Memo

A link to the code and how we created our graphics in our memo can be found [here](memo/memo.md).

## Data

Gapminder. (n.d.). Data [Dataset portal]. Gapminder. Retrieved December 5, 2025, from https://www.gapminder.org/data/

## References

Gapminder. (n.d.). Data [Dataset portal]. Gapminder. Retrieved December 5, 2025, from https://www.gapminder.org/data/

Taub, A. (2025, July 24). Why One of the Causes of Falling Birthrates May Be Prosperity — Economic growth has the unintended side effects of making parenthood more difficult and expensive. The New York Times. Retrieved December 5, 2025, from https://www.nytimes.com/2025/07/24/world/falling-birthrates-economic-growth-pronatalism.html
