# Investigating Data Visualization Methods using the Global Terrorism Database

## Introduction
This repository explores various data visualization techniques and plots using the Global Terrorism Database (GTD). The GTD, maintained by the University of Maryland's National Consortium for the Study of Terrorism and Responses to Terrorism (START), contains detailed information on over 200,000 terrorist attacks worldwide since 1970. By utilizing effective visualization techniques, this project aims to gain deeper insights into global terrorism phenomena and highlight patterns, trends, and distributions within the dataset. The analysis utilizes R packages such as ggplot2, ggmap, and tmap, and includes the development of a Shiny app for interactive visualizations.

## Purpose of the Report
The purpose of this report is to visually analyze the GTD dataset and present its findings using a variety of visualization techniques. The report adheres to Edward Tufte's guidelines for creating engaging visuals, aiming to provide accessible and interesting presentations of the data. Through visualizations of the frequency of terrorist attacks by region and year, weapon usage, casualties, and more, this report aims to uncover recurring trends and patterns, providing new insights into the global terrorism landscape.

## Methodology
### Overview of Visualization Techniques Used
The report employs a range of visualization techniques, including bar charts, scatter plots, heat maps, bubble charts, interactive maps, and a Shiny dashboard/app. These techniques effectively convey information from the GTD dataset in a clear and concise manner, enabling a comprehensive exploration of the data.

### Theoretical Considerations Used
The accuracy, clarity, and interpretation of data visualizations depend on theoretical considerations and choices. This project carefully considers theoretical aspects to ensure accurate and easily interpretable visualizations. The choice of visualization techniques, color and contrast usage, and incorporation of interactivity were all guided by theoretical considerations. These decisions aim to present the GTD data accurately, highlight patterns, and provide an engaging user experience.

## Data Preparation
### Characteristics of the GTD Dataset
The GTD dataset contains details on over 200,000 terrorist attacks, including bombings, assassinations, and kidnappings. It covers incidents in more than 150 countries since 1970 and offers extensive information for recent occurrences. The dataset is publicly available and can be downloaded for immediate use.

### Data Cleaning and Processing Techniques Used
To prepare the GTD dataset for analysis, data cleaning and processing techniques were applied. The tidyverse library in R was utilized to remove unnecessary columns, rename relevant columns, and ensure consistency in the analysis. Categorical variables were appropriately labeled, and the date information was extracted and converted to a date format. Spatial analysis was performed by converting the dataset to a simple features (sf) file using the sf and sp packages. Missing coordinates were filtered out, and the Coordinate Reference System (CRS) was set before conducting spatial analysis. Population and GDP datasets were cleaned, merged, and combined with country coordinates for enhanced spatial analysis. These data cleaning and processing techniques ensured the dataset's accuracy, consistency, and analysis readiness.

## Visualization and Insights
The report presents several visualizations and insights derived from the GTD dataset. By attack type and region, temporal analysis illustrates the frequency of terrorist attacks over time. Weapon analysis visualizes the most common weapon usage over time, providing insights into the prevalence of certain types of attacks. Causality analysis examines the relationship between fatalities and injuries resulting from terrorist attacks. Further visualizations explore the distribution of attacks by target type, attack type, and region. Spatial maps visualize the number of attacks, injuries, and fatalities by country, highlighting the most affected regions. These visualizations offer valuable insights into global terrorism patterns and trends.

## Answering the Questions
The report addresses several questions related to the project:
- The concept and implementation of the project were based on prior familiarity with data visualization techniques and packages, such as ggplot2. Additionally, new tools like tmap and leaflet were learned and utilized.
- While visualizations for the GTD dataset have been published, this project takes a unique approach with different visual encodings and specific variables.
- The novelty in this approach lies in the use of interactive maps, visual encodings, and the transformation of GTD into a simple features dataset.
- A complex concept/implementation in this work was the use of the sf package for spatial analysis and the implementation of interactive maps with popups and legends in the Shiny dashboard.
- The aspect of data analysis investigated in this project is primarily exploratory, focusing on patterns, trends, and distributions in the GTD dataset.

## References
The project references various works related to the GTD dataset and data visualization techniques. The provided references include sources on terrorism research, data analysis of terrorism activities, and works on data visualization by notable authors such as Edward Tufte.


Please note that the above summary is a brief overview of the project. For a more detailed analysis, insights, and reproduction instructions, please refer to the analysis files and code provided in this repository.

For any inquiries, please contact Oluyori Benjamin at oluwagbemiga.oluyori@mymtu.ie.
Link to report: https://www.oluyoridata.com/mywork/2023-03-07-INVESTIGATING-DATA-VISUALIZATION-METHODS-USING-THE-GLOBAL-TERRORISM-DATABASE%3Cbr%3E%0A%26nbsp%3B.html?post_id=1

Keywords: data visualization, Global Terrorism Database, GTD, R, ggplot2, ggmap, tmap, Shiny app, spatial analysis, temporal analysis, patterns, trends, interactive maps
