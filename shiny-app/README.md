## Seattle Neighborhood Disparities — Shiny Application

This Shiny web application provides an interactive exploration of neighborhood-level social and health disparities across Seattle. The app allows users to examine demographic patterns, health outcomes, and modeled relationships using dynamic visualizations and tables.

### App Structure
The application is organized into multiple tabs:
- **Overview** – Summary of the project, data sources, and key metrics
- **Visualizations** – Interactive plots and maps describing neighborhood-level patterns
- **Analysis & Models** – Statistical modeling and summaries used to explore relationships between social factors and health outcomes
- **Data Table** – An interactive snapshot of the analysis dataset

### Key Features
- Multiple reactive widgets for filtering neighborhoods, indicators, and demographics
- Interactive visualizations describing health and social disparities
- A statistical model summarizing relationships between key variables
- An interactive data table displaying the underlying analysis data

### Data
The app uses a cleaned, analysis-ready dataset compiled from publicly available Seattle neighborhood data:
- `seattle_neighborhood_analysis.csv`

Only variables used in the app and analysis are included.

### Running the App Locally
1. Ensure R and required packages are installed
2. Set your working directory to this folder
3. Run the app:
```r
shiny::runApp()
