# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a professionally styled R Shiny application for audience insights analysis focused on NYC demographic data. The app provides an interactive dashboard with custom branding, sophisticated UI design, and comprehensive data visualization for analyzing demographic patterns and potential audience characteristics based on census tract data.

## How to Run the Application

```r
# Install required packages if not already installed
install.packages(c("shiny", "shinydashboard", "DT", "leaflet", "tidyverse", "sf", "viridis", "scales", "htmltools"))

# Run the Shiny app
shiny::runApp("app.R")
```

## Core Architecture

### Single File Structure
The entire application is contained in `app.R` with the following main sections:

1. **Data Loading** (lines 15-28): Loads pre-processed NYC census tract data, school locations, libraries, and defines MOMA as the target location
2. **Layer Configuration** (lines 30-105): Defines available demographic variables and their metadata in `layer_variables` list
3. **Data Processing Functions** (lines 107-234): Core calculation and visualization helper functions
4. **UI Definition** (lines 240-558): Custom-styled Shiny dashboard UI with professional header
5. **Server Logic** (lines 560+): Reactive server functions for data processing, navigation, and map rendering

### Custom UI Design - MVP Version
The application currently uses a **traditional navigation approach** for MVP stability:
- **Custom Header**: Branded header with logo and professional styling (navigation icons removed)
- **Traditional Sidebar**: Standard `sidebarMenu` navigation (reliable and proven)
- **Simplified Design**: Navigation icons removed to prevent hanging issues

### Key Data Files
- `data/tract_data.rds`: Primary NYC census tract demographic data
- `data/schools/SchoolPoints_APS_2024_08_28.shp`: School location shapefile
- `data/nyc-libraries/data/shp/nyc-libraries.shp`: NYC library locations shapefile
- `images/rglogo.svg`: Reground Design company logo

### Core Functions

**`layer_variables`** (line 30): Configuration object defining all available demographic layers with labels, variable names, and descriptions.

**`calculate_composite_index(data, selected_layers)`** (line 135): Main analytical function that normalizes selected demographic variables to 0-100 scale and creates composite indices and percentile rankings.

**`create_tooltip_content(data)`** (line 188): Generates dynamic HTML tooltips for map polygons based on available data fields.

## UI Design & Branding

### Professional Header
- **Custom Gradient**: `#28202c` to `#342a3a` background
- **Logo Integration**: Reground Design logo with brightness enhancement
- **Typography**: Professional title "Audience Insight Tool" with subtitle
- **Clean Layout**: Streamlined design focused on branding (navigation icons removed for MVP)

### Color Scheme
- **Primary Brand Color**: `#28202c` (dark purple)
- **Accent Gradient**: `#342a3a` for depth
- **Panel Headers**: Custom `#28202c` backgrounds
- **Active Navigation**: Red (`#e74c3c`) for selected state
- **Inactive Navigation**: Translucent white for unselected state

### Special Features
- **Top N Solid Color**: When using the "Top N Neighborhoods" filter, selected tracts display in solid `#28202c` instead of gradient colors
- **Responsive Design**: Fixed header with proper z-index and full-width layout
- **Professional Styling**: Box shadows, rounded corners, smooth transitions

## Key Components

### Navigation System (MVP)
The app uses a **traditional navigation approach** for MVP stability:
1. **Standard Sidebar**: Reliable `sidebarMenu` with `menuItem` components
2. **Custom Header**: Professional branding without navigation functionality
3. **Simplified Architecture**: No custom navigation logic to avoid reactive loops
4. **Future Enhancement**: Header navigation icons to be implemented post-MVP

### Demographic Layers
The app supports 14 demographic variables including:
- Household counts and density
- Age demographics (children, seniors)
- Education (school enrollment)
- Economics (income, poverty)
- Language and accessibility metrics
- Infrastructure (broadband, walkability, transit)

### Interactive Features
- **Layer Selection**: Users can select multiple demographic layers to create composite indices
- **Color Schemes**: Four viridis color palettes for map visualization
- **Top N Filter**: Display only the highest-scoring neighborhoods (with solid color styling)
- **Spatial Overlays**: Toggle schools, libraries, and MOMA location on the map
- **Traditional Navigation**: Standard sidebar menu for reliable MVP operation

### Map Implementation
Uses Leaflet for interactive mapping with:
- Census tract polygons colored by percentile rankings OR solid `#28202c` for Top N filter
- Dynamic tooltips with formatted demographic data
- Spatial point overlays for schools/libraries
- Custom popup content generation
- Conditional styling based on filter state

## Navigation Architecture (MVP)

### Traditional Sidebar Navigation
```r
dashboardSidebar(
  sidebarMenu(
    menuItem("Interactive Map", tabName = "map", icon = icon("map")),
    menuItem("Data Summary", tabName = "summary", icon = icon("table"))
  )
)
```

### Custom Header (Branding Only)
```r
dashboardHeader(
  title = tags$div(
    # Custom branded header with logo and titles
    # No navigation functionality for MVP stability
  )
)
```

## Data Flow

1. Pre-loaded data is processed and filtered (minimum 10 households per tract)
2. User selects demographic layers via checkboxes
3. `calculate_composite_index()` normalizes and combines selected variables
4. Map renders with color-coded percentile rankings OR solid color for Top N
5. Interactive tooltips display formatted demographic details
6. Traditional sidebar navigation provides reliable tab switching

## Dependencies

Required R packages are loaded at the top of `app.R`:
- `shiny`, `shinydashboard`: Core application framework with custom styling
- `DT`: Data table rendering for summary statistics
- `leaflet`: Interactive mapping with conditional styling
- `tidyverse`: Data manipulation (dplyr, purrr, etc.)
- `sf`: Spatial data handling and transformations
- `viridis`, `scales`: Color schemes and data formatting
- `htmltools`: HTML generation for custom header and tooltips

## Important Notes for Future Development

### Current MVP Architecture
- **Traditional Navigation**: Uses standard `sidebarMenu` for stability and reliability
- **Custom Header**: Professional branding without navigation functionality
- **No Complex Logic**: Avoids reactive loops that can cause app hanging

### Future Enhancements (Post-MVP)
- **Header Navigation Icons**: Add map/table icons to custom header
- **Bridge Navigation**: Connect header icons to sidebar via `updateTabItems()`
- **Visual Feedback**: Update button styling to reflect active tab state
- **Testing Required**: Ensure navigation doesn't introduce hanging issues

### Styling Consistency
- **Brand Color**: Always use `#28202c` for primary elements
- **Top N Feature**: Ensure solid color (`#28202c`) is applied when filter is active
- **Header Layout**: Maintain the fixed position with proper z-index (1031)
- **Logo**: Use `images/rglogo.svg` with brightness filter enhancement