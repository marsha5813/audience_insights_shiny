# Audience Insights Interactive Shiny App

# Libraries
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)
library(sf)
library(viridis)
library(scales)
library(htmltools)

# Load data
nyc_data <- readRDS("data/tract_data.rds")
schools_sf <- st_read("data/schools/SchoolPoints_APS_2024_08_28.shp", quiet = TRUE) %>% st_transform(crs = 4326)
libraries <- st_read("data/nyc-libraries/data/shp/nyc-libraries.shp", quiet = TRUE) %>% st_transform(crs = 4326) |> st_cast("POINT") |> unique()
#parks_sf <- st_read("data/Parks Properties_20250807.geojson", quiet = TRUE) %>% st_transform(crs = 4326)  |> st_cast("POINT") |> unique()

# Define MOMA location (Museum of Modern Art - 11 W 53rd St, Manhattan)
moma_coords <- data.frame(
  name = "Museum of Modern Art (MOMA)",
  address = "11 W 53rd St, Manhattan",
  lng = -73.9776, 
  lat = 40.7614
)
moma_sf <- st_as_sf(moma_coords, coords = c("lng", "lat"), crs = 4326)

# Define available layers
layer_variables <- list(
  households = list(
    label = "Total Households",
    variable = "total_households",
    description = "Total number of households in each area"
  ),
  density = list(
    label = "Households per Square Mile", 
    variable = "hh_per_sqmile",
    description = "Number of households per square mile in the area"
  ),
  children = list(
    label = "% Households with Children", 
    variable = "hh_with_children_under18",
    description = "Percentage of households with children under 18"
  ),
  adults_65plus = list(
    label = "% Households with Adults 65+", 
    variable = "hh_with_adults_65plus",
    description = "Percentage of households with at least one adult aged 65 or older"
  ),
  school_enrollment = list(
    label = "% School Enrollment", 
    variable = "school_enrollment_rate",
    description = "Percentage of the population enrolled in school"
  ),
  elementary_school = list(
    label = "% Elementary School Enrollment", 
    variable = "school_enrollment_rate_elementary",
    description = "Percentage of the population enrolled in elementary school"
  ),
  limited_english = list(
    label = "% Limited English Proficiency", 
    variable = "limited_english",
    description = "Percentage of the population with limited English proficiency"
  ),
  poverty = list(
    label = "% Poverty Rate", 
    variable = "percent_poverty",
    description = "Percentage of the population living below the poverty line"
  ),
  median_income = list(
    label = "Median Household Income", 
    variable = "median_hh_income",
    description = "Median household income in dollars"
  ),
  broadband = list(
    label = "% Broadband Access", 
    variable = "broadband",
    description = "Percentage of households with a broadband internet subscription"
  ),
  walkability = list(
    label = "Walkability Index", 
    variable = "walkability",
    description = "Walkability index score for the area"
  ),
  transit_access = list(
    label = "Transit Access",
    variable = "transit_dist",
    description = "Transit access score (higher = closer to transit)"),
  spanish = list(
    label = "% Spanish Speakers", 
    variable = "speak_spanish",
    description = "Percent of the population that speaks Spanish at home"
  ),
  schools = list(
    label = "Schools Count", 
    variable = "schools_count",
    description = "Number of schools within the census tract"
  ),
  parks = list(
    label = "Parks Count", 
    variable = "parks_count",
    description = "Number of parks that intersect with the census tract"
  )
)

# Function to normalize values to 0-100 scale
normalize_to_0_100 <- function(x, method = "minmax") {
  if (method == "minmax") {
    # Min-max normalization
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    if (max_val == min_val) return(rep(50, length(x)))
    return(((x - min_val) / (max_val - min_val)) * 100)
  } else if (method == "zscore") {
    # Z-score normalization then scale to 0-100
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    if (sd_val == 0) return(rep(50, length(x)))
    z_scores <- (x - mean_val) / sd_val
    # Convert z-scores to 0-100 scale (roughly -3 to +3 SD)
    return(pmax(0, pmin(100, ((z_scores + 3) / 6) * 100)))
  }
}

# Function to calculate percentile ranks
calculate_percentile_rank <- function(x) {
  # Use percent_rank function for percentile calculation
  # This returns values from 0 to 1, multiply by 100 for percentiles
  percentile_ranks <- percent_rank(x) * 100
  return(percentile_ranks)
}

# Calculate composite index from selected pre-calculated variables
calculate_composite_index <- function(data, selected_layers) {
  if (length(selected_layers) == 0) {
    return(data %>% mutate(
      composite_index = 0,
      composite_percentile = 0
    ))
  }
  
  # Get variable names for selected layers
  selected_variables <- map_chr(selected_layers, function(layer_name) {
    layer_variables[[layer_name]]$variable
  })
  
  # Check which variables actually exist in the data
  available_variables <- selected_variables[selected_variables %in% names(data)]
  missing_variables <- selected_variables[!selected_variables %in% names(data)]
  
  if (length(missing_variables) > 0) {
    print(paste("Warning: Missing variables:", paste(missing_variables, collapse = ", ")))
  }
  
  if (length(available_variables) == 0) {
    print("Warning: No selected variables found in data")
    return(data %>% mutate(
      composite_index = 0,
      composite_percentile = 0
    ))
  }
  
  if (length(available_variables) == 1) {
    # Single variable case
    composite_scores <- normalize_to_0_100(data[[available_variables[1]]])
  } else {
    # Multiple variables - normalize each and average
    normalized_vars <- map(available_variables, function(var_name) {
      normalize_to_0_100(data[[var_name]])
    })
    
    # Combine into matrix and calculate mean
    combined_matrix <- do.call(cbind, normalized_vars)
    composite_scores <- rowMeans(combined_matrix, na.rm = TRUE)
  }
  
  # Calculate percentile ranks for the composite scores
  percentile_ranks <- calculate_percentile_rank(composite_scores)
  
  return(data %>% mutate(
    composite_index = composite_scores,
    composite_percentile = percentile_ranks
  ))
}

# Create detailed tooltip content for census tracts
create_tooltip_content <- function(data) {
  map_chr(1:nrow(data), function(i) {
    row <- data[i, ]
    
    # Basic info
    content <- paste0(
      "<strong>", ifelse("borough" %in% names(row), row$borough, "NYC"), "</strong><br/>",
      "Census Tract: ", substr(row$GEOID, nchar(row$GEOID)-5, nchar(row$GEOID)), "<br/><br/>"
    )
    
    # Add available variables dynamically
    for (layer_name in names(layer_variables)) {
      var_name <- layer_variables[[layer_name]]$variable
      if (var_name %in% names(row) && !is.na(row[[var_name]])) {
        label <- layer_variables[[layer_name]]$label
        value <- row[[var_name]]
        
        # Format based on variable type
        if (grepl("pct_|percent", var_name, ignore.case = TRUE)) {
          formatted_value <- paste0(round(value, 1), "%")
        } else if (grepl("income", var_name, ignore.case = TRUE)) {
          if (var_name == "median_income_k") {
            formatted_value <- paste0("$", round(value, 1), "k")
          } else {
            formatted_value <- paste0("$", format(round(value), big.mark = ","))
          }
        } else if (var_name == "total_households") {
          formatted_value <- format(value, big.mark = ",")
        } else {
          formatted_value <- round(value, 1)
        }
        
        content <- paste0(content, label, ": ", formatted_value, "<br/>")
      }
    }
    
    # Add composite index and percentile
    if ("composite_index" %in% names(row)) {
      content <- paste0(content, "<br/><strong>Composite Index: ", round(row$composite_index, 1), "</strong>")
    }
    if ("composite_percentile" %in% names(row)) {
      content <- paste0(content, "<br/><strong>Percentile Rank: ", round(row$composite_percentile, 1), "th percentile</strong>")
    }
    
    return(content)
  })
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Audience Insights"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Data Summary", tabName = "summary", icon = icon("table")))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .layer-checkbox {
          margin: 5px 0;
          padding: 8px;
          border: 1px solid #ddd;
          border-radius: 4px;
          background-color: #f9f9f9;
        }
        .layer-checkbox:hover {
          background-color: #f0f0f0;
        }
        .composite-info {
          background-color: #e8f4f8;
          border: 1px solid #bee5eb;
          border-radius: 4px;
          padding: 10px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "map",
        fluidRow(
          box(width = 3, status = "primary", solidHeader = TRUE,
              title = "Layer Selection",
              
              h4("Demographic Layers"),
              p("Select layers to include in composite index:"),
              
              div(class = "composite-info",
                  HTML("<strong>Composite Index:</strong><br>
                       When multiple layers are selected, they are normalized to 0-100 scale 
                       and averaged to create a composite index showing areas that score 
                       high on <em>all</em> selected factors.")
              ),
              
              # Dynamic checkboxes for layers
              uiOutput("layer_checkboxes"),
              
              br(),
              h4("Display Options"),
              selectInput("color_scheme", "Color Scheme:",
                         choices = list(
                           "Plasma (Purple-Pink)" = "plasma",
                           "Viridis (Blue-Green-Yellow)" = "viridis", 
                           "Inferno (Black-Red-Yellow)" = "inferno",
                           "Magma (Black-Purple-White)" = "magma"
                         ),
                         selected = "plasma"),
              
              br(),
              h4("Neighborhood Filter"),
              numericInput("top_n_tracts", "Show Top N Neighborhoods:", 
                          value = NA, 
                          min = 1, 
                          max = 2500, 
                          step = 1),
              helpText("Leave blank to show all neighborhoods. Enter a number (e.g., 100) to display only the top scoring tracts by composite index."),
              
              br(),
              h4("Spatial Overlays"),
              checkboxInput("show_moma", "Show MOMA Location", value = TRUE),
              
              # Dynamic spatial overlay checkboxes
              uiOutput("spatial_overlay_checkboxes"),
              
              br(),
              actionButton("refresh_data", "Refresh Data", 
                          class = "btn-warning", icon = icon("refresh"))
          ),
          
          box(width = 9, status = "primary", solidHeader = TRUE,
              title = "Interactive Map",
              
              leafletOutput("main_map", height = "600px"),
              
              br(),
              div(style = "padding: 10px;",
                  htmlOutput("map_info")
              )
          )
        )
      ),
      
      tabItem(tabName = "summary",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Data Summary",
              
              h4("Selected Layers Summary"),
              verbatimTextOutput("selected_layers_info"),
              
              br(),
              h4("Borough Statistics"),
              DT::dataTableOutput("borough_table"),
              
              br(),
              h4("Spatial Overlay Data"),
              verbatimTextOutput("spatial_overlay_info")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Use pre-loaded census data
  census_data <- reactive({
    input$refresh_data  # Dependency for refresh button
    
    # Process the pre-loaded data
    processed_data <- nyc_data
    
    # Ensure composite_percentile column exists (for backward compatibility)
    if (!"composite_percentile" %in% names(processed_data)) {
      print("Adding composite_percentile column for backward compatibility...")
      processed_data <- processed_data %>%
        mutate(composite_percentile = 0)
    }
    
    # Add borough column if it doesn't exist
    if (!"borough" %in% names(processed_data)) {
      print("Adding borough column based on GEOID...")
      processed_data <- processed_data %>%
        mutate(
          borough = case_when(
            str_detect(GEOID, "^36005") ~ "Bronx",
            str_detect(GEOID, "^36047") ~ "Brooklyn", 
            str_detect(GEOID, "^36061") ~ "Manhattan",
            str_detect(GEOID, "^36081") ~ "Queens",
            str_detect(GEOID, "^36085") ~ "Staten Island",
            TRUE ~ "Unknown"
          )
        )
    }
    
    # Filter out tracts with fewer than 10 households
    if ("total_households" %in% names(processed_data)) {
      tracts_before <- nrow(processed_data)
      processed_data <- processed_data %>%
        filter(!is.na(total_households), total_households >= 10)
      tracts_after <- nrow(processed_data)
      
      if (tracts_before > tracts_after) {
        print(paste("Filtered out", tracts_before - tracts_after, "tracts with <10 households"))
      }
    }
    
    print(paste("Using", nrow(processed_data), "NYC census tracts"))
    return(processed_data)
  })
  
  # Use pre-loaded spatial overlay data
  spatial_overlays <- reactive({
    overlays <- list()
    
    # Add schools overlay using pre-loaded data
    if (exists("schools_sf") && !is.null(schools_sf)) {
      # Check column names and try to identify common school fields
      school_cols <- names(schools_sf)
      popup_fields <- c()
      
      if (any(grepl("name|NAME|school", school_cols, ignore.case = TRUE))) {
        name_col <- school_cols[grepl("name|NAME|school", school_cols, ignore.case = TRUE)][1]
        popup_fields <- c(popup_fields, name_col)
      }
      if (any(grepl("addr|street|address", school_cols, ignore.case = TRUE))) {
        addr_col <- school_cols[grepl("addr|street|address", school_cols, ignore.case = TRUE)][1]
        popup_fields <- c(popup_fields, addr_col)
      }
      if (any(grepl("type|level|grade", school_cols, ignore.case = TRUE))) {
        type_col <- school_cols[grepl("type|level|grade", school_cols, ignore.case = TRUE)][1]
        popup_fields <- c(popup_fields, type_col)
      }
      
      if (length(popup_fields) == 0) {
        popup_fields <- names(schools_sf)[1:min(3, ncol(schools_sf)-1)]  # Use first few columns as fallback
      }
      
      overlays$schools <- list(
        data = schools_sf,
        type = "points",
        name = "Schools",
        color = "#FF6B35",
        popup_fields = popup_fields,
        default_visible = FALSE
      )
      
      print(paste("Loaded schools overlay with", nrow(schools_sf), "points"))
    }

    # Add libraries overlay
    if (exists("libraries") && !is.null(libraries)) {
      # Check column names and try to identify common library fields
      library_cols <- names(libraries)
      popup_fields <- c()
      
      if (any(grepl("name|NAME", library_cols, ignore.case = TRUE))) {
        name_col <- library_cols[grepl("name|NAME", library_cols, ignore.case = TRUE)][1]
        popup_fields <- c(popup_fields, name_col)
      }
      if (any(grepl("addr|street|address", library_cols, ignore.case = TRUE))) {
        addr_col <- library_cols[grepl("addr|street|address", library_cols, ignore.case = TRUE)][1]
        popup_fields <- c(popup_fields, addr_col)
      }
      
      if (length(popup_fields) == 0) {
        popup_fields <- names(libraries)[1:min(3, ncol(libraries)-1)]  # Use first few columns as fallback
      }
      
      overlays$libraries <- list(
        data = libraries,
        type = "points",
        name = "Libraries",
        color = "#007BFF",
        popup_fields = popup_fields,
        default_visible = FALSE
      )
      
      print(paste("Loaded libraries overlay with", nrow(libraries), "points"))
    }

    # Add parks overlay
#   if (exists("parks_sf") && !is.null(parks_sf)) {
#     # Check column names and try to identify common park fields
#     park_cols <- names(parks_sf)
#     popup_fields <- c()
#     
#     if (any(grepl("name|NAME", park_cols, ignore.case = TRUE))) {
#       name_col <- park_cols[grepl("name|NAME", park_cols, ignore.case = TRUE)][1]
#       popup_fields <- c(popup_fields, name_col)
#     }
#     if (any(grepl("addr|street|address", park_cols, ignore.case = TRUE))) {
#       addr_col <- park_cols[grepl("addr|street|address", park_cols, ignore.case = TRUE)][1]
#       popup_fields <- c(popup_fields, addr_col)
#     }
#     
#     if (length(popup_fields) == 0) {
#       popup_fields <- names(parks_sf)[1:min(3, ncol(parks_sf)-1)]  # Use first few columns as fallback
#     }
#     
#     overlays$parks <- list(
#       data = parks_sf,
#       type = "points",
#       name = "Parks",
#       color = "#28A745",
#       popup_fields = popup_fields,
#       default_visible = FALSE
#     )
#     
#     print(paste("Loaded parks overlay with", nrow(parks_sf), "points"))
#   }
    
    return(overlays)
  })
  
  # Generate layer checkboxes
  output$layer_checkboxes <- renderUI({
    checkbox_list <- map(names(layer_variables), function(layer_name) {
      layer_info <- layer_variables[[layer_name]]
      div(class = "layer-checkbox",
          checkboxInput(paste0("layer_", layer_name), 
                       layer_info$label, 
                       value = if(layer_name == "children") TRUE else FALSE),
          tags$small(style = "color: #666;", layer_info$description)
      )
    })
    
    do.call(tagList, checkbox_list)
  })
  
  # Generate spatial overlay checkboxes
  output$spatial_overlay_checkboxes <- renderUI({
    overlays <- spatial_overlays()
    
    if (length(overlays) == 0) {
      return(tags$p("No additional spatial data available", style = "color: #666; font-style: italic;"))
    }
    
    checkbox_list <- map(names(overlays), function(overlay_name) {
      overlay_info <- overlays[[overlay_name]]
      div(style = "margin: 5px 0;",
          checkboxInput(paste0("overlay_", overlay_name), 
                       paste0("Show ", overlay_info$name), 
                       value = overlay_info$default_visible),
          tags$small(style = "color: #666;", 
                    paste("Point data -", nrow(overlay_info$data), "locations"))
      )
    })
    
    do.call(tagList, checkbox_list)
  })
  
  # Get selected layers
  selected_layers <- reactive({
    layer_names <- names(layer_variables)
    selected <- map_lgl(layer_names, function(name) {
      input_name <- paste0("layer_", name)
      !is.null(input[[input_name]]) && input[[input_name]]
    })
    result <- layer_names[selected]
    print(paste("Selected layers:", paste(result, collapse = ", ")))
    return(result)
  })
  
  # Calculate composite data
  map_data <- reactive({
    data <- census_data()
    req(data)  # Require data to be available
    
    tryCatch({
      selected <- selected_layers()
      if (length(selected) == 0) {
        # No layers selected, return with minimal composite
        result <- data %>% mutate(composite_index = 0)
        print("No layers selected, returning zero index")
        return(result)
      }
      
      withProgress(message = 'Calculating composite index...', {
        setProgress(0.3, detail = paste("Processing", length(selected), "layers..."))
        result <- calculate_composite_index(data, selected)
        setProgress(0.7, detail = "Finalizing calculations...")
        
        # Apply top N filter if specified
        if (!is.null(input$top_n_tracts) && !is.na(input$top_n_tracts) && input$top_n_tracts > 0) {
          # Filter to top N tracts by composite percentile
          total_tracts <- nrow(result)
          n_to_show <- min(input$top_n_tracts, total_tracts)
          
          result <- result %>%
            arrange(desc(composite_percentile)) %>%
            head(n_to_show)
          
          print(paste("Filtered to top", n_to_show, "of", total_tracts, "tracts"))
          setProgress(0.9, detail = paste("Showing top", n_to_show, "neighborhoods..."))
        } else {
          setProgress(0.9, detail = "Showing all neighborhoods...")
        }
        
        index_range <- paste(round(min(result$composite_index, na.rm = TRUE), 1), 
                           "to", round(max(result$composite_index, na.rm = TRUE), 1))
        percentile_range <- paste(round(min(result$composite_percentile, na.rm = TRUE), 1), 
                                "to", round(max(result$composite_percentile, na.rm = TRUE), 1))
        print(paste("Composite index calculated, range:", index_range))
        print(paste("Percentile ranks calculated, range:", percentile_range, "percentile"))
        setProgress(1.0, detail = "Complete!")
        return(result)
      })
    }, error = function(e) {
      print(paste("Composite calculation error:", e$message))
      showNotification(paste("Error calculating composite index:", e$message), type = "error")
      return(data %>% mutate(composite_index = 0))
    })
  })
  
  # Render main map
  output$main_map <- renderLeaflet({
    tryCatch({
      req(map_data())
      print("Starting map render...")
      
      # Transform data to WGS84 for leaflet
      data_sf <- st_transform(map_data(), crs = 4326)
      print(paste("Data transformed, CRS:", st_crs(data_sf)$input))
      
      # Create base map first
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, 
                         options = providerTileOptions(opacity = 0.99)) %>%
        setView(lng = -73.935, lat = 40.730, zoom = 10)
      
      # Check if we have valid composite percentile data (use percentile for coloring)
      max_percentile <- max(data_sf$composite_percentile, na.rm = TRUE)
      min_percentile <- min(data_sf$composite_percentile, na.rm = TRUE)
      print(paste("Percentile range:", min_percentile, "-", max_percentile))
      
      if (max_percentile > 0 && !is.infinite(max_percentile)) {
        # Create color palette based on percentile ranks
        color_func <- switch(input$color_scheme,
                            "plasma" = viridis::plasma,
                            "viridis" = viridis::viridis, 
                            "inferno" = viridis::inferno,
                            "magma" = viridis::magma,
                            viridis::plasma)
        
        pal <- colorNumeric(
          palette = color_func(256),
          domain = c(min_percentile, max_percentile),
          na.color = "transparent"
        )
        
        # Create detailed tooltips
        tooltip_content <- create_tooltip_content(data_sf)
        
        # Add polygons with color coding
        map <- map %>%
          addPolygons(
            data = data_sf,
            fillColor = ~pal(composite_percentile),
            weight = 0.5,
            opacity = 1,
            color = "white",
            dashArray = "",
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            popup = tooltip_content,
            label = ~paste0(ifelse("borough" %in% names(.), borough, "NYC"), 
                           ": ", round(composite_percentile, 1), "th percentile"),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal,
            values = data_sf$composite_percentile,
            opacity = 0.7,
            title = "Percentile Rank<br>(0-100th)",
            position = "bottomright",
            labFormat = labelFormat(
              suffix = "th",
              digits = 0
            )
          )
      } else {
        # Create detailed tooltips for gray polygons too
        tooltip_content <- create_tooltip_content(data_sf)
        
        # Add polygons without color coding (all gray)
        map <- map %>%
          addPolygons(
            data = data_sf,
            fillColor = "#cccccc",
            weight = 0.5,
            opacity = 1,
            color = "white",
            dashArray = "",
            fillOpacity = 0.7,
            popup = tooltip_content,
            label = ~paste0(ifelse("borough" %in% names(.), borough, "NYC"))
          )
      }
      
      # Add MOMA marker if selected
      if (!is.null(input$show_moma) && input$show_moma) {
        map <- map %>%
          addMarkers(
            lng = moma_coords$lng, 
            lat = moma_coords$lat,
            popup = paste0("<strong>", moma_coords$name, "</strong><br/>",
                          moma_coords$address, "<br/>",
                          "Target location for audience analysis"),
            label = "MOMA Location",
            group = "MOMA"
          )
      }
      
      # Add spatial overlays
      overlays <- spatial_overlays()
      for (overlay_name in names(overlays)) {
        input_name <- paste0("overlay_", overlay_name)
        show_overlay <- !is.null(input[[input_name]]) && input[[input_name]]
        
        if (show_overlay) {
          overlay_info <- overlays[[overlay_name]]
          overlay_data <- overlay_info$data
          
          if (overlay_info$type == "points") {
            # Create popup content dynamically based on available fields
            popup_fields <- overlay_info$popup_fields
            available_fields <- popup_fields[popup_fields %in% names(overlay_data)]
            
            if (length(available_fields) > 0) {
              # Build popup content
              popup_content <- map_chr(1:nrow(overlay_data), function(i) {
                row_data <- overlay_data[i, ]
                content_parts <- c()
                
                for (field in available_fields) {
                  value <- row_data[[field]]
                  if (!is.na(value) && value != "") {
                    content_parts <- c(content_parts, paste0("<strong>", field, ":</strong> ", value))
                  }
                }
                
                if (length(content_parts) > 0) {
                  paste(content_parts, collapse = "<br/>")
                } else {
                  paste0("<strong>", overlay_info$name, "</strong>")
                }
              })
            } else {
              popup_content <- paste0("<strong>", overlay_info$name, "</strong>")
            }
            
            # Add markers for this overlay
            map <- map %>%
              addCircleMarkers(
                data = overlay_data,
                radius = 4,
                fillColor = overlay_info$color,
                color = "white",
                weight = 1,
                opacity = 1,
                fillOpacity = 0.7,
                popup = popup_content,
                label = ~paste(overlay_info$name),
                group = overlay_info$name
              )
          }
        }
      }
      
      print("Map render completed successfully")
      return(map)
      
    }, error = function(e) {
      print(paste("Map render error:", e$message))
      showNotification(paste("Error rendering map:", e$message), type = "error")
      
      # Return basic map on error
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -73.935, lat = 40.730, zoom = 10) %>%
        addMarkers(lng = moma_coords$lng, lat = moma_coords$lat,
                   popup = "MOMA Location", label = "MOMA")
    })
  })
  
  # Map info display
  output$map_info <- renderText({
    selected <- selected_layers()
    
    # Base info about selected layers
    base_info <- if (length(selected) == 0) {
      "<em>Select one or more demographic layers to display data on the map.</em>"
    } else if (length(selected) == 1) {
      layer_info <- layer_variables[[selected[1]]]
      paste0("<strong>Displaying:</strong> ", layer_info$label, 
             "<br><em>", layer_info$description, "</em>")
    } else {
      layer_labels <- map_chr(selected, function(x) layer_variables[[x]]$label)
      paste0("<strong>Composite Index of:</strong> ", 
             paste(layer_labels, collapse = ", "),
             "<br><em>Areas shown in darker colors score high on ALL selected factors.</em>")
    }
    
    # Add filter info if applicable
    filter_info <- ""
    if (!is.null(input$top_n_tracts) && !is.na(input$top_n_tracts) && input$top_n_tracts > 0 && length(selected) > 0) {
      current_data <- map_data()
      if (!is.null(current_data)) {
        filter_info <- paste0("<br><br><strong>Filter:</strong> Showing top ", 
                             min(input$top_n_tracts, nrow(current_data)), 
                             " highest-scoring neighborhoods")
      }
    }
    
    return(paste0(base_info, filter_info))
  })
  
  # Selected layers info
  output$selected_layers_info <- renderText({
    selected <- selected_layers()
    if (length(selected) == 0) {
      return("No layers selected.")
    } else {
      layer_info <- map_chr(selected, function(x) {
        info <- layer_variables[[x]]
        paste0("- ", info$label, ": ", info$description, " (variable: ", info$variable, ")")
      })
      paste(layer_info, collapse = "\n")
    }
  })
  
  # Borough statistics table
  output$borough_table <- DT::renderDataTable({
    req(map_data())
    
    data_for_table <- map_data() %>% st_drop_geometry()
    
    if ("borough" %in% names(data_for_table)) {
      borough_stats <- data_for_table %>%
        group_by(borough) %>%
        summarise(
          tract_count = n(),
          avg_composite = round(mean(composite_index, na.rm = TRUE), 1),
          avg_percentile = round(mean(composite_percentile, na.rm = TRUE), 1),
          max_percentile = round(max(composite_percentile, na.rm = TRUE), 1),
          min_percentile = round(min(composite_percentile, na.rm = TRUE), 1),
          total_households = sum(total_households, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(avg_composite))
      
      colnames(borough_stats) <- c("Borough", "Census Tracts", "Avg Index", 
                                  "Avg Percentile", "Max Percentile", "Min Percentile", "Total HH")
    } else {
      # Create summary without borough grouping
      borough_stats <- data_for_table %>%
        summarise(
          area = "NYC Total",
          tract_count = n(),
          avg_composite = round(mean(composite_index, na.rm = TRUE), 1),
          avg_percentile = round(mean(composite_percentile, na.rm = TRUE), 1),
          max_percentile = round(max(composite_percentile, na.rm = TRUE), 1),
          min_percentile = round(min(composite_percentile, na.rm = TRUE), 1),
          total_households = sum(total_households, na.rm = TRUE)
        )
      
      colnames(borough_stats) <- c("Area", "Census Tracts", "Avg Index", 
                                  "Avg Percentile", "Max Percentile", "Min Percentile", "Total HH")
    }
    
    DT::datatable(borough_stats, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Top areas table
  output$top_areas_table <- DT::renderDataTable({
    req(map_data())
    
    data_for_top <- map_data() %>% st_drop_geometry()
    
    # Select columns that exist
    base_cols <- c("GEOID", "composite_index")
    available_cols <- base_cols[base_cols %in% names(data_for_top)]
    
    if ("borough" %in% names(data_for_top)) {
      available_cols <- c(available_cols, "borough")
    }
    if ("total_households" %in% names(data_for_top)) {
      available_cols <- c(available_cols, "total_households")
    }
    
    top_areas <- data_for_top %>%
      arrange(desc(composite_percentile)) %>%
      head(20) %>%
      select(all_of(available_cols)) %>%
      mutate(
        tract_id = str_sub(GEOID, -6, -1),
        composite_index = round(composite_index, 1),
        composite_percentile = round(composite_percentile, 1)
      )
    
    # Build final table based on available columns - prioritize percentile display
    if ("borough" %in% names(top_areas) && "total_households" %in% names(top_areas)) {
      final_table <- top_areas %>% select(borough, tract_id, composite_percentile, composite_index, total_households)
      colnames(final_table) <- c("Borough", "Tract ID", "Percentile", "Index", "Households")
    } else if ("borough" %in% names(top_areas)) {
      final_table <- top_areas %>% select(borough, tract_id, composite_percentile, composite_index)
      colnames(final_table) <- c("Borough", "Tract ID", "Percentile", "Index")
    } else if ("total_households" %in% names(top_areas)) {
      final_table <- top_areas %>% select(tract_id, composite_percentile, composite_index, total_households)
      colnames(final_table) <- c("Tract ID", "Percentile", "Index", "Households")
    } else {
      final_table <- top_areas %>% select(tract_id, composite_percentile, composite_index)
      colnames(final_table) <- c("Tract ID", "Percentile", "Index")
    }
    
    DT::datatable(final_table,
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Spatial overlay summary
  output$spatial_overlay_info <- renderText({
    overlays <- spatial_overlays()
    
    if (length(overlays) == 0) {
      return("No spatial overlay data loaded.")
    }
    
    overlay_info <- map_chr(names(overlays), function(name) {
      info <- overlays[[name]]
      fields_text <- if (length(info$popup_fields) > 0) {
        paste("Fields:", paste(info$popup_fields, collapse = ", "))
      } else {
        "No detailed fields available"
      }
      paste0("- ", info$name, ": ", nrow(info$data), " points (", fields_text, ")")
    })
    
    paste(c("Available spatial overlay datasets:", overlay_info), collapse = "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)