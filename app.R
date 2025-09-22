library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(leaflet)
library(raster)
library(rgdal)
library(RavenR)
library(geojsonio)
library(stringdist)
library(rmapshaper)
library(shinyjs)

options(shiny.sanitize.errors = FALSE)

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Clean Bootstrap theme
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .header-img {
        height: 80px;
        margin-bottom: 10px;
      }
      .section-header {
        font-weight: 700;
        font-size: 18px;
        margin-top: 20px;
        margin-bottom: 10px;
        border-bottom: 2px solid #337ab7;
        padding-bottom: 5px;
      }
      .help-text {
        font-size: 13px;
        color: #555;
        margin-bottom: 15px;
      }
      .btn-primary {
        background-color: #337ab7;
        border-color: #2e6da4;
      }
    "))
  ),
  
  titlePanel(
    fluidRow(
      column(8, "GeoJSON Converter for Raven Compliant Watershed Files"),
      column(4, img(src = "ravenbanner180.png", class = "header-img", alt = "Raven Banner"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class = "section-header", "Upload Files"),
      
      fileInput("rvhfile", "Choose RVH File (.rvh)", 
                multiple = FALSE, accept = c(".rvh")),
      
      fileInput("shpzip", "Choose ZIP with ESRI Shapefile", 
                multiple = FALSE, accept = c(".zip")),
      
      fileInput("outletCoords", "Choose Outlet Coordinates CSV", 
                multiple = FALSE, accept = c(".csv")),
      tags$div(class = "help-text",
               "CSV must have NO HEADER and three columns: Subbasin ID, Longitude, Latitude."),
      tags$a(href = "https://github.com/rarabzad/shp2geojson/raw/main/test%20cases.zip", "Download example files", target = "_blank"),
      
      tags$div(class = "section-header", "Matching Attributes"),
      
      selectInput("rvhcol", "Linking Attribute in RVH File:", choices = NULL),
      selectInput("shpcol", "Linking Attribute in Shapefile:", choices = NULL),
      
      tags$div(class = "section-header", "Settings"),
      radioButtons("simplifyGeometry", "Simplify GeoJSON?", choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE),
      textInput("CRSshp", "CRS for Shapefile (Proj4 format, if missing)", value = ""),
      tags$a(href = "https://spatialreference.org/", "Find CRS info here", target = "_blank"),
      
      actionButton("plot", "Plot Map", icon = icon("map")),
      br(), br(),
      actionButton("convert", "Convert to GeoJSON", icon = icon("cogs")),
      br(), br(),
      textOutput("conversionmessage"),
      br(),
      downloadButton("downloadData", "Download GeoJSON", icon = icon("download"))
    ),
    
    mainPanel(
      tags$div(class = "section-header", "RVH File Preview"),
      DTOutput("rvhtable"),
      tags$div(class = "section-header", "Shapefile Preview"),
      DTOutput("shptable"),
      tags$div(class = "section-header", "Map Preview"),
      leafletOutput("map", height = "600px")
    )
  )
)


server <- function(input, output, session) {
  # Reactive values to hold data
  rvh_data <- reactiveVal(NULL)
  shp_data <- reactiveVal(NULL)
  outlet_coords <- reactiveVal(NULL)
  outputfile <- reactiveVal(NULL)
  
  # Reactive value to store unzipped shapefile directory
  unzipped_dir <- reactiveVal(NULL)
  
  # Read rvh file and update linking attribute choices
  observeEvent(input$rvhfile, {
    req(input$rvhfile)
    rvhpath <- input$rvhfile$datapath
    rvh_read <- tryCatch({
      rvn_rvh_read(rvhpath)
    }, error = function(e) NULL)
    if (is.null(rvh_read)) {
      showNotification("Failed to read RVH file", type = "error")
      return()
    }
    rvh_data(rvh_read)
    cols <- colnames(rvh_read$SBtable)
    updateSelectInput(session, "rvhcol", choices = cols, selected = tail(cols, 1))
    output$rvhtable <- renderDT({
      datatable(head(rvh_read$SBtable, 10), options = list(scrollX = TRUE))
    })
  })
  
  # Read shapefile zip and update linking attribute choices
  observeEvent(input$shpzip, {
    req(input$shpzip)
    shpzip <- input$shpzip$datapath
    exdir <- file.path(dirname(shpzip), "shp_unzipped")
    unlink(exdir, recursive = TRUE, force = TRUE)
    dir.create(exdir)
    unzip(zipfile = shpzip, exdir = exdir)
    
    # Save unzipped directory path
    unzipped_dir(exdir)
    
    shpfile_path <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE)[1]
    if (is.null(shpfile_path) || length(shpfile_path) == 0) {
      showNotification("No .shp file found in ZIP", type = "error")
      return()
    }
    basins <- tryCatch({
      shapefile(shpfile_path)
    }, error = function(e) NULL)
    if (is.null(basins)) {
      showNotification("Failed to read shapefile", type = "error")
      return()
    }
    shp_crs <- crs(basins)
    if (is.na(shp_crs@projargs) || shp_crs@projargs == "") {
      # Compute extent
      e <- extent(basins)
      lon_range <- e@xmax - e@xmin
      lat_range <- e@ymax - e@ymin
      if (e@xmin >= -180 && e@xmax <= 180 && e@ymin >= -90 && e@ymax <= 90) {
        suggested_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
      } else {
        centroid_x <- (e@xmin + e@xmax) / 2
        utm_zone <- floor((centroid_x + 180)/6) + 1
        suggested_crs <- sprintf("+proj=utm +zone=%s +datum=NAD83 +units=m +no_defs", utm_zone)
      }
      updateTextInput(session, "CRSshp", value = suggested_crs)
    }
    shp_data(basins)
    cols <- colnames(basins@data)
    updateSelectInput(session, "shpcol", choices = cols, selected = tail(cols, 1))
    output$shptable <- renderDT({
      datatable(head(basins@data, 10), options = list(scrollX = TRUE))
    })
  })
  
  # Read outlet coordinates CSV, no header, assign colnames and save
  observeEvent(input$outletCoords, {
    req(input$outletCoords)
    df <- tryCatch({
      read.csv(input$outletCoords$datapath, header = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
    if (is.null(df) || ncol(df) < 3) {
      showNotification("Invalid outlet coordinates CSV: Must have at least 3 columns without header", type = "error")
      outlet_coords(NULL)
      return()
    }
    colnames(df) <- c("ID", "outletLng", "outletLat")
    outlet_coords(df)
    showNotification("Outlet coordinates loaded successfully", type = "message")
  })
  
  # Plot the map with polygons and matched status
  observeEvent(input$plot, {
    req(shp_data())
    basins <- shp_data()
    req(input$shpcol, rvh_data(), input$rvhcol)
    RVH <- rvh_data()
    coords_df <- outlet_coords()
    
    # Match polygons
    values <- match(basins@data[, input$shpcol], RVH$SBtable[, input$rvhcol])
    # Safety check: ensure values has the same length as basins
    if (is.null(values) || length(values) != nrow(basins)) {
      values <- rep(NA, nrow(basins))
    }
    col <- ifelse(is.na(values), "red", "green")
    message <- sprintf("matching rate: %s percent", round(sum(!is.na(values)) * 100 / nrow(basins), 2))
    
    # Prepare leaflet base
    map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = spTransform(basins, CRS("+proj=longlat +datum=WGS84 +no_defs")),
                  label = paste("ID = ", basins@data[, input$shpcol]),
                  fillColor = col, fillOpacity = 0.4, weight = 2) %>%
      addLegend(position = "topright", colors = c("red", "green"), labels = c("Unmatched", "Matched")) %>%
      addControl(html = paste0("<b>", message, "</b>"), position = "topright")
    
    # If outlet coordinates are available, add them as points
    if (!is.null(coords_df)) {
      shp_ids <- basins@data[[input$shpcol]]
      coord_ids <- coords_df$ID
      
      matched_idx <- match(coord_ids, shp_ids)  # find which outlet coords match shapefile IDs
      
      # Matched outlet points
      matched_points <- coords_df[!is.na(matched_idx), ]
      # Unmatched outlet points
      unmatched_points <- coords_df[is.na(matched_idx), ]
      
      if (nrow(matched_points) > 0) {
        map <- map %>%
          addCircleMarkers(lng = matched_points$outletLng, lat = matched_points$outletLat,
                           color = "darkgreen", radius = 5,
                           label = paste("Matched Outlet ID:", matched_points$ID),
                           fillOpacity = 1)
      }
      
      if (nrow(unmatched_points) > 0) {
        map <- map %>%
          addCircleMarkers(lng = unmatched_points$outletLng, lat = unmatched_points$outletLat,
                           color = "darkred", radius = 5,
                           label = paste("Unmatched Outlet ID:", unmatched_points$ID),
                           fillOpacity = 1)
      }
    }
    
    output$map <- renderLeaflet({ map })
  })  
  # Prepare outlet coordinates list matched to shapefile polygons
  getOutletCoordsList <- reactive({
    req(shp_data(), input$shpcol)
    basins <- shp_data()
    df_coords <- outlet_coords()
    
    if (is.null(df_coords)) {
      # No outlet coords provided; assign NA coords
      n <- nrow(basins)
      return(list(outletLat = rep(NA_real_, n), outletLng = rep(NA_real_, n)))
    }
    
    shp_ids <- basins@data[[input$shpcol]]
    coord_ids <- df_coords$ID
    
    matched_idx <- match(shp_ids, coord_ids)
    
    outletLat <- rep(-9999, length(shp_ids))
    outletLng <- rep(-9999, length(shp_ids))
    ID        <- rep(NA, length(shp_ids))
    
    valid <- !is.na(matched_idx)
    outletLat[valid] <- df_coords$outletLat[matched_idx[valid]]
    outletLng[valid] <- df_coords$outletLng[matched_idx[valid]]
    ID       [valid] <- df_coords$ID       [matched_idx[valid]]

    return(list(ID = ID, outletLat = outletLat, outletLng = outletLng))
  })
  
  # Conversion button logic with progress and feedback
  observeEvent(input$convert, {
    req(shp_data(), rvh_data(), input$shpcol, input$rvhcol)
    
    shinyjs::disable("convert")
    output$conversionmessage <- renderText("Conversion in progress, please wait...")
    
    withProgress(message = "Converting files...", value = 0, {
      incProgress(0.2)
      # Load conversion function from GitHub
      source("https://raw.githubusercontent.com/rarabzad/shp2geojson/main/rvn_rvh_shp_geojson.R", local = TRUE)
      
      exdir <- unzipped_dir()
      req(exdir)
      
      shpfile_path <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE)[1]
      req(shpfile_path)
      
      output_dir <- dirname(shpfile_path)
      base_name <- tools::file_path_sans_ext(basename(shpfile_path))
      out_json_path <- file.path(output_dir, paste0(base_name, ".json"))
      outputfile(out_json_path)
      
      CRSshp_val <- input$CRSshp
      if (CRSshp_val == "") CRSshp_val <- NA_character_
      
      outlet_list <- getOutletCoordsList()
      
      incProgress(0.5)
      
      res <- tryCatch({
        rvn_rvh_shp_geojson(shpfile = shpfile_path,
                            rvhfile = input$rvhfile$datapath,
                            outputfile = out_json_path,
                            CRSshp = CRSshp_val,
                            matchingColumns = list(shpfile = input$shpcol, rvhfile = input$rvhcol),
                            simplifyGeometry = as.logical(input$simplifyGeometry),
                            outletCoords = outlet_list)
      }, error = function(e) {
        e$message
      })
      
      incProgress(1)
      
      if (is.character(res) && grepl("^Error|failed|invalid", res, ignore.case = TRUE)) {
        output$conversionmessage <- renderText(paste("Conversion failed:", res))
      } else {
        output$conversionmessage <- renderText(paste("Conversion successful!"))
      }
    })
    
    shinyjs::enable("convert")
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      if (is.null(outputfile()) || !file.exists(outputfile())) {
        return("output.geojson")
      }
      basename(outputfile())
    },
    content = function(file) {
      req(outputfile())
      out_path <- normalizePath(outputfile(), mustWork = TRUE)
      file.copy(out_path, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)


