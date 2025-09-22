library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(leaflet)
library(sf)
library(RavenR)
library(geojsonio)
library(rmapshaper)
library(shinyjs)
library(shinycssloaders)

options(shiny.sanitize.errors = FALSE)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .header-img { height: 80px; margin-bottom: 10px; }
      .section-header { font-weight: 700; font-size: 18px; margin-top: 20px; margin-bottom: 10px; border-bottom: 2px solid #337ab7; padding-bottom: 5px; }
      .help-text { font-size: 13px; color: #555; margin-bottom: 15px; }
      .btn-primary { background-color: #337ab7; border-color: #2e6da4; }
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
      wellPanel(
        tags$div(class = "section-header", "Upload Files"),
        fileInput("rvhfile", "Choose RVH File (.rvh)", multiple = FALSE, accept = c(".rvh")),
        fileInput("shpzip", "Choose ZIP with ESRI Shapefile", multiple = FALSE, accept = c(".zip")),
        fileInput("outletCoords", "Choose Outlet Coordinates CSV", multiple = FALSE, accept = c(".csv")),
        tags$div(class = "help-text", "CSV must have NO HEADER and three columns: Subbasin ID, Longitude, Latitude."),
        tags$a(href = "https://github.com/rarabzad/shp2geojson/raw/main/test%20cases.zip", 
               class = "btn btn-info btn-sm", "Download example files", target = "_blank")
      ),
      wellPanel(
        tags$div(class = "section-header", "Matching Attributes"),
        selectInput("rvhcol", "Linking Attribute in RVH File:", choices = NULL),
        selectInput("shpcol", "Linking Attribute in Shapefile:", choices = NULL)
      ),
      wellPanel(
        tags$div(class = "section-header", "Settings"),
        radioButtons("simplifyGeometry", "Simplify GeoJSON?", choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE),
        textInput("CRSshp", "CRS for Shapefile (Proj4 format, if missing)", value = ""),
        tags$a(href = "https://spatialreference.org/", class = "btn btn-link btn-sm", "Find CRS info here", target = "_blank")
      ),
      actionButton("plot", "Plot Map", width = "100%", icon = icon("map")),
      br(), br(),
      actionButton("convert", "Convert to GeoJSON", width = "100%", icon = icon("cogs")),
      br(), br(),
      textOutput("conversionmessage"),
      br(),
      downloadButton("downloadData", "Download GeoJSON", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("RVH Preview", DTOutput("rvhtable") %>% shinycssloaders::withSpinner()),
        tabPanel("Shapefile Preview", DTOutput("shptable") %>% shinycssloaders::withSpinner()),
        tabPanel("Map Preview", leafletOutput("map", height = "600px") %>% shinycssloaders::withSpinner())
      )
    )
  )
)

server <- function(input, output, session) {
  rvh_data <- reactiveVal(NULL)
  shp_data <- reactiveVal(NULL)
  outlet_coords <- reactiveVal(NULL)
  outputfile <- reactiveVal(NULL)
  unzipped_dir <- reactiveVal(NULL)
  shinyjs::disable("downloadData")

  shinyjs::disable("plot")
  shinyjs::disable("convert")
  
  observeEvent(input$rvhfile, {
    req(input$rvhfile)
    rvh_read <- tryCatch(rvn_rvh_read(input$rvhfile$datapath), error = function(e) NULL)
    if (is.null(rvh_read)) {
      showNotification("Failed to read RVH file", type = "error")
      return()
    }
    rvh_data(rvh_read)
    cols <- colnames(rvh_read$SBtable)
    updateSelectInput(session, "rvhcol", choices = cols, selected = tail(cols, 1))
    output$rvhtable <- renderDT({ datatable(head(rvh_read$SBtable, 10), options = list(scrollX = TRUE)) })
  })
  
  observeEvent(input$shpzip, {
    req(input$shpzip)
    exdir <- file.path(dirname(input$shpzip$datapath), "shp_unzipped")
    unlink(exdir, recursive = TRUE, force = TRUE)
    dir.create(exdir)
    unzip(input$shpzip$datapath, exdir = exdir)
    unzipped_dir(exdir)
    
    shpfile_path <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE)[1]
    if (is.null(shpfile_path)) {
      showNotification("No .shp file found in ZIP", type = "error")
      return()
    }
    
    basins <- tryCatch(st_read(shpfile_path, quiet = TRUE), error = function(e) NULL)
    if (is.null(basins)) {
      showNotification("Failed to read shapefile", type = "error")
      return()
    }
    if (is.na(st_crs(basins))) {
      e <- st_bbox(basins)
      if(all(c(e["xmin"], e["xmax"]) >= -180 & c(e["xmin"], e["xmax"]) <= 180)) {
        # WGS84
        suggested_epsg <- 4326
      } else {
        # Compute UTM zone, clamp to 1-60
        lon_center <- mean(c(e["xmin"], e["xmax"]))
        utm_zone <- floor((lon_center + 180)/6) + 1
        utm_zone <- max(min(utm_zone, 60), 1)
        suggested_epsg <- 26900 + utm_zone  # NAD83 UTM
      }
      st_crs(basins) <- suggested_epsg
      
      # Only update CRSshp text box if it is empty
      if (is.null(input$CRSshp) || input$CRSshp == "") {
        updateTextInput(session, "CRSshp", value = st_crs(suggested_epsg)$proj4string)
      }
    }
    shp_data(basins)
    updateSelectInput(session, "shpcol", choices = colnames(basins), selected = tail(colnames(basins),1))
    output$shptable <- renderDT({ datatable(head(basins,10), options=list(scrollX=TRUE)) })
  })
  
  observeEvent(input$outletCoords, {
    req(input$outletCoords)
    df <- tryCatch(read.csv(input$outletCoords$datapath, header = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || ncol(df) < 3) {
      showNotification("Invalid CSV: Must have at least 3 columns without header", type = "error")
      outlet_coords(NULL)
      return()
    }
    colnames(df) <- c("ID","outletLng","outletLat")
    outlet_coords(df)
    showNotification("Outlet coordinates loaded successfully", type = "message")
  })
  
  basins_latlon <- reactive({
    req(shp_data())
    shp <- shp_data()
    if (!is.null(input$CRSshp) && input$CRSshp != "") {
      # Attempt to set CRS safely
      crs_try <- try(st_crs(shp) <- input$CRSshp, silent = TRUE)
      if (inherits(crs_try, "try-error") || is.na(st_crs(shp))) {
        st_crs(shp) <- 4326
      }
    } else if (is.na(st_crs(shp))) {
      st_crs(shp) <- 4326
    }
    
    # Transform to lat/lon for Leaflet
    st_transform(shp, crs = 4326)
  })
  
  observe({
    if(!is.null(rvh_data()) && !is.null(shp_data())) {
      shinyjs::enable("plot")
      shinyjs::enable("convert")
    }
  })
  
  observeEvent(input$plot, {
    req(basins_latlon(), rvh_data(), input$rvhcol, input$shpcol)
    basins <- basins_latlon()
    RVH <- rvh_data()
    coords_df <- outlet_coords()
    
    values <- match(basins[[input$shpcol]], RVH$SBtable[[input$rvhcol]])
    col <- ifelse(is.na(values),"red","green")
    message <- sprintf("Matching rate: %s %%", round(sum(!is.na(values))*100/nrow(basins),2))
    
    map <- leaflet(basins) %>%
      addTiles() %>%
      addPolygons(label = paste("ID =", basins[[input$shpcol]]),
                  fillColor = col, fillOpacity = 0.4, weight = 2) %>%
      addLegend(position = "topright", colors = c("red","green"), labels = c("Unmatched","Matched")) %>%
      addControl(html = paste0("<b>",message,"</b>"), position="topright")
    
    if(!is.null(coords_df)){
      matched_idx <- match(coords_df$ID, basins[[input$shpcol]])
      matched_points <- coords_df[!is.na(matched_idx),]
      unmatched_points <- coords_df[is.na(matched_idx),]
      
      if(nrow(matched_points) > 0)
        map <- map %>% addCircleMarkers(lng=matched_points$outletLng, lat=matched_points$outletLat, color="darkgreen", radius=5, label=paste("Matched Outlet ID:",matched_points$ID))
      if(nrow(unmatched_points) > 0)
        map <- map %>% addCircleMarkers(lng=unmatched_points$outletLng, lat=unmatched_points$outletLat, color="darkred", radius=5, label=paste("Unmatched Outlet ID:",unmatched_points$ID))
    }
    
    output$map <- renderLeaflet({ map })
  })
  
  getOutletCoordsList <- reactive({
    req(shp_data(), input$shpcol)
    basins <- shp_data()
    df_coords <- outlet_coords()
    
    n <- nrow(basins)
    outletLat <- rep(NA_real_, n)
    outletLng <- rep(NA_real_, n)
    ID <- rep(NA_character_, n)
    
    if(!is.null(df_coords)){
      matched_idx <- match(basins[[input$shpcol]], df_coords$ID)
      valid <- !is.na(matched_idx)
      outletLat[valid] <- df_coords$outletLat[matched_idx[valid]]
      outletLng[valid] <- df_coords$outletLng[matched_idx[valid]]
      ID[valid] <- df_coords$ID[matched_idx[valid]]
    }
    list(ID=ID, outletLat=outletLat, outletLng=outletLng)
  })
  
observeEvent(input$convert, {
  req(shp_data(), rvh_data(), input$shpcol, input$rvhcol)

  shinyjs::disable("convert")
  output$conversionmessage <- renderText("Conversion in progress, please wait...")

  withProgress(message = "Converting files...", value = 0, {
    incProgress(0.15, detail = "Loading converter...")
    # (re)load converter function if you source it from GitHub
    source("https://raw.githubusercontent.com/rarabzad/shp2geojson/main/rvn_rvh_shp_geojson.R", local = TRUE)

    exdir <- unzipped_dir()
    req(exdir)

    shpfile_path <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE)[1]
    req(shpfile_path)

    output_dir <- dirname(shpfile_path)
    base_name <- tools::file_path_sans_ext(basename(shpfile_path))
    # request .geojson in same folder (converter will still normalize)
    requested_out <- file.path(output_dir, paste0(base_name, ".geojson"))

    # Prepare CRS argument: convert numeric-only strings to integer EPSG if appropriate
    CRSshp_val <- input$CRSshp
    if (!is.null(CRSshp_val) && nzchar(CRSshp_val) && grepl("^\\d+$", CRSshp_val)) {
      CRSshp_val <- as.integer(CRSshp_val)
    }
    if (is.null(CRSshp_val) || CRSshp_val == "") CRSshp_val <- NA

    outlet_list <- getOutletCoordsList()

    incProgress(0.4, detail = "Running converter...")
    # call the converter (it returns a list: success, file, sf, message)
    res <- tryCatch({
      rvn_rvh_shp_geojson(
        shpfile = shpfile_path,
        rvhfile = input$rvhfile$datapath,
        outputfile = requested_out,
        CRSshp = CRSshp_val,
        matchingColumns = list(shpfile = input$shpcol, rvhfile = input$rvhcol),
        simplifyGeometry = if (isTRUE(as.logical(input$simplifyGeometry))) TRUE else as.numeric(input$simplifyGeometry),
        outletCoords = outlet_list,
        overwrite = TRUE
      )
    }, error = function(e) {
      list(success = FALSE, file = NULL, sf = NULL, message = e$message)
    })

    incProgress(1)

    # inspect result and update app state
    if (is.list(res) && !is.null(res$success) && isTRUE(res$success) && !is.null(res$file) && file.exists(res$file)) {
      # set the authoritative path to what the converter actually wrote
      outputfile(normalizePath(res$file))
      output$conversionmessage <- renderText(paste("Conversion successful:", res$message))
      showNotification("Conversion successful", type = "message")
      shinyjs::enable("downloadData")
    } else {
      # ensure no stale path stays set
      outputfile(NULL)
      shinyjs::disable("downloadData")
      msg <- if (is.list(res) && !is.null(res$message)) res$message else "Conversion failed (unknown reason)"
      output$conversionmessage <- renderText(paste("Conversion failed:", msg))
      showNotification(paste("Conversion failed:", msg), type = "error")
    }
  }) # end withProgress

  shinyjs::enable("convert")
})

  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      if (is.null(outputfile()) || !file.exists(outputfile())) return("output.geojson")
      basename(outputfile())
    },
    content = function(file) {
      req(outputfile())
      if (!file.exists(outputfile())) stop("GeoJSON file not found. Please convert first!")
      file.copy(outputfile(), file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)


