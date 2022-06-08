

ui <- shiny::fluidPage(title = "CashFusion Stats",
  
  theme = bslib::bs_theme( 
    bg ="#f2f8ee", fg = "#1c3557", primary = "#467c9e", secondary = "#1c3557"),
  # For more info on themes, see https://shiny.rstudio.com/articles/themes.html 
  shiny::br(),
  
  shiny::HTML("<img src=\"images/logos/CashFusion-Stats-logo-1869-by-478.png\" width=\"50%\" />"),
  shiny::br(),
  shiny::hr(),
  shiny::h5("Since November 28, 2019, more than"),
  shiny::column(12, align = "center",
    shiny::h2(shiny::textOutput("n_fusions_text")),
    shiny::h5("involving"),
    shiny::h2(shiny::textOutput("n_bch_text"))),
  # https://spartanideas.msu.edu/2016/09/09/formatting-in-a-shiny-app/
  shiny::h4("have been performed to protect Bitcoin Cash users' fundamental human rights to privacy in their financial affairs."),
  shiny::br(),
  # span(shiny::checkboxInput("dark_mode", ("Dark mode")), style="color:red;font-weight:bold;font-size:150%"),
  #  shiny::span(shinyWidgets::materialSwitch(
  #    inputId = "dark_mode",
  #    label = "Dark mode", 
  #    status = "primary",
  #    right = TRUE
  #  ), style="color:red;font-weight:bold;font-size:150%"),
    
    shiny::plotOutput("line_chart", height = "600px"),
    shiny::br(),
    shiny::br(),
    
  
    shiny::radioButtons("line_plot_type",
      label = "Choose Chart Type",
      choiceNames = c("Number of CashFusions", "BCH in CashFusion UTXO \"pool\"",
        "BCH coming into CashFusion UTXO \"pool\" (cumulative)", "BCH leaving CashFusion UTXO \"pool\" (cumulative)",
        "Number of transactions spending from the CashFusion UTXO \"pool\""),
      choiceValues = c("n.fusions", "value.stock",  
        "incoming.value.cumulative", "outgoing.value.cumulative", "outgoing.txs")),    
  
  #checkboxInput("fusion_friday", "Fusion Fridays!"),
  shiny::span(shinyWidgets::materialSwitch(
    inputId = "fusion_friday",
    label = "Fusion Fridays!", 
    status = "primary",
    right = TRUE
  ), style="color:#e63946;font-weight:bold;font-size:100%"),
  
  sliderInput("line_plot_date_range",
    label = NULL,
    # label = "Zoom in on dates:",
    min = as.Date("2019-11-28","%Y-%m-%d") - 60,
    max = as.Date(format(Sys.Date(), "%Y-%m-%d"), "%Y-%m-%d") + 60,
    value = c(as.Date("2019-11-28","%Y-%m-%d"), as.Date(format(Sys.Date(), "%Y-%m-%d"), "%Y-%m-%d")),
    timeFormat = "%Y-%m-%d",
    width = "100%"),
  shiny::br(),
  shiny::h5("👆 Move the sliders to zoom in on a time period. Click \"Fusion Fridays!\""),
  
  shiny::br(),
  shiny::br(),
  plotly::plotlyOutput("sankey"),
  shiny::br(),
  shiny::br(),
  shiny::span(shinyWidgets::materialSwitch(
    inputId = "sankey_parent",
    label = "Show transactions before CashFusion", 
    status = "primary",
    right = TRUE
  ), style="color:#e63946;font-weight:bold;font-size:85%"),
  shiny::span(shinyWidgets::materialSwitch(
    inputId = "sankey_child",
    label = "Show transactions after CashFusion", 
    status = "primary",
    right = TRUE
  ), style="color:#e63946;font-weight:bold;font-size:85%"),
  
  shiny::br(),
  shiny::h6("👇 Click on rows to visualize CashFusion transactions in the Sankey diagram above 👆"),
  shiny::h6("👇 Click on column headers to sort the data table by column."),
  shiny::h6("👇 Click and drag column headers to rearrange columns."),
  # shiny::h4("NOTE: For the time being, the data is updated manually by CashFusion Red Team, so the data shown here may be slightly out-of-date. Automatic data updates is a planned feature."),
  shiny::br(),
  DT::dataTableOutput("fusion_txs_table"),
  # https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
  shiny::br(),
  shiny::br(),
  shiny::h5(shiny::HTML("<u><a href=\"https://github.com/Rucknium/CashFusionStats/tree/main/data\">Data available here</a></u>")),
  shiny::h5(shiny::HTML("<u><a href=\"https://github.com/Rucknium/CashFusionStats\">Code available here</a></u>")),
  shiny::br(),
  shiny::br(),
  shiny::h5("CashFusion Stats is a project of the CashFusion Red Team"),
  shiny::br(),
  shiny::HTML("<img src=\"/images/logos/CashFusion-Red-Team-logo-1869-by-478.png\" width=\"100%\" />"),
  # Must have images in the www directory
  shiny::br(),
  shiny::br(),
  shiny::br()
  
  

)


