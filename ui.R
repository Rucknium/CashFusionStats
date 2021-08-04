

ui <- shiny::navbarPage("CashFusion Stats",
  theme = bslib::bs_theme(bootswatch = "cerulean"), 
  # For more info on themes, see https://shiny.rstudio.com/articles/themes.html 
  shiny::fluidRow(
    shiny::h4("Since November 28, 2019, more than"),
    shiny::column(12, align = "center",
      shiny::h1(shiny::textOutput("n_fusions_text")),
      shiny::h4("involving"),
      shiny::h1(shiny::textOutput("n_bch_text"))),
    # https://spartanideas.msu.edu/2016/09/09/formatting-in-a-shiny-app/
    shiny::h4("have been performed to protect Bitcoin Cash users' fundamental human rights to privacy in their financial affairs."),
    shiny::br(),
    shiny::br(),
  # span(shiny::checkboxInput("dark_mode", ("Dark mode")), style="color:red;font-weight:bold;font-size:150%"),
  #  shiny::span(shinyWidgets::materialSwitch(
  #    inputId = "dark_mode",
  #    label = "Dark mode", 
  #    status = "primary",
  #    right = TRUE
  #  ), style="color:red;font-weight:bold;font-size:150%"),
    
    shiny::plotOutput("line_chart", height = "600px"),
    
    #checkboxInput("fusion_friday", "Fusion Fridays!"),
    shiny::span(shinyWidgets::materialSwitch(
      inputId = "fusion_friday",
      label = "Fusion Fridays!", 
      status = "primary",
      right = TRUE
    ), style="color:red;font-weight:bold;font-size:100%"),
    
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
    shiny::h6("👇 Click on column headers to sort the data table by column."),
    shiny::h6("👇 Click and drag column headers to rearrange columns."),
    shiny::br(),
    DT::dataTableOutput("fusion_txs_table"),
    # https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
    shiny::br(),
    shiny::br(),
    shiny::h5(shiny::HTML("<u><a href=\"https://github.com/Rucknium/CashFusionStats/tree/main/data\">Data available here</a></u>")),
    shiny::h5(shiny::HTML("<u><a href=\"https://github.com/Rucknium/CashFusionStats\">Code available here</a></u>")),
    shiny::br(),
    shiny::br()
  )
  
)








