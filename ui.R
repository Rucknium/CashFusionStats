

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
    shiny::span(shinyWidgets::materialSwitch(
      inputId = "dark_mode",
      label = "Dark mode", 
      status = "primary",
      right = TRUE
    ), style="color:red;font-weight:bold;font-size:150%"),
    
    shiny::plotOutput("line_chart"),
    #checkboxInput("fusion_friday", "Fusion Fridays!"),
    shiny::span(shinyWidgets::materialSwitch(
      inputId = "fusion_friday",
      label = "Fusion Fridays!", 
      status = "primary",
      right = TRUE
    ), style="color:red;font-weight:bold;font-size:100%"),
    
    sliderInput("line_plot_date_range",
      "Zoom in on dates:",
      min = as.Date("2019-11-28","%Y-%m-%d"),
      max = as.Date(format(Sys.Date(), "%Y-%m-%d"), "%Y-%m-%d"),
      value = c(as.Date("2019-11-28","%Y-%m-%d"), as.Date(format(Sys.Date(), "%Y-%m-%d"), "%Y-%m-%d")),
      timeFormat = "%Y-%m-%d",
      width = "100%"),
    shiny::br(),
    shiny::br(),
    DT::dataTableOutput("fusion_txs_table"),
    # https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
    shiny::br(),
    shiny::br(),
    shiny::h5(shiny::HTML("<a href=\"https://github.com/Rucknium/CashFusionStats\">Code available here</a>")),
    shiny::br(),
    shiny::br()
  )
  
)






