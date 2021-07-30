

server <- function(input, output, session) {
  
  thematic::thematic_shiny(font = "auto")
  
 # light <- bslib::bs_theme(bootswatch = "cerulean")
  #dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")
 # dark <- bslib::bs_theme(bootswatch = "darkly")
 # shiny::observe(session$setCurrentTheme(
 #   if (isTRUE(input$dark_mode)) {dark} else {light}
 # ))
  # https://rstudio.github.io/bslib/articles/bslib.html#dynamic-theming
  
  
  fusions.df <- readRDS("data/fusions_df.rds")
  fusions.date.agg <- readRDS("data/fusions_date_agg.rds")
  
  output$n_fusions_text <- shiny::renderText({
    paste0(prettyNum(nrow(fusions.df), big.mark = ","),  " CashFusions")
  })
  
  output$n_bch_text <- shiny::renderText({
    paste0(prettyNum(sum(fusions.df$value, na.rm = TRUE), big.mark = ","),  " BCH")
  })
  
  output$line_chart <- shiny::renderPlot({
    fusions.date.agg.temp <- fusions.date.agg[
      input$line_plot_date_range[1] <= as.Date(fusions.date.agg$Date) & 
        as.Date(fusions.date.agg$Date) <= input$line_plot_date_range[2], ]

    plot(fusions.date.agg.temp$Date, fusions.date.agg.temp$Freq, type = "n",
      ylim = c(0, max(fusions.date.agg.temp$Freq)),
      main = "CashFusions per day",
      ylab = "CashFusions per day", xlab = "Date")
    
    polygon(c(min(fusions.date.agg.temp$Date) - 1, fusions.date.agg.temp$Date, max(fusions.date.agg.temp$Date) + 1), 
      c(-1, fusions.date.agg.temp$Freq, -1),
      border = NA
      , col = "#1b98e0" # , xpd = TRUE
      )

    if (input$fusion_friday) {
      fusions.date.agg.temp.friday <- fusions.date.agg.temp[lubridate::wday(fusions.date.agg.temp$Date) == 6, ]
      points(fusions.date.agg.temp.friday$Date,  fusions.date.agg.temp.friday$Freq, col = "red") #, pch = ".")
    }
  })
  
  output$fusion_txs_table <- DT::renderDataTable({
    fusions.df[, c("block.height", "block.time",  "txid.link", "value", "n.inputs", "n.outputs", "size" )]},
    rownames = FALSE,
    extensions = c("Buttons", "ColReorder"), 
    escape = FALSE,
    options = list(dom = "Bfrtip", buttons = I("colvis"), colReorder = list(realtime = FALSE)) )
  # https://rstudio.github.io/DT/extensions.html
  # https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
  
  
}







