

#shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))
shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))
# see https://shiny.rstudio.com/articles/caching.html

fusion.polished.data.dir <- "data/"
# MUST have trailing /

fusions.summary.ls <- readRDS("data/fusions_summary_ls.rds")
fusions.date.agg <- readRDS(paste0(fusion.polished.data.dir, "fusions_date_agg.rds"))
fusions.df <- readRDS(paste0(fusion.polished.data.dir, "fusions_df.rds"))



server <- function(input, output, session) {
  
  thematic::thematic_shiny(font = "auto")


  
 # light <- bslib::bs_theme(bootswatch = "cerulean")
  #dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")
 # dark <- bslib::bs_theme(bootswatch = "darkly")
 # shiny::observe(session$setCurrentTheme(
 #   if (isTRUE(input$dark_mode)) {dark} else {light}
 # ))
  # https://rstudio.github.io/bslib/articles/bslib.html#dynamic-theming
  

  
  output$n_fusions_text <- shiny::renderText({
    paste0(prettyNum(fusions.summary.ls$n.fusions, big.mark = ","),  " CashFusions")
  })
  
  output$n_bch_text <- shiny::renderText({
    paste0(prettyNum(fusions.summary.ls$n.bch, big.mark = ","),  " BCH")
  })
  
  output$line_chart <- shiny::bindCache( {
    shiny::renderPlot({
      fusions.date.agg.temp <- fusions.date.agg[
        input$line_plot_date_range[1] <= as.Date(fusions.date.agg$Date) & 
          as.Date(fusions.date.agg$Date) <= input$line_plot_date_range[2], ]
      
      par(mar = c(8, 4, 4, 2) + 0.1)
      # c(bottom, left, top, right)
      
      plot(fusions.date.agg.temp$Date, fusions.date.agg.temp$Freq, type = "n",
        ylim = c(0, max(fusions.date.agg.temp$Freq)),
        xaxt = "n",
        main = "CashFusions per day",
        ylab = "CashFusions per day", xlab = "")
      
      #axis.Date(1, format = "%Y-%m-%d")
      
      axis.POSIXct(1, at = seq(as.POSIXct( "2019-12-01", format = "%Y-%m-%d"), 
        max(fusions.date.agg.temp$Date), by = "1 mon"), 
        format = "%m/%Y", las = 2)
      # Is hard-coded as.POSIXct( "2019-12-01", format = "%Y-%m-%d")
      title(xlab = "Date", mgp = c(5.5, 1, 0))
      # "Note that mgp[1] affects title whereas mgp[2:3] affect axis. The default is c(3, 1, 0))"
      
      polygon(c(min(fusions.date.agg.temp$Date) - 1, fusions.date.agg.temp$Date, max(fusions.date.agg.temp$Date) + 1), 
        c(-1, fusions.date.agg.temp$Freq, -1),
        border = NA
        , col = "#1b98e0" # , xpd = TRUE
      )
      
      lines(fusions.date.agg.temp$Date, fusions.date.agg.temp$moving.average.7.day, lwd = 2, col = "black")
      legend("topleft", 
        legend = c("7-day moving average", "Official release date"),
        col = c("black", "red"),
        lwd = c(2, 2),
        lty = c(1, 2))
      
      abline(v = fusions.summary.ls$full.release, col = "red", lwd = 2, lty = 2)
      
      if (input$fusion_friday) {
        fusions.date.agg.temp.friday <- fusions.date.agg.temp[lubridate::wday(fusions.date.agg.temp$Date) == 6, ]
        points(fusions.date.agg.temp.friday$Date,  fusions.date.agg.temp.friday$Freq, col = "red", cex = 1.5) #, pch = ".")
        
        mtext(paste("CashFusions/day overall mean: ",
          round(mean(fusions.date.agg.temp$Freq), digits = 1),
          " | Friday mean: ", round(mean(fusions.date.agg.temp.friday$Freq, na.rm = TRUE), digits = 1)
        ), col = "red", line = 0.25)
        
      }
      
      par(mar = c(5, 4, 4, 2) + 0.1) #, mgp = c(3, 1, 0))
      
    })
  }, input$line_plot_date_range, input$fusion_friday )
  
  output$fusion_txs_table <- DT::renderDataTable({
    fusions.df[, c("block.height", "block.time",  "txid.link", "value", "tx.fee",
      "n.inputs", "n.outputs", "size", "txid", "block.time.orig",  "block.date" )]},
    # WARNING: If The above columns are changed, must also change "targets =" below
    rownames = FALSE,
    extensions = c("Buttons", "ColReorder"), 
    escape = FALSE,
    style = "default",
    options = list(dom = "Blfrtip", buttons = I("colvis"), colReorder = list(realtime = FALSE),
      columnDefs = list(list(targets = 9:11 - 1, visible = FALSE
      ))) )

  # https://rstudio.github.io/DT/extensions.html
  # https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
  # https://github.com/rstudio/DT/issues/153
  # The dom argument: https://datatables.net/reference/option/dom#Options
  # and https://stackoverflow.com/questions/51730816/remove-showing-1-to-n-of-n-entries-shiny-dt
  # and https://rstudio.github.io/DT/options.html
  
}











