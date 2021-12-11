# Extracts all CashFusions via RPC calls to bitcoind

# bitcoind MUST be running with the transaction index enabled:
# Set txindex=1 in bitcoin.conf, or -txindex when invoking bitcoind on the command line


while (TRUE) {

# install.packages("devtools")
# devtools::install_github("Rucknium/rbch")
library(rbch)
bch.config <- rbch::conrpc("~/config-files/bitcoin.conf")
# Path to bitcoind config file. The file must contain, at a minimum:
# testnet=0
# rpcuser=<userhere>
# rpcpassword=<passwordhere>

# For RPC documentation, see https://docs.bitcoincashnode.org/doc/json-rpc/
# https://docs.bitcoincashnode.org/doc/json-rpc/getblock.html
# https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html


# fusion.raw.data.dir <- "/srv/shiny-server/fusionstats/data-raw/"
fusion.polished.data.dir <- "/srv/shiny-server/fusionstats/data/"
# MUST have trailing /

fusions.df <- readRDS(paste0( fusion.polished.data.dir, "fusions_df.rds") )

current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks

last.updated.fusion.height <- max(fusions.df$block.height)

fused.update.ls <- vector("list", length = current.block.height)

for (iter.block.height in last.updated.fusion.height:current.block.height) {
  
  if (iter.block.height %% 10 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbch::getblockhash(bch.config, iter.block.height)
  block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l2")
  # Argument verbose = 2 gives full transaction data
  # For some reason it doesn't give the fee: 
  # https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html
  
  raw.txs.ls <- block.data@result$tx
  
  fused.ind <- sapply(raw.txs.ls, FUN = function(x) {
    ret <- x$vout[[1]]$scriptPubKey$asm
    if (length(ret) == 0) { return(FALSE) }
    ret <- substr(ret, 1, 17) == "OP_RETURN 5920070"
    return(ret)
  }) 
  # "OP_RETURN 5920070" prefix indicates that the transaction is a CashFusion transaction
  
  if ( ! any(fused.ind) ) { next }
  
  fused.ls <- lapply(raw.txs.ls[fused.ind], FUN = function(x) { 
    txid <- x$txid
    n.inputs <- length(x$vin)
    n.outputs <- length(x$vout)
    value <- sum(sapply(x$vout, FUN = function(y) { y$value }) )
    size <- x$size
    data.frame(txid, n.inputs, n.outputs, value, size, stringsAsFactors = FALSE)
  })
  
  fused.df <- do.call(rbind, fused.ls)
  fused.df$tx.fee <- NA
  
  for (tx.i in 1:nrow(fused.df)) {
    fused.df$tx.fee[tx.i] <- rbch::txfee(bch.config, fused.df$txid[tx.i])
  }
  
  fused.df$block.height <- iter.block.height
  fused.df$block.time <- as.POSIXct(block.data@result$time,  origin = "1970-01-01", tz = "GMT")
  fused.update.ls[[iter.block.height]] <- fused.df
  
}

fused.update.df <- do.call(rbind, fused.update.ls)

if (nrow(fused.update.df) > 0) {
  
  fused.update.df$block.time.orig <- fused.update.df$block.time
  fused.update.df$block.time <- as.character(fused.update.df$block.time)
  fused.update.df$txid.link <- paste0("<a href=\"https://explorer.bitcoin.com/bch/tx/", 
    fused.update.df$txid, "\">", substr(fused.update.df$txid, 1, 8), "</a>")
  fused.update.df$block.date <- lubridate::date(fused.update.df$block.time.orig)
  
  fusions.df <- unique(rbind(fusions.df, fused.update.df))
  fusions.df <- fusions.df[order(fusions.df$block.height, decreasing = TRUE), ]
  
  saveRDS(fusions.df, file = paste0(fusion.polished.data.dir, "fusions_df.rds"), compress = FALSE)
  write.csv(fusions.df, file = paste0(fusion.polished.data.dir, "fusions_df.csv"), row.names = FALSE)
  
  # https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf
  fusions.date.agg <- as.data.frame.table(table(Date = fusions.df$block.date))
  fusions.date.agg$Date <- as.POSIXct(as.character(fusions.date.agg$Date), tz = "GMT")
  fusions.date.agg.holes <- data.frame(Date = seq(min(fusions.date.agg$Date), max(fusions.date.agg$Date), "days"))
  fusions.date.agg.holes$Date <- as.POSIXct(as.character(lubridate::date(fusions.date.agg.holes$Date)), tz = "GMT")
  fusions.date.agg <- merge(fusions.date.agg, fusions.date.agg.holes, all = TRUE)
  fusions.date.agg$Freq[is.na(fusions.date.agg$Freq)] <- 0
  fusions.date.agg$moving.average.7.day <- NA
  fusions.date.agg$moving.average.7.day[4:(nrow(fusions.date.agg) - 3)] <- zoo::rollapply(fusions.date.agg$Freq, 7, mean)
  # Here, center the moving average; don't lag it.
  # https://koalatea.io/r-moving-average/
  fusions.date.agg <- fusions.date.agg[order(fusions.date.agg$Date, decreasing = FALSE), ]
  
  saveRDS(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.rds"), compress = FALSE)
  write.csv(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.csv"), row.names = FALSE)
  
  fusions.summary.ls <- list(
    n.fusions = nrow(fusions.df), 
    n.bch = sum(fusions.df$value, na.rm = TRUE),
    full.release = as.POSIXct("2020-07-30", tz = "GMT")
    # https://github.com/Electron-Cash/Electron-Cash/releases/tag/4.1.0
  )
  
  saveRDS(fusions.summary.ls, file = paste0(fusion.polished.data.dir, "fusions_summary_ls.rds"), compress = FALSE)
  
  
  
  
  latest.tx <- getrawtransaction(bch.config, fusions.df$txid[1], verbose = TRUE)@result
  
  latest.tx <- raw.txs.ls[fused.ind][[1]]
  
  tx.inputs <- txinids(bch.config, latest.tx$txid)
  
  tx.inputs <- split(tx.inputs$txinpos, tx.inputs$txinids)
  
  input.amounts <- vector("list", length(tx.inputs))
  
  for (i in seq_along(tx.inputs)) {
    
    tx.temp <- getrawtransaction(bch.config, names(tx.inputs)[i], verbose = TRUE)@result$vout
    
    addresses <- vector("character", length(tx.inputs[[i]]))
    
    for (j in seq_along(tx.inputs[[i]])) {
      extracted.address <- tx.temp[[ tx.inputs[[i]][j] ]]$scriptPubKey$addresses
      stopifnot(length(extracted.address) == 1)
      stopifnot(length(extracted.address[[1]]) == 1)
      addresses[j] <- extracted.address[[1]]
    }
    
    input.amounts[[i]] <- data.frame(addresses = addresses,
      value = utxovalue(bch.config, names(tx.inputs)[i])[ tx.inputs[[i]] ],
      creating.tx = names(tx.inputs)[i],
      stringsAsFactors = FALSE)
    
  }
  
  input.amounts <- do.call(rbind, input.amounts)
  
  
  graph.edgelist <- with(input.amounts, {
    rbind(data.frame(source = creating.tx, target = addresses, value = value),
      data.frame(source = addresses, target = latest.tx$txid, value = value) )
  } )
  
  
  addresses <- vector("character", length(latest.tx$vout) - 1 )
  value <- vector("numeric", length(latest.tx$vout) - 1 )
  
  for (j in seq_along(  latest.tx$vout  )[-1]  ) {  # Don't take the first output since it is an OP_RETURN
    extracted.address <- latest.tx$vout[[j]]$scriptPubKey$addresses
    stopifnot(length(extracted.address) == 1)
    stopifnot(length(extracted.address[[1]]) == 1)
    addresses[j] <- extracted.address[[1]]
    
    value[j] <- latest.tx$vout[[j]]$value
  }
  
  
  graph.edgelist <- rbind(graph.edgelist,
    data.frame(source = latest.tx$txid, target = addresses, value = value)
  )
  
  
  saveRDS(graph.edgelist, file = paste0(fusion.polished.data.dir, "graph_edgelist.rds"), compress = FALSE)
  
}


Sys.sleep(5 * 60)

}


# NOTE: readRDS() with compress = FALSE loads twice as fast as with compress = TRUE. 0.25 secs vs 0.5 secs

#saveRDS(fusions.date.agg, file = "data/fusions_date_agg.rds", compress = FALSE)
#write.csv(fusions.date.agg, file = "data/fusions_date_agg.csv", row.names = FALSE)

# Store the data frame to disk
# fst::write.fst(fusions.df, "dataset.fst", compress = 50)

# Retrieve the data frame again
# system.time(test <- fst::read.fst("dataset.fst"))


# https://bitcoin.stackexchange.com/questions/41749/get-transaction-fees-per-transaction-via-gettransaction?rq=1
# "Unfortunately, you'll have to look up all the inputs and see how much is in them in order to calculate the transaction fee using this method."
