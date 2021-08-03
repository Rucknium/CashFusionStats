# Extracts all CashFusions via RPC calls to bitcoind

# bitcoind MUST be running with the transaction index enabled:
# Set txindex=1 in bitcoin.conf, or -txindex when invoking bitcoind on the command line

# install.packages("rbtc")
library(rbtc)
bch.config <- rbtc::conrpc("~/.bitcoin/bitcoin.conf")
# Path to bitcoind config file. The file must contain, at a minimum:
# testnet=0
# rpcuser=<userhere>
# rpcpassword=<passwordhere>

# For RPC documentation, see https://docs.bitcoincashnode.org/doc/json-rpc/
# https://docs.bitcoincashnode.org/doc/json-rpc/getblock.html
# https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html

current.block.height <- rbtc::rpcpost(bch.config, "getblockchaininfo", list())@result$blocks
# 698417
fused.all.ls <- vector("list", length = current.block.height)

first.fusion.height <- 610700
# This is a block found on Nov 26, 2019. Fusions started Nov 28, 2019.
# Giving a little time buffer to be certain all fusions are captured.

for (iter.block.height in first.fusion.height:current.block.height) {
  
  if (iter.block.height %% 1000 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbtc::rpcpost(bch.config, "getblockhash", list(iter.block.height))
  block.data <- rbtc::rpcpost(bch.config, "getblock", list(block.hash@result, 2))
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
  fused.df$block.height <- iter.block.height
  fused.df$block.time <- as.POSIXct(block.data@result$time,  origin = "1970-01-01", tz = "GMT")
  fused.all.ls[[iter.block.height]] <- fused.df
  
}

fused.all.df <- do.call(rbind, fused.all.ls)
str(fused.all.df)


# saveRDS(fused.all.df, file = "data/fusions_df_original.rds", compress = FALSE)




# fusions.df <- readRDS("data/fusions_df_original.rds")
# saveRDS(fusions.df, file = paste0("data/fusions_df_original_height_", max(fusions.df$block.height), ".rds"), compress = FALSE)


fusion.raw.data.dir <- "data-raw/"
fusion.polished.data.dir <- "data/"
# MUST have trailing /

fusions.df.files <- list.files(fusion.raw.data.dir)
fusions.df.files <- fusions.df.files[grepl("fusions_df_original_height_", fusions.df.files)]

fusions.df <- readRDS(paste0( fusion.raw.data.dir,
  fusions.df.files[which.max(as.numeric(gsub("(fusions_df_original_height_)|([.]rds)", "", fusions.df.files)))]) )

current.block.height <- rbtc::rpcpost(bch.config, "getblockchaininfo", list())@result$blocks



last.updated.fusion.height <- max(fusions.df$block.height)

fused.update.ls <- vector("list", length = current.block.height)


for (iter.block.height in last.updated.fusion.height:current.block.height) {
  
  if (iter.block.height %% 10 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbtc::rpcpost(bch.config, "getblockhash", list(iter.block.height))
  block.data <- rbtc::rpcpost(bch.config, "getblock", list(block.hash@result, 2))
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
  fused.df$block.height <- iter.block.height
  fused.df$block.time <- as.POSIXct(block.data@result$time,  origin = "1970-01-01", tz = "GMT")
  fused.update.ls[[iter.block.height]] <- fused.df
  
}

fused.update.df <- do.call(rbind, fused.update.ls)
str(fused.update.df)
if (length(fused.update.df) > 0) {
  fusions.df <- unique(rbind(fusions.df, fused.update.df))
}

saveRDS(fusions.df, file = paste0("data/fusions_df_original_height_", 
  max(fusions.df$block.height), ".rds"), compress = FALSE)

# system.time(DT::datatable(fusions.df))


fusions.df.files <- list.files(fusion.raw.data.dir)
fusions.df.files <- fusions.df.files[grepl("fusions_df_original_height_", fusions.df.files)]

fusions.df <- readRDS(paste0( fusion.raw.data.dir,
  fusions.df.files[which.max(as.numeric(gsub("(fusions_df_original_height_)|([.]rds)", "", fusions.df.files)))]) )


fusions.df <- fusions.df[order(fusions.df$block.height, decreasing = TRUE), ]
fusions.df$block.time.orig <- fusions.df$block.time
fusions.df$block.time <- as.character(fusions.df$block.time)
fusions.df$txid.link <- paste0("<a href=\"https://explorer.bitcoin.com/bch/tx/", 
  fusions.df$txid, "\">", substr(fusions.df$txid, 1, 8), "</a>")
fusions.df$block.date <- lubridate::date(fusions.df$block.time.orig)

saveRDS(fusions.df, file = paste0(fusion.raw.data.dir, "fusions_df_height_", 
  max(fusions.df$block.height), ".rds"), compress = FALSE)
write.csv(fusions.df, file = paste0(fusion.raw.data.dir, "fusions_df_height_", 
  max(fusions.df$block.height), ".csv"), row.names = FALSE)
saveRDS(fusions.df, file = paste0(fusion.polished.data.dir, "fusions_df.rds"), compress = FALSE)
write.csv(fusions.df, file = paste0(fusion.polished.data.dir, "fusions_df.csv"), row.names = FALSE)
#saveRDS(fusions.df, file = "data/fusions_df.rds", compress = FALSE)
#write.csv(fusions.df, file = "data/fusions_df.csv", row.names = FALSE)


# https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf
fusions.date.agg <- as.data.frame.table(table(Date = fusions.df$block.date))
fusions.date.agg$Date <- as.POSIXct(as.character(fusions.date.agg$Date))
fusions.date.agg.holes <- data.frame(Date = seq(min(fusions.date.agg$Date), max(fusions.date.agg$Date), "days"))
fusions.date.agg.holes$Date <- as.POSIXct(as.character(lubridate::date(fusions.date.agg.holes$Date)))
fusions.date.agg <- merge(fusions.date.agg, fusions.date.agg.holes, all = TRUE)
fusions.date.agg$Freq[is.na(fusions.date.agg$Freq)] <- 0
fusions.date.agg$moving.average.7.day <- NA
fusions.date.agg$moving.average.7.day[4:(nrow(fusions.date.agg) - 3)] <- zoo::rollapply(fusions.date.agg$Freq, 7, mean)
# Here, center the moving average; don't lag it.
# https://koalatea.io/r-moving-average/
fusions.date.agg <- fusions.date.agg[order(fusions.date.agg$Date, decreasing = FALSE), ]

saveRDS(fusions.date.agg, file = paste0(fusion.raw.data.dir, "fusions_date_agg_height_", 
  max(fusions.df$block.height), ".rds"), compress = FALSE)
write.csv(fusions.date.agg, file = paste0(fusion.raw.data.dir, "fusions_date_agg_height_", 
  max(fusions.df$block.height), ".csv"), row.names = FALSE)
saveRDS(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.rds"), compress = FALSE)
write.csv(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.csv"), row.names = FALSE)

#saveRDS(fusions.date.agg, file = "data/fusions_date_agg.rds", compress = FALSE)
#write.csv(fusions.date.agg, file = "data/fusions_date_agg.csv", row.names = FALSE)


# https://bitcoin.stackexchange.com/questions/41749/get-transaction-fees-per-transaction-via-gettransaction?rq=1
# "Unfortunately, you'll have to look up all the inputs and see how much is in them in order to calculate the transaction fee using this method."
