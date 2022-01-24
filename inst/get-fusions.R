# Extracts all CashFusions via RPC calls to bitcoind

# bitcoind MUST be running with the transaction index enabled:
# Set txindex=1 in bitcoin.conf, or -txindex when invoking bitcoind on the command line

# install.packages("future.apply")
# install.packages("devtools")
# devtools::install_github("Rucknium/rbch")
library(rbch)
library(future.apply)
bch.config <- rbch::conrpc("~/.bitcoin/bitcoin.conf")
# Path to bitcoind config file. The file must contain, at a minimum:
# testnet=0
# rpcuser=<userhere>
# rpcpassword=<passwordhere>

# For RPC documentation, see https://docs.bitcoincashnode.org/doc/json-rpc/
# https://docs.bitcoincashnode.org/doc/json-rpc/getblock.html
# https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html

current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks
# 698417
fused.all.ls <- vector("list", length = current.block.height)

first.fusion.height <- 610700
# This is a block found on Nov 26, 2019. Fusions started Nov 28, 2019.
# Giving a little time buffer to be certain all fusions are captured.


future::plan(multiprocess)

# for (iter.block.height in first.fusion.height:current.block.height) {
#  system.time({
fused.all.ls <- future.apply::future_lapply( first.fusion.height:current.block.height, function(iter.block.height) {
  
  if (iter.block.height %% 1000 == 0) {
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
  
  if ( ! any(fused.ind) ) { return(NULL) }
  
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
  #fused.all.ls[[iter.block.height]] <- fused.df
  
  fused.df
  
} )
#})

fused.all.df <- do.call(rbind, fused.all.ls)
str(fused.all.df)




get.single.tx.graph <- function(con, txid, fusion = FALSE) {
  
  latest.tx <- rbch::getrawtransaction(con, txid, verbose = TRUE)@result
  
  block.height <- rbch::getblock(bch.config, blockhash = latest.tx$blockhash, verbosity = "l1")@result$height
  
  tx.inputs <- rbch::txinids(con, latest.tx$txid)
  
  tx.inputs <- split(tx.inputs$txinpos, tx.inputs$txinids)
  
  input.amounts <- vector("list", length(tx.inputs))
  
  for (i in seq_along(tx.inputs)) {
    
    tx.temp <- rbch::getrawtransaction(con, names(tx.inputs)[i], verbose = TRUE)@result$vout
    
    addresses <- vector("character", length(tx.inputs[[i]]))
    
    for (j in seq_along(tx.inputs[[i]])) {
      extracted.address <- tx.temp[[ tx.inputs[[i]][j] ]]$scriptPubKey$addresses
      stopifnot(length(extracted.address) == 1)
      stopifnot(length(extracted.address[[1]]) == 1)
      addresses[j] <- extracted.address[[1]]
    }
    
    input.amounts[[i]] <- data.frame(addresses = addresses,
      value = rbch::utxovalue(con, names(tx.inputs)[i])[ tx.inputs[[i]] ],
      creating.tx = names(tx.inputs)[i],
      stringsAsFactors = FALSE)
    
  }
  
  input.amounts <- do.call(rbind, input.amounts)
  
  
  graph.edgelist <- with(input.amounts, {
    rbind(data.frame(source = creating.tx, target = addresses, 
      value = value, block.height = block.height, fusion = fusion),
      data.frame(source = addresses, target = latest.tx$txid, 
        value = value, block.height = block.height, fusion = fusion) )
  } )
  
  
  addresses <- vector("character", length(latest.tx$vout) )
  value <- vector("numeric", length(latest.tx$vout) )
  
  for (j in seq_along(  latest.tx$vout  )  ) {  
    if (latest.tx$vout[[j]]$scriptPubKey$type == "nulldata") {
      addresses[j] <- "DELETE"
      value[j] <- NA
      # These are generally OP_RETURN outputs
      next
    }
    extracted.address <- latest.tx$vout[[j]]$scriptPubKey$addresses
    stopifnot(length(extracted.address) == 1)
    stopifnot(length(extracted.address[[1]]) == 1)
    addresses[j] <- extracted.address[[1]]
    value[j] <- latest.tx$vout[[j]]$value
  }
  
  addresses <- addresses[addresses != "DELETE"]
  value <- value[! is.na(value)]
  
  
  rbind(graph.edgelist,
    data.frame(source = latest.tx$txid, target = addresses, value = value, 
      block.height = block.height, fusion = fusion)
  )
  
}





fused.txids <- fused.all.df$txid

fused.edgelists <- future.apply::future_lapply( fused.txids, function(fused.txid.iter) {
  
  zero.level <- get.single.tx.graph(bch.config, fused.txid.iter, fusion = TRUE)
  
  backward.tx.spider <- unique(zero.level$source[ ! grepl("^bitcoincash", zero.level$source)])
  fused.txids.backward <- intersect(backward.tx.spider, fused.txids)
  backward.tx.spider <- setdiff(backward.tx.spider, fused.txids.backward)
  
  first.level.parent <- vector("list", length(backward.tx.spider))
  
  for (i in backward.tx.spider) {
    first.level.parent[[i]] <- get.single.tx.graph(bch.config, i, fusion = FALSE)
  }
  
  if(length(first.level.parent) > 0) {
    first.level.parent <- do.call(rbind, first.level.parent)
  } else {
    first.level.parent <- zero.level[FALSE, ]
  }
  
  row.names(first.level.parent) <- NULL
  
  list(first.level.parent = 
      list(edgelist = first.level.parent, fusions = fused.txids.backward),
    zero.level = zero.level)
  
} )



names(fused.edgelists) <- fused.txids

for ( i in names(fused.edgelists)) {
  
  fusions.to.incorporate <- fused.edgelists[fused.edgelists[[i]]$first.level.parent$fusions]
  
  for (j in names(fusions.to.incorporate)) {
    fusions.to.incorporate[[j]] <- fusions.to.incorporate[[j]]$zero.level
  }
  
  fusions.to.incorporate <- do.call(rbind, fusions.to.incorporate)
  row.names(fusions.to.incorporate) <- NULL
  
  fused.edgelists[[i]]$first.level.parent$edgelist <- rbind(
    fused.edgelists[[i]]$first.level.parent$edgelist, fusions.to.incorporate
  )
  
}



fusion.child.records <- future.apply::future_lapply( first.fusion.height:current.block.height, function(iter.block.height) {
  
  if (iter.block.height %% 1000 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbch::getblockhash(bch.config, iter.block.height)
  block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l2")
  
  raw.txs.ls <- block.data@result$tx
  
  fusion.child.records <- lapply(raw.txs.ls, FUN = function(x) {

    vin.txids <- vector("character", length(x$vin) )
    
    for (i in seq_along(x$vin)) {
      vin.txids[[i]] <- x$vin[[i]]$txid
    }
    
    if ( any(vin.txids %in% fused.txids)) {
      if (x$txid %in% fused.txids) { 
        return(list(fusion.record.insertion = intersect(vin.txids, fused.txids),
          fusions = x$txid))
      } else {
        return(list(fusion.record.insertion = intersect(vin.txids, fused.txids),
          edgelist = get.single.tx.graph(bch.config, x$txid, fusion = FALSE)))
      }
    } else {
      return (NULL)
    }
  })
  
  fusion.child.records
      
} )


first.level.child.empty <- fused.edgelists[[1]]$zero.level[FALSE, ]

for ( i in seq_along(fused.edgelists)) {
  fused.edgelists[[i]]$first.level.child <-
    list(edgelist = first.level.child.empty, fusions = NULL)
}


for ( i in seq_along(fusion.child.records)) {
  
  if (is.null(fusion.child.records[[i]])) {next}
  
  for (j in seq_along(fusion.child.records[[i]])) {
    
    for (fusion.record.insertion in fusion.child.records[[i]][[j]]$fusion.record.insertion) {
      
      obj.to.modify <- fused.edgelists[[ fusion.record.insertion ]]$first.level.child
      obj.to.insert <- fusion.child.records[[i]][[j]]

      if (names(obj.to.modify) %in% "fusions") {
        fused.edgelists[[ fusion.record.insertion ]]$first.level.child$fusions <-
          c(obj.to.modify$fusions, obj.to.insert$fusions)
      }
      if (names(obj.to.modify) %in% "edgelist") {
        fused.edgelists[[ fusion.record.insertion ]]$first.level.child$edgelist <-
          rbind(obj.to.modify$edgelist, obj.to.insert$edgelist)
      }
    }
  }
  
  fusions.to.incorporate <- fused.edgelists[fused.edgelists[[i]]$first.level.parent$fusions]
  
  for (j in names(fusions.to.incorporate)) {
    fusions.to.incorporate[[j]] <- fusions.to.incorporate[[j]]$zero.level
  }
  
  fusions.to.incorporate <- do.call(rbind, fusions.to.incorporate)
  row.names(fusions.to.incorporate) <- NULL
  
  fused.edgelists[[i]]$first.level.parent$edgelist <- rbind(
    fused.edgelists[[i]]$first.level.parent$edgelist, fusions.to.incorporate
  )
  
}







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
str(fused.update.df)
if (length(fused.update.df) > 0) {
  fusions.df <- unique(rbind(fusions.df, fused.update.df))
}

saveRDS(fusions.df, file = paste0(fusion.raw.data.dir, "fusions_df_original_height_", 
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

saveRDS(fusions.date.agg, file = paste0(fusion.raw.data.dir, "fusions_date_agg_height_", 
  max(fusions.df$block.height), ".rds"), compress = FALSE)
write.csv(fusions.date.agg, file = paste0(fusion.raw.data.dir, "fusions_date_agg_height_", 
  max(fusions.df$block.height), ".csv"), row.names = FALSE)
saveRDS(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.rds"), compress = FALSE)
write.csv(fusions.date.agg, file = paste0(fusion.polished.data.dir, "fusions_date_agg.csv"), row.names = FALSE)

fusions.summary.ls <- list(
  n.fusions = nrow(fusions.df), 
  n.bch = sum(fusions.df$value, na.rm = TRUE),
  full.release = as.POSIXct("2020-07-30", tz = "GMT")
  # https://github.com/Electron-Cash/Electron-Cash/releases/tag/4.1.0
)

saveRDS(fusions.summary.ls, file = paste0(fusion.polished.data.dir, "fusions_summary_ls.rds"), compress = FALSE)

# NOTE: readRDS() with compress = FALSE loads twice as fast as with compress = TRUE. 0.25 secs vs 0.5 secs

#saveRDS(fusions.date.agg, file = "data/fusions_date_agg.rds", compress = FALSE)
#write.csv(fusions.date.agg, file = "data/fusions_date_agg.csv", row.names = FALSE)

# Store the data frame to disk
# fst::write.fst(fusions.df, "dataset.fst", compress = 50)

# Retrieve the data frame again
# system.time(test <- fst::read.fst("dataset.fst"))


# https://bitcoin.stackexchange.com/questions/41749/get-transaction-fees-per-transaction-via-gettransaction?rq=1
# "Unfortunately, you'll have to look up all the inputs and see how much is in them in order to calculate the transaction fee using this method."
