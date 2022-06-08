library(data.table)
library(rbch)

bitcoin.conf.file <- ""
# Input filepath for your bitcoin.conf file

data.dir <- "data-raw/"
# Input data directory here, with trailing "/"

bch.config <- rbch::conrpc(bitcoin.conf.file)

initial.block.height <- 610700
# This is a block found on Nov 26, 2019. Fusions started Nov 28, 2019.
# Giving a little time buffer to be certain all fusions are captured.

current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks

block.times <- vector(length(initial.block.height:current.block.height), mode ="list")

for (iter.block.height in initial.block.height:current.block.height) {
  
  if (iter.block.height %% 1000 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbch::getblockhash(bch.config, iter.block.height)
  block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l1")
  block.times[[iter.block.height - initial.block.height + 1]] <- 
    data.frame(block_height = iter.block.height, block_time = block.data@result$time)
}

block.times <- data.table::rbindlist(block.times)

saveRDS(block.times, file = paste0(data.dir, "block_times.rds"))



