% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monero.R
\name{getBlockTemplate}
\alias{getBlockTemplate}
\title{getBlockTemplate}
\usage{
getBlockTemplate(wallet.address, reserve.size = 60,
  ip = getOption("monerod.ip", "127.0.0.1"),
  port = getOption("monerod.port", 18081))
}
\arguments{
\item{wallet.address}{Address of wallet to receive coinbase transactions
if block is successfully mined.}

\item{reserve.size}{integer reserve size.}

\item{ip}{daemon ip address}

\item{port}{daemon port}
}
\value{
\itemize{
 \item blocktemplate_blob - chr; Blob on which to try to mine a new block.
 \item difficulty - int; Difficulty of next block.
 \item height - int; Height on which to mine.
 \item prev_hash - chr; Hash of the most recent block on which to mine the next block.
 \item reserved_offset - int; Reserved offset
 \item status - chr; General RPC error code. "OK" means everything looks good.
}
}
\description{
get block template
}
\examples{
\dontrun{
x="44GBHzv6ZyQdJkjqZje6KLZ3xSyN1hBSFAnLP6EAqJtCRVzMzZmeXTC2AHKDS9aEDTRKmo6a6o9r9j86pYfhCWDkKjbtcns"
getBlockTemplate(x)
}
}
\references{
\url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getblocktemplate}
}
\author{
Garrett See
}
