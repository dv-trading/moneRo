
monerod <- function(method="",
                    params=list(),
                    interface=getOption("monerod.interface", "json_rpc"),
                    ip=getOption("monerod.ip", "127.0.0.1"),
                    port=getOption("monerod.port", 18081)) {
  opsci <- options(scipen=1000)
  on.exit(options(opsci))
  res <- getURLContent(url=sprintf("http://%s:%s/%s", ip, port, interface),
                       curl=getCurlHandle(useragent="R"),
                       httpheader=list('Content-type'='application/json'),
                       postfields=toJSON(list(jsonrpc="2.0", id="0",
                                              method=method, params=I(params)),
                                         collapse="", digits=10))
  fromJSON(res)
}

#' getBlockCount
#'
#' Look up how many blocks are in the longest chain known to the node.
#'
#' @return integer number of blocks in longest chain seen by the node.
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getblockcount}
#' @param ip daemon ip address
#' @param port daemon port
#' @return numeric
#' @author Garrett See
#' @examples
#' \dontrun{
#' getBlockCount()
#' }
#' @export
getBlockCount <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                          port=getOption("monerod.port", 18081)) {
  res <- monerod(method="getblockcount", params=list(), interface="json_rpc",
                 ip=ip, port=port)
  if (res$result$status != "OK") return(res)
  res$result$count
}


#' onGetBlockHash
#'
#' Look up a block's hash by its height.
#' @param height integer
#' @param ip daemon ip address
#' @param port daemon port
#' @return character block hash
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#ongetblockhash}
#' @examples
#' \dontrun{
#' onGetBlockHash(height=912345)
#' }
#' @export
onGetBlockHash <- function(height,
                           ip=getOption("monerod.ip", "127.0.0.1"),
                           port=getOption("monerod.port", 18081)) {
  if (missing(height)) stop("Please provide a height.")
  res <- monerod(method="on_getblockhash", params=height, interface="json_rpc",
                 ip=ip, port=port)
  res[['result']]
}

#' getBlockTemplate
#'
#' get block template
#' @param wallet.address Address of wallet to receive coinbase transactions
#'  if block is successfully mined.
#' @param reserve.size integer reserve size.
#' @param ip daemon ip address
#' @param port daemon port
#' @return
#' \itemize{
#'  \item blocktemplate_blob - chr; Blob on which to try to mine a new block.
#'  \item difficulty - int; Difficulty of next block.
#'  \item height - int; Height on which to mine.
#'  \item prev_hash - chr; Hash of the most recent block on which to mine the next block.
#'  \item reserved_offset - int; Reserved offset
#'  \item status - chr; General RPC error code. "OK" means everything looks good.
#' }
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getblocktemplate}
#' @examples
#' \dontrun{
#' x="44GBHzv6ZyQdJkjqZje6KLZ3xSyN1hBSFAnLP6EAqJtCRVzMzZmeXTC2AHKDS9aEDTRKmo6a6o9r9j86pYfhCWDkKjbtcns"
#' getBlockTemplate(x)
#' }
#' @export
getBlockTemplate <- function(wallet.address, reserve.size=60,
                             ip=getOption("monerod.ip", "127.0.0.1"),
                             port=getOption("monerod.port", 18081)) {
  if (missing(wallet.address)) stop("Please provide a height.")
  res <- monerod(method="getblocktemplate",
                 params=list(wallet_address=wallet.address,
                 reserve_size=reserve.size), interface="json_rpc", ip=ip,
                 port=port)
  res[["result"]]
}

#' getBlock
#'
#' Full block information can be retrieved by either block height or hash. 
#'
#' Provide either the \code{height} parameter or \code{hash} parameter, but
#' not both.
#'
#' @param height int; the block's height.
#' @param hash chr; the block's hash.
#' @param ip daemon ip address
#' @param port daemon port
#' @return
#' \itemize{
#'  \item blob - string; Hexadecimal blob of block information.
#'  \item block_header - A structure containing block header information. See getlastblockheader.
#'   \itemize{
#'     \item block_size
#'     \item depth
#'     \item difficulty
#'     \item hash
#'     \item height
#'     \item major_version
#'     \item minero_version
#'     \item nonce
#'     \item num_txes
#'     \item orphan_status
#'     \item prev_hash
#'     \item reward
#'     \item timestamp
#'   }
#'  \item json - json string; JSON formatted block details:
#'   \itemize{
#'   \item major_version - Same as in block header.
#'   \item minor_version - Same as in block header.
#'   \item timestamp - Same as in block header.
#'   \item prev_id - Same as prev_hash in block header.
#'   \item nonce - Same as in block header.
#'   \item miner_tx - Miner transaction information
#'     \itemize{
#'        \item version - Transaction version number.
#'        \item unlock_time - The block height when the coinbase transaction becomes spendable.
#'        \item vin - List of transaction inputs:
#'         \itemize{
#'          \item gen - Miner txs are coinbase txs, or "gen".  height - This block height, a.k.a. when the coinbase is generated.
#'         }
#'        \item vout - List of transaction outputs. Each output contains:
#'         \itemize{
#'           \item amount - The amount of the output, in atomic units.
#'           \item target -
#'           \item key -
#'         }
#'        \item extra - Usually called the "transaction ID" but can be used to include any random 32 byte/64 character hex string.
#'        \item signatures - Contain signatures of tx signers. Coinbased txs do not have signatures.
#'      }
#'    \item tx_hashes - List of hashes of non-coinbase transactions in the block. If there are no other transactions, this will be an empty list.
#'   }
#'   \item status - string; General RPC error code. "OK" means everything looks good.
#' }
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getblock}
#' @examples
#' # Look up by height:
#' # In the following example, block 912345 is looked up by its height.
#' # Note that block 912345 does not have any non-coinbase transactions.
#' # (See the next example for a block with extra transactions):
#' \dontrun{
#' getBlock(912345)
#' }
#'
#' # Look up by hash:
#' # In the following example, block 993056 is looked up by its hash. Note that
#' # block 993056 has 3 non-coinbase transactions:
#' \dontrun{
#'  getBlock(hash="510ee3c4e14330a7b96e883c323a60ebd1b5556ac1262d0bc03c24a3b785516f")
#' }
#' @export
getBlock <- function(height, hash, ip=getOption("monerod.ip", "127.0.0.1"),
                     port=getOption("monerod.port", 18081)) {
  params <- if (missing(height)) {
    list(hash=hash)
  } else list(height=as.integer(height))
  monerod(method="getblock", params=params, interface="json_rpc",
          ip=ip, port=port)$result
}

#' getConnections
#'
#' Retrieve information about incoming and outgoing connections to your node.
#'
#' You may need to `Sys.setlocale('LC_ALL','C')` before running this function
#' because often the data returned contains a string that is not valid in all
#' locales.
#'
#' @param ip daemon ip address
#' @param port daemon port
#' @return
#' connections - List of all connections and their info
#' \itemize{
#'  \item avg_download unsigned int; Average bytes of data downloaded by node.
#'  \item avg_upload unsigned int; Average bytes of data uploaded by node.
#'  \item current_download unsigned int; Current bytes downloaded by node.
#'  \item current_upload unsigned int; Current bytes uploaded by node.
#'  \item incoming boolean; Is the node getting information from your node?
#'  \item ip string; The node's IP address.
#'  \item live_time unsigned int
#'  \item local_ip boolean
#'  \item localhost boolean
#'  \item peer_id string; The node's ID on the network.
#'  \item port stringl The port that the node is using to connect to the network.
#'  \item recv_count unsigned int
#'  \item recv_idle_time unsigned int
#'  \item send_count unsigned int
#'  \item send_idle_time unsigned int
#'  \item state string
#' }
#'
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getconnections}
#' @export
getConnections <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                           port=getOption("monerod.port", 18081)) {
  res <- try(suppressWarnings(monerod(method="get_connections", ip=ip, 
                                      port=port)$result), silent=TRUE)
  if (inherits(res, "try-error")) {
    stop("Try running Sys.setlocale('LC_ALL','C') first and then try again.")
  }
  res
}

#' getInfo
#'
#' Retrieve general information about the state of your node and the network.
#'
#' @param ip daemon ip address
#' @param port daemon port
#' @return
#' \itemize{
#'  \item alt_blocks_count - unsigned int; Number of alternative blocks to main chain.
#'  \item difficulty - unsigned int; Network difficulty (analogous to the strength of the network)
#'  \item grey_peerlist_size - unsigned int; Grey Peerlist Size
#'  \item height - unsigned int; Current length of longest chain known to daemon.
#'  \item incoming_connections_count - unsigned int; Number of peers connected to and pulling from your node.
#'  \item outgoing_connections_count - unsigned int; Number of peers that you are connected to and getting information from.
#'  \item status - string; General RPC error code. "OK" means everything looks good.
#'  \item target - unsigned int; Current target for next proof of work.
#'  \item target_height - unsigned int; The height of the next block in the chain.
#'  \item testnet - boolean; States if the node is on the testnet (true) or mainnet (false).
#'  \item top_block_hash - string; Hash of the highest block in the chain.
#'  \item tx_count - unsigned int; Total number of non-coinbase transaction in the chain.
#'  \item tx_pool_siz - unsigned int; Number of transactions that have been broadcast but not included in a block.
#'  \item white_peerlist_size - unsigned int; White Peerlist Size
#' }
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getinfo}
#' @export
getInfo <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                    port=getOption("monerod.port", 18081)) {
  monerod(method="get_info", ip=ip, port=port)$result
}

#' hardForkInfo
#'
#' hard fork info
#'
#' Look up information regarding hard fork voting and readiness.
#' \itemize{
#'  \item earliest_height - unsigned int; Block height at which hard fork would be enabled if voted in.
#'  \item enabled - boolean; Tells if hard fork is enforced.
#'  \item state - unsigned int; Current hard fork state: 0 (There is likely a hard fork), 1 (An update is needed to fork properly), or 2 (Everything looks good).
#'  \item status - string; General RPC error code. "OK" means everything looks good.
#'  \item threshold - unsigned int; Minimum percent of votes to trigger hard fork. Default is 80.
#'  \item version - unsigned int; The major block version for the fork.
#'  \item votes - unsigned int; Number of votes towards hard fork.
#'  \item voting - unsigned int; Hard fork voting status.
#'  \item window - unsigned int; Number of blocks over which current votes are cast. Default is 10080 blocks.
#'}
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#hardforkinfo}
#' @param ip daemon ip address
#' @param port daemon port
#' @author Garrett See
#' @examples
#' \dontrun{
#' hardForkInfo()
#' }
#' @export
hardForkInfo <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                         port=getOption("monerod.port", 18081)) {
  monerod(method="hard_fork_info", ip=ip, port=port)$result
}


# setBans
#
# Ban another node by IP.
# @param bans A list of nodes to ban:
# @param ip unsigned int; IP address to ban, in Int format.
# @param ban boolean; Set true to ban.
# @param seconds - unsigned int; Number of seconds to ban node.
# @return
# status - string; General RPC error code. "OK" means everything looks good.
# @references \url{}
# @examples
# \dontrun{
# setBans(bans=list(ip=838969536, ban=TRUE, seconds=30))
# }
#THIS FUNCTION DOES NOT WORK
#setBans <- function(ip, ban=TRUE, seconds) {
#  params <- RJSONIO::toJSON(list(bans=list(ip=ip, ban=ban, seconds=seconds)), collapse="", digits=10)
#  monerod(method="setbans", params=params)
#}

##{"bans":[{"ip":838969536,"ban":true,"seconds":30}]}}' -H  'Content-Type: application/json'

# getBans
#
# @return list of banned nodes
# @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getbans}
# @examples
# \dontrun{
# getBans()
# }
# THIS FUNCTION DOES NOT WORK -- Method not found
#getBans <- function() {
#  monerod(method="getbans")
#}


#' getHeight
#'
#' Get the node's current height
#'
#' @param ip daemon ip address
#' @param port daemon port
#' @return height unsigned int; Current length of th elongest chain known to
#' the daemon.
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getheight}
#' @examples
#' \dontrun{
#' getHeight()
#' }
#' @export
#' @rdname getHeight.monerod
getHeight.monerod <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                      port=getOption("monerod.port", 18081)) {
  monerod(method="", interface="getheight", ip=ip, port=port)$height
}

#' @export
#' @rdname getHeight.monerod
getHeight <- getHeight.monerod

getTransactions <- function(txs.hashes=list(), decode.as.json=TRUE,
                            ip=getOption("monerod.ip", "127.0.0.1"),
                            port=getOption("monerod.port", 18081)) {
  stop("not yet implemented")

  monerod(method="",
  params=list(txs_hashes=txs.hashes,
              decode_as_json=decode.as.json),
  interface="gettransactions", ip=ip, port=port)

}


#https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#iskeyimagespent
#https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#sendrawtransaction
#https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#gettransactionpool
#https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#stopdaemon


#curl -X POST http://127.0.0.1:18081/get_transaction_pool -H 'Content-Type: application/json'

#' getTransactionPool
#' 
#' @param ip daemon ip address
#' @param port daemon port
#' @return
#' \itemize{
#'   \item spent_key_images - List of spent output key images:
#'     \itemize{
#'       \item id_hash - string; Key image ID hash.
#'       \item txs_hashes - string list; Key image transaction hashes.
#'     }
#'   \item status - string; General RPC error code. "OK" means everything looks good.
#'   \item transactions - List of transactions in the mempool that have not been included in a block:
#'     \itemize{
#'       \item blob_size - unsigned int; The size of the full transaction blob.
#'       \item fee - unsigned int; The amount of the mining fee included in the transaction, in atomic units.
#'       \item id_hash - string; The transaction ID hash.
#'       \item kept_by_block - boolean; We do not accept transactions that timed out before, unless set true.
#'       \item last_failed_height - unsigned int; If the transaction has previously timed out, this tells at what height that occured.
#'       \item last_failed_id_hash - string; Like the previous, this tells the previous transaction ID hash.
#'       \item max_used_block_height - unsigned int; Tells the height of the most recent block with an output used in this transaction.
#'       \item max_used_block_hash - string; Tells the hash of the most recent block with an output used in this transaction.
#'       \item receive_time - unsigned int; The Unix time that the transaction was first seen on the network by the node.
#'       \item tx_json - json string; JSON structure of all information in the transaction:
#'         \itemize{
#'           \item version - Transaction version
#'           \item unlock_time - If not 0, this tells when a transaction output is spendable.
#'           \item vin - List of inputs into transaction:
#'             \itemize{
#'               \item key - The public key of the previous output spent in this transaction.
#'               \item amount - The amount of the input, in atomic units.
#'               \item key_offsets - A list of integer offets to the input.
#'               \item k_image - The key image for the given input
#'             }
#'           \item vout - List of outputs from transaction:
#'           \itemize{
#'              \item amount - Amount of transaction output, in atomic units.
#'              \item target - Output destination information:
#'              \item key - The stealth public key of the receiver. Whoever owns the private key associated with this key controls this transaction output.
#'          }
#'          \item extra - Usually called the "transaction ID" but can be used to include any random 32 bytes.
#'          \item signatures - List of ignatures used in ring signature to hide the true origin of the transaction.
#'        }
#'      }
#'  }
#' @author Garrett See
#' @export
getTransactionPool <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                               port=getOption("monerod.port", "18081")) {
  monerod(method="", interface="get_transaction_pool", ip=ip, port=port)
}


#submitblock
#getlastblockheader
#getblockheaderbyhash
#getblockheaderbyheight


#' getLastBlockHeader
#'
#' get block header of the most recent block
#'
#' @param ip daemon ip address
#' @param port daemon port
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getlastblockheader}
#' @examples
#' \dontrun{
#' getLastBlockHeader()
#' }
#' @export
getLastBlockHeader <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                               port=getOption("monerod.port", "18081")) {
  monerod(method="getlastblockheader", interface="json_rpc", ip=ip,
          port=port)$result$block_header
}


#' getBlockHeaderByHeight
#'
#' Similar to \code{getBlockHeaderByHash}, this method includes a
#' block's height as an input parameter to retrieve basic information about
#' the block.
#'
#' @param height unsigned int; The block's height.
#' @param ip daemon ip address
#' @param port daemon port
#' @return block_header - A structure containing block header information.
#'   See \code{getLastBlockHeader}.
#' @author Garrett See
#' @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getblockheaderbyheight}
#' @examples
#' \dontrun{
#' getBlockHeaderByHeight(912345)
#' }
#' @export
getBlockHeaderByHeight <- function(height,
                                   ip=getOption("monerod.ip", "127.0.0.1"),
                                   port=getOption("monerod.port", 18081)) {
  if (missing(height)) stop("Please provide a height.")
    monerod(method="getblockheaderbyheight",
            params=toJSON(list(height=height), digits=10),
            interface="json_rpc", ip=ip, port=port)$result$block_header

}

# getBlockHeaderByHash
#
# Block header information can be retrieved using either a block's hash or
# height. This method includes a block's hash as an input parameter to
# retrieve basic information about the block.
#
# @param hash string; The block's sha256 hash.
# @param ip daemon ip address
# @param port daemon port
# @return block_header A structure containing block header information.
# See \code{\link{getLastBlockHeader}}.
# @references \url{https://getmonero.org/knowledge-base/developer-guides/daemon-rpc#getlastblockheader}
# @examples
# \dontrun{
# getBlockHeaderByHash("e22cf75f39ae720e8b71b3d120a5ac03f0db50bba6379e2850975b4859190bc6")
# }
getBlockHeaderByHash <- function(hash, ip=getOption("monerod.ip", "127.0.0.1"),
                                 port=getOption("monerod.port", 18081)) {
  stop("not yet implemented")
  if (missing(hash)) stop("Please provide a hash.")
  monerod(method="getblockheaderbyhash",
          params=toJSON(list(hash=hash)),
          interface="json_rpc", ip=ip, port=port)$result$block_header
}

#' getNetHash
#'
#' get network hash rate estimate by dividing difficulty by 120
#'
#' @param ip daemon ip address
#' @param port daemon port
#' @return hashes per second of the network.
#' @author Garrett See
#' @examples
#' \dontrun{
#' getNetHash(ip="node.moneroworld.com")
#' }
#' @export
getNetHash <- function(ip=getOption("monerod.ip", "127.0.0.1"),
                       port=getOption("monerod.port", 18081)) {
  #monerod("get_info", ...)$result$difficulty/120
  getInfo(ip=ip, port=port)$difficulty/120
}

#' getLastReward
#'
#' get reward of the most recent block
#'
#' cals \code{getLastBlockHeader} to get the reward in atomic units, then
#' divides by 1e12 to return the reward in XMR.
#'
#' @param ... arguments to pass to \code{getLastBlockHeader}
#' @return numeric amount of XMR that the reward of the most recent block.
#' @author Garrett See
#' @export
getLastReward <- function(...) {
  getLastBlockHeader(...)$reward/1e12
}

