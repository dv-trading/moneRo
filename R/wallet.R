
# monero-wallet-rpc requires a username and password, which doesn't work
# with the monerod() function. Since I was having trouble figuring out how
# to pass --digest and -u to RCurl, it was easier to just create a separate
# wallet() function for wallet related functions
wallet <- function(method, userpwd, ip="127.0.0.1", port="18082", 
                   interface="json_rpc") {
  if (length(userpwd) == 0) stop("please provide a user:password")
  d <- sprintf('{"jsonrpc":"2.0","id":"0","method":"%s"}', method)
  H <- 'Content-Type: application/json'
  res <- paste(system(paste0("curl --silent -u ", userpwd, 
                             " --digest -X POST http://", ip, ":", port, "/", 
                             interface, 
                             " -d ", shQuote(d), 
                             " -H ", shQuote(H)), intern=TRUE), 
               collapse="\n")
  fromJSON(res)
}


#' getBalance
#'
#' get the wallet's balance.
#'
#' @param userpwd user and password for monero-wallet-rpc calls. 
#'   (e.g. "username:password")
#' @param ip ip address of wallet
#' @param port port of wallet
#' @author Garrett See
#' @return
#' \itemize{
#'  \item balance unsigned int; The total balance of the current simplewallet in
#'  session.
#'  \item unlocked_balance unsigned int; Unlocked funds are those funds that are
#'  sufficiently deep enough in the Monero blockchain to be considered safe to
#'  spend.
#' }
#' @export
getBalance <- function(userpwd=getOption("monero.wallet.userpwd"),
                       ip=getOption("monero.wallet.ip", "127.0.0.1"),
                       port=getOption("monero.wallet.port", "18082")) {
  
  wallet(method="getbalance", userpwd=userpwd, ip=ip, port=port)$result
}

#getBalance()

#' getAddress
#'
#' get the wallet's address
#'
#' @param userpwd user and password for monero-wallet-rpc calls. 
#'   (e.g. "username:password")
#' @param ip ip address of wallet
#' @param port port of wallet
#' @author Garrett See
#' @return chr address
#' @export
getAddress <- function(userpwd=getOption("monero.wallet.userpwd"),
                       ip=getOption("monero.wallet.ip", "127.0.0.1"),
                       port=getOption("monero.wallet.port", "18082")) {
  wallet(method="getaddress", userpwd=userpwd, ip=ip, port=port)$result
}


#' getHeight
#'
#' get current block height of wallet
#'
#' @param userpwd user and password for monero-wallet-rpc calls. 
#'   (e.g. "username:password")
#' @param ip ip address of wallet
#' @param port port of wallet
#' @author Garrett See
#' @return chr address
#' @export
getHeight.wallet <- function(userpwd=getOption("monero.wallet.userpwd"),
                             ip=getOption("monero.wallet.ip", "127.0.0.1"),
                             port=getOption("monero.wallet.port", "18082")) {
  wallet(method="getheight", userpwd=userpwd, ip=ip, port=port)$result
}

#getHeight()


# destinations - array of destinations to receive XMR:
# amount - unsigned int; Amount to send to each destination, in atomic units.
# address - string; Destination public address.
# fee - unsigned int; Ignored, will be automatically calculated.
# mixin - unsigned int; Number of outpouts from the blockchain to mix with (0 means no mixing).
# unlock_time - unsigned int; Number of blocks before the monero can be spent (0 to not add a lock).
# payment_id - string; (Optional) Random 32-byte/64-character hex string to identify a transaction.
# get_tx_key - boolean; (Optional) Return the transaction key after sending. Outputs:
# tx_hash - array of: string
# tx_key -
# transfer <- function(ip=getOption("monero.wallet.ip", "127.0.0.1"),
#                      port=getOption("monero.wallet.port", "18082")) {
#   moneRo:::monerod(method="getbalance", ip=ip, port=port)
# }

# transfer
# transfer_split
# sweep_dust
# store
# get_payments
# get_bulk_payments
# incoming_transfers
# query_key
# make_integrated_address
# split_integrated_address
# stop_wallet
