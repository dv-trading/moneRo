# moneRo

This package provides an R interface to the monero daemon and wallet RPC APIs.  
Not all RPC calls have been implemented yet, especially on the wallet side.  
Pull requests are welcome.

To download monero or to learn more about it, visit https://getmonero.org.

## Installation

```
# install.packages("devtools")
library(devtools)
install_github("dv-trading/moneRo")
```

## Examples


```
library(moneRo)
# if you do not have a local daemon running, set options to connect
# to a remote node
options(monerod.ip="xmr.dvchain.co")

getInfo()
getNetHash()
formatHashes(getNetHash()) # pretty print

getBlockCount()
getBlock(height=993056)
getBlock(hash="510ee3c4e14330a7b96e883c323a60ebd1b5556ac1262d0bc03c24a3b785516f")


## Assuming you started monero-wallet-rpc like this:
## monero-wallet-rpc --wallet-file="mywallet" --password="walletpassword" \ 
##   --rpc-bind-port=18082 --rpc-login="user:password"

getAddress(userpwd="username:password")
getBalance(userpwd="username:password")
```

<a href="http://www.wtfpl.net/"><img
       src="http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl-badge-4.png"
       width="80" height="15" alt="WTFPL" /></a>
