
# options("monerod.ip"="node.moneroworld.com",
#         "monerod.port"=18081,
#         "monerod.interface"="json_rpc")


## try to get an option, and set it if it doesn't exist
getOption("monerod.ip",
          options("monerod.ip"="127.0.0.1"))
getOption("monerod.port",
          options("monerod.port"=18081))
getOption("monerod.interface",
          options("monerod.interface"="json_rpc"))

getOption("monero.wallet.ip",
          options("monero.wallet.ip"="127.0.0.1"))
getOption("monero.wallet.port",
          options("monero.wallet.port"=18082))
# You might also want to set option "monero.wallet.userpwd"
# for example, you start your with --rpc-login="user:password" then you'll want
# to set options(monero.wallet.userpwd="user:password")

