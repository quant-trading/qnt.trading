source("TaxLot.R")
source("Transaction.R")

HifoStack <- R6Class("HifoStack",
                     public = list(
                       initialize = function() {
                         
                       },
                       
                       pop = function(t) {
                         
                         # HIFO
                         qty <- t$qty.close
                         
                         while(qty > 0.00000001) {
                           
                           # find highest open price
                           if(length(private$tax.lots) == 0) break
                           
                           max.price = private$tax.lots[[1]]$get_open_price()
                           index <- 1
                           
                           for(i in 1:length(private$tax.lots))
                             if(private$tax.lots[[i]]$get_open_price() > max.price) {
                               max.price <- private$tax.lots[[i]]$get_open_price()
                               index <- i
                             }
                           
                           # process tax lot
                           if(private$tax.lots[[index]]$get_qty() > qty) {
                             private$tax.lots[[index]]$reduce_qty(qty)
                             
                             tr <- Transaction$new(
                               asset = private$tax.lots[[index]]$get_asset(),
                               qty = qty,
                               open.price = private$tax.lots[[index]]$get_open_price(),
                               open.date = private$tax.lots[[index]]$get_open_date(),
                               close.price = t$proceeds / t$qty.close,
                               close.date = t$date
                             )
                             
                             qty <- 0
                           } else {
                             
                             tr <- Transaction$new(
                               asset = private$tax.lots[[index]]$get_asset(),
                               qty = private$tax.lots[[index]]$get_qty(),
                               open.price = private$tax.lots[[index]]$get_open_price(),
                               open.date = private$tax.lots[[index]]$get_open_date(),
                               close.price = t$proceeds / t$qty.close,
                               close.date = t$date
                             )
                             
                             qty <- qty - private$tax.lots[[index]]$get_qty()
                             private$tax.lots[[index]] <- NULL
                           }
                           
                           private$transactions[[length(private$transactions)+1]] <- tr
                         }
                       },
                       
                       get_wash_report = function() {
                         
                         report <- c()
                         
                         if(length(private$transactions) > 0) {
                           for(i in 1:length(private$transactions)) {
                             if(private$transactions[[i]]$is_potential_wash_sale())
                               report <- c(report, paste("Potential Wash:", private$transactions[[i]]$asset, private$transactions[[i]]$close.date, "=>",private$transactions[[i]]$close.date+31))
                           }
                         }
                         if(!is.null(report))
                           return(unique(report))
                       },
                       
                       push = function(t) {
                         private$tax.lots[[length(private$tax.lots)+1]] <- TaxLot$new(
                           asset = t$asset.open,
                           qty = t$qty.open,
                           open.date = t$date,
                           cost.basis = t$cost.basis
                         )
                       },
                       
                       get_balance = function(t) {
                         if(length(private$tax.lots) == 0) return(0.)
                         
                         balance <- 0
                         for(i in 1:length(private$tax.lots))
                           balance <- balance + private$tax.lots[[i]]$get_qty()
                         
                         balance
                       },
                       
                       get_open_lots_report = function(price) {
                         #print("there")
                         report <- NULL
                         
                         if(length(private$tax.lots) == 0) return(report)
                         
                         for(i in 1:length(private$tax.lots))
                           report <- rbind(report,
                                           private$tax.lots[[i]]$get_info(price)
                           )
                         report
                       },
                       
                       get_loss_report = function(price, fee = 0.004){
                         raw.data <- self$get_open_lots_report(price)
                         
                         if(is.null(raw.data)) return(NULL)
                         
                         raw.data <- subset(raw.data, gnl < 0)
                         
                         data.frame(
                           total.losses = round(sum(raw.data$gnl),2),
                           qty = sum(raw.data$qty),
                           mv = round(sum(raw.data$current.mv),2),
                           tax.efficiency = round((abs(sum(raw.data$gnl*raw.data$tax.factor))*(1 - 2*fee)) / sum(raw.data$current.mv)*100,2)
                         )
                       },
                       
                       get_pnl_report = function(price){
                         raw.data <- self$get_open_lots_report(price)
                         
                         if(is.null(raw.data)) return(NULL)
                         
                         #raw.data <- subset(raw.data, gnl < 0)
                         
                         data.frame(
                           total.pnl = round(sum(raw.data$gnl),2),
                           qty = sum(raw.data$qty),
                           mv = round(sum(raw.data$current.mv),2),
                           pnl.pct = round(sum(raw.data$gnl) / sum(raw.data$current.mv)*100,2)
                         )
                       }
                     ),
                     
                     
                     private = list(
                       tax.lots = list(),
                       transactions = list()
                     )
                     
)