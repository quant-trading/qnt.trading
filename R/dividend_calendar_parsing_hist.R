#install.packages('XML')
#install.packages('RPostgreSQL')

y = tryCatch({

  require(XML)

  Sys.setlocale("LC_ALL", 'russian')
  
  for(i in 2001:2014) {
    y <- htmlParse(paste("http://bcs-express.ru/kalendari-investora/dividends/",i),encoding="utf-8")
  
    recs = getNodeSet(y, "//*[@id='con_tab3']//*[@class='clearfix']")
  
    company <- sapply(recs, xpathSApply, ".//*[@class='divids_item_name item_header']", xmlValue)
    div_size <- sapply(recs, xpathSApply, ".//*[@class='divids_item_size item_content']", xmlValue)
  
  
    stock_price <- sapply(recs, xpathSApply, ".//*[@class='divids_item_price']", xmlValue)
    return_rate <- sapply(sapply(recs, xpathSApply, ".//*[@class='divids_item_dohod item_content']", xmlValue), function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
    last_trading_date <- sapply(recs, xpathSApply, ".//*[@class='divids_item_sobran item_content']", xmlValue)
    close_registry_date <- sapply(recs, xpathSApply, ".//*[@class='divids_item_zakritie item_content']", xmlValue)
    upload_dt = rep(Sys.Date(),length(company))
    data = data.frame(cbind(upload_dt, company, div_size, stock_price, return_rate, last_trading_date, close_registry_date))
    
    data$upload_dt = rep(Sys.Date(),length(company))
    data$last_trading_date = as.Date(data$last_trading_date, "%d.%m.%Y")
    data$close_registry_date = as.Date(data$close_registry_date, "%d.%m.%Y")
    
    
    names(data) = c("upload_dt","div_event","div_size","stock_price","return_rate","last_trading_date","close_registry_date")
    
    # DB routine
    require("RPostgreSQL")
    
    # create a connection
    # save the password that we can "hide" it as best as we can by collapsing it
    pw <- {
      "kalinovmost19842006"
    }
    
    # loads the PostgreSQL driver
    drv <- dbDriver("PostgreSQL")
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- dbConnect(drv, dbname = "postgres",
                     host = "localhost", port = 5432,
                     user = "postgres", password = pw)
    rm(pw) # removes the password
    
    dbWriteTable(con, "dividends_bks", value = data, append = TRUE, row.names = FALSE)
  
  
    # close the connection
    dbDisconnect(con)
    dbUnloadDriver(drv)
  }
})