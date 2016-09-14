y = tryCatch({

  
library(XML)
#y <- htmlParse("http://bcs-express.ru/dividednyj-kalendar")
y <- htmlParse("http://bcs-express.ru/kalendari-investora/dividends/2015")

#recs = getNodeSet(y, "//*[@id='con_tab3']//*[@class='calendar_item_header clearfix']")
recs = getNodeSet(y, "//*[@id='con_tab3']//*[@class='clearfix']")

#company <- sapply(recs, xpathSApply, ".//*[@class='divids_item_name item_header2']", xmlValue)
#div_size <- sapply(recs, xpathSApply, ".//*[@class='divids_item_size item_content2']", xmlValue)
company <- sapply(recs, xpathSApply, ".//*[@class='divids_item_name item_header']", xmlValue)
div_size <- sapply(recs, xpathSApply, ".//*[@class='divids_item_size item_content']", xmlValue)


stock_price <- sapply(recs, xpathSApply, ".//*[@class='divids_item_price']", xmlValue)
#return_rate <- sapply(sapply(recs, xpathSApply, ".//*[@class='divids_item_dohod item_content2']", xmlValue), function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
#last_trading_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_sobran item_content2']", xmlValue), "%d.%m.%Y")
#close_registry_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_zakritie item_content2']", xmlValue), "%d.%m.%Y")

#return_rate <- sapply(sapply(recs, xpathSApply, ".//*[@class='divids_item_dohod item_content2']", xmlValue), function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
#last_trading_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_sobran item_content2']", xmlValue), "%d.%m.%Y")
#close_registry_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_zakritie item_content2']", xmlValue), "%d.%m.%Y")

return_rate <- sapply(sapply(recs, xpathSApply, ".//*[@class='divids_item_dohod item_content']", xmlValue), function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
last_trading_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_sobran item_content']", xmlValue), "%d.%m.%Y")
close_registry_date <- as.Date(sapply(recs, xpathSApply, ".//*[@class='divids_item_zakritie item_content']", xmlValue), "%d.%m.%Y")


data = cbind(company, div_size, stock_price, return_rate, last_trading_date, close_registry_date)

})
