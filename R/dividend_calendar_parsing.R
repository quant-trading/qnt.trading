
#install.packages('XML')
#install.packages('RPostgreSQL')

y = tryCatch({
  
  require(XML)
  
  Sys.setlocale("LC_ALL", 'russian')
  
    #y <- htmlParse(paste("http://bcs-express.ru/kalendari-investora/dividends/",i),encoding="utf-8")
    y <- htmlParse("http://bcs-express.ru/dividednyj-kalendar")
    recs = getNodeSet(y, "//*[@id='con_tab3']//*[@class='calendar_item_header clearfix']")
    
    company <- sapply(recs, xpathSApply, ".//*[@class='divids_item_name item_header2']", xmlValue)
    div_size <- sapply(recs, xpathSApply, ".//*[@class='divids_item_size item_content2']", xmlValue)
    
    
    stock_price <- sapply(recs, xpathSApply, ".//*[@class='divids_item_price']", xmlValue)
    return_rate <- sapply(sapply(recs, xpathSApply, ".//*[@class='divids_item_dohod item_content2']", xmlValue), function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
    last_trading_date <- sapply(recs, xpathSApply, ".//*[@class='divids_item_sobran item_content2']", xmlValue)
    close_registry_date <- sapply(recs, xpathSApply, ".//*[@class='divids_item_zakritie item_content2']", xmlValue)
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
    ?dbConnect
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- dbConnect(drv, dbname = "postgres",
                     host = "localhost", port = 5432,
                     user = "postgres", password = pw)
    rm(pw) # removes the password
    
    
    #dbWriteTable(con, "dividends_bks", value = data, append = TRUE, row.names = FALSE)
    
    k = 0
    
    for(i in 1:NROW(data$div_event)) {
      
      if(!is.na(data$last_trading_date[i])) {
      curr_rec= dbGetQuery(con, paste("SELECT tt.*
FROM dividends_bks tt
                                      INNER JOIN
                                      (SELECT div_event, MAX(upload_dt) AS max_upload_dt
                                      FROM dividends_bks where last_trading_date = '",data$last_trading_date[i],"'
                                      GROUP BY div_event) groupedtt 
                                      ON tt.div_event = groupedtt.div_event 
                                      AND tt.upload_dt = groupedtt.max_upload_dt
                                      and tt.div_event = '",data$div_event[i],"' and tt.last_trading_date = '",data$last_trading_date[i],"'",sep=""))
      }
      class(curr_rec)
      if(NROW(curr_rec)==0 && !is.na(data$last_trading_date[i]))
      {
        #print("NROW")
        # write new  
        dbWriteTable(con, "dividends_bks", value = data[i,], append = TRUE, row.names = FALSE)
        k = k + 1
      } else {
        #update existing
        #print("Update")
        
        if(curr_rec$upload_dt < Sys.Date() && !is.null(curr_rec$last_trading_date)
           && !is.na(curr_rec$last_trading_date) && curr_rec$last_trading_date >= Sys.Date() &&
           (curr_rec$last_trading_date != data[i,]$last_trading_date || 
            curr_rec$close_registry_date != data[i,]$close_registry_date ||
            curr_rec$div_size != data[i,]$div_size ||
            curr_rec$stock_price != data[i,]$stock_price
            )
           )
          {
          print("COND")
            dbWriteTable(con, "dividends_bks", value = data[i,], append = TRUE, row.names = FALSE)
            k = k + 1
          }
      }
    }
    
    # get upcoming events
    upcoming = dbGetQuery(con, "SELECT tt.div_event, tt.return_rate, tt.last_trading_date
                FROM dividends_bks tt
               INNER JOIN
               (SELECT div_event, MAX(upload_dt) AS max_upload_dt
               FROM dividends_bks where last_trading_date >= CURRENT_DATE
               GROUP BY div_event) groupedtt 
               ON tt.div_event = groupedtt.div_event 
               AND tt.upload_dt = groupedtt.max_upload_dt
               and (tt.last_trading_date > CURRENT_DATE) and (tt.last_trading_date < (CURRENT_DATE + 10 *INTERVAL '1 day'))")
 
    # close the connection
    dbDisconnect(con)
    dbUnloadDriver(drv)
    
    upcoming$div_event = sapply(upcoming$div_event, function(d){return(iconv(d, from = "UTF-8", to = "CP1251"))})  
    iconv(upcoming$div_event, from = "UTF-8", to = "CP1251")
    text = ""
    for(i in 1:NROW(upcoming)){
      text = paste(text,"<br>", upcoming$div_event[i],": ",upcoming$return_rate[i],"% ",upcoming$last_trading_date[i], sep="")
    }
    
    require(mailR)
    send.mail(from = "qnt.trading@gmail.com",
              to = c("qnt.trading@gmail.com"),
              subject = paste("[",k,"] Divident Calendar Update:",toString(Sys.Date())),
              body = toString(data$div_event),
              encoding = "koi8-r",
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "qnt.trading@gmail.com", passwd = "kalinovmost19842006", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    
    send.mail(from = "qnt.trading@gmail.com",
               to = c("andreevm@bk.ru"),
               subject = paste("Upcoming RUS Dividends:",toString(Sys.Date())),
               body = toString(text),
               html = TRUE,
               encoding = "utf-8",
               smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "qnt.trading@gmail.com", passwd = "kalinovmost19842006", ssl = TRUE),
               authenticate = TRUE,
               send = TRUE)

}, error = function(err) {
  library(mailR)
  send.mail(from = "qnt.trading@gmail.com",
            to = c("qnt.trading@gmail.com"),
            subject = paste("(ERR)Divident Calendar Update:",toString(Sys.Date())),
            body = toString(err),
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "qnt.trading@gmail.com", passwd = "kalinovmost19842006", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  return(0)
})





