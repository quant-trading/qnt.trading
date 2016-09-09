library(RCurl)
library(XML)

# Download page using RCurl
# You may need to set proxy details, etc.,  in the call to getURL
theurl <- "http://nlu.ru/indicators.htm"
webpage <- getURL(theurl,.encoding = "UTF-8")


# Process escape characters
webpage <- readLines(tc <- textConnection(webpage)); close(tc)


# Parse the html tree, ignoring errors on the page
pagetree <- htmlTreeParse(webpage, error=function(...){})

# Navigate your way through the tree. It may be possible to do this more efficiently using getNodeSet
body <- pagetree$children$html$children$body 
divbodyContent <- body$children$div$children[[1]]$children$div$children[[4]]
tables <- body[names(body)=="table"]


#In this case, the required table is the only one with class "wikitable sortable"  
tableclasses <- sapply(tables, function(x) x$attributes["class"])
thetable  <- tables[2]$table#tables[which(tableclasses=="datagrid")]$table

#parse data

n = 16
k = 8

ids = rep("",n)
dates = rep("",n)
v0 = rep("",n)
v1 = rep("",n)
v2 = rep("",n)
v3 = rep("",n)
v4 = rep("",n)
v5 = rep("",n)
v6 = rep("",n)
v7 = rep("",n)

for(i in seq(1,n)) {
  
  #parse IDs
  txt = toString(thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[1]$td$children$table$children$tr$children$td$children$a)
  
  s1=gregexpr("indId=",txt)[[1]][1]
  s2=gregexpr(" target=",txt)[[1]][1]
  ids[i] = substr(txt,s1+6,s2-2)
  
  if(!ids[i]=="") {
    
    #parse date
    txt = thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[2]$td$children$text$value[1]
    #print()
    dates[i] = txt
    v0[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[3]$td$children$text$value[1])
    v1[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[4]$td$children$span$children$text$value[1])
    v2[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[5]$td$children$span$children$text$value[1])
    v3[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[6]$td$children$span$children$text$value[1])
    v4[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[7]$td$children$span$children$text$value[1])
    v5[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[8]$td$children$span$children$text$value[1])
    v6[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[9]$td$children$span$children$text$value[1])
    v7[i] = max("",thetable$children$tr$children[4]$td$children$table$children[3+i]$tr$children[10]$td$children$span$children$text$value[1])
  }
}

upload_dt = rep(Sys.Date(),length(ids))
res = cbind(upload_dt,ids,dates,v0, v1,v2,v3,v4,v5,v6,v7)
res = subset(res, ids != "")

x = data.frame(res)

x$upload_dt = rep(Sys.Date(),length(x$ids))
x$dates = as.Date(x$dates, "%d.%m.%Y")
x$v0 = sapply(x$v0, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v1 = sapply(x$v1, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v2 = sapply(x$v2, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v3 = sapply(x$v3, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v4 = sapply(x$v4, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v5 = sapply(x$v5, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v6 = sapply(x$v6, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})
x$v7 = sapply(x$v7, function(k) { as.numeric(gsub(" ","",gsub(",",".",k)))})

names(x) = c("ind_id","ind_dt","value","delta_1d","delta_1m","delta_3m","delta_6m","delta_1y","delta_3y","delta_5y","upload_dt")

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

# check for the cartable
dbExistsTable(con, "indicators_nlu_basic")

time_stamp <- dbGetQuery(con, "select max(b.upload_dt) from public.indicators_nlu_basic b")

if(Sys.Date() > time_stamp$max || is.na(time_stamp$max)) {
  dbWriteTable(con, "indicators_nlu_basic", value = x, append = TRUE, row.names = FALSE)
}


# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)
  