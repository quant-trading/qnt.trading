


library(RJSONIO)
library(rjson)
library(RCurl)
appannie <- getURL("https://lk.olb.ru/Trade/Packet", verbose = TRUE)



library(httr)
library(XML)

handle <- handle("https://lk.olb.ru/Trade/Packet") 
path   <- "amember/login.php"

# fields found in the login form.
login <- list(
  amember_login = "username"
  ,amember_pass  = "password"
  ,amember_redirect_url = 
    "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=2"
)

response <- POST(handle = handle, path = path, body = login)





library(mailR)
send.mail(from = "qnt.trading@gmail.com",
          to = c("andreevm@bk.ru"),
          subject = "Test E-mail",
          body = "Report status: OK",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "qnt.trading@gmail.com", passwd = "kalinovmost19842006", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)





