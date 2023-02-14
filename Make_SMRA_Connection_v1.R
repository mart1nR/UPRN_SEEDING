#Create a credentials file as required above: first time a user runs
#User<-toupper(Sys.info()['user'])
#password<-""
#credentials <- data.frame(Username = User,Password = password)
#write.csv(credentials, paste0("~/","User","_credentials.csv"))
credentials <- read.csv(paste0("~/","User","_credentials.csv"))


con <- dbConnect(odbc(), dsn = "SMRA",
              uid = paste0(credentials$Username),
             pwd = paste0(credentials$Password),
               port = "1527",
                 host = "nssstats01.csa.scot.nhs.uk",                 
             SVC = "SMRA.nss.scot.nhs.uk")

