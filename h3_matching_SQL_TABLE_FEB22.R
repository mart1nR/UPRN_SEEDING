library(dplyr)
library(odbc)
library(h3)
library(stringdist)
library(readr)
library(stringr)
library(tidylog)
#install.packages("textreuse")

# install.packages("remotes")
# remotes::install_github("obrl-soil/h3jsr")
# 
# install.packages("h3r")
# remotes::install_github("crazycapivara/h3-r")
#alternative db connection once ad_base ingested regularly
# con <- dbConnect(odbc(), dsn = "APXP",
#                  uid = "mgsref", #ref_readonly
#                  pwd = "refapp", # aperol
#                  port = "1529",
#                  host = "nssapexpdb01.csa.scot.nhs.uk",
#                  SVC = "APXP.nss.scot.nhs.uk")
# 
# ad_base2a<-dbGetQuery(con, statement ="SELECT * FROM MGSREF.ADDRESSBASE")

# 
library(readr)
ad_base<- read_delim("/chi/(5) Postcode Lookup/UPRN_LOOKUP/Scottish_Address_Data_NHSNSS_EPOCH92_2022-05-03.zip", 
                     "|", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

#test<- data.table::fread("/chi/(5) Postcode Lookup/UPRN_LOOKUP/Scottish_Address_Data_NHSNSS_EPOCH87_2021-10-05/Scottish_Address_Data_NHSNSS_EPOCH87_2021-10-05.csv",sep="|", sep2=" ")

ad_base<-ad_base %>% 
  
  rename(UPRN=X1,PARENT_UPRN=X2,ADDRESS_LINE_1=X3,ADDRESS_LINE_2=X4,ADDRESS_LINE_3=X5,POST_TOWN=X6,POSTCODE=X7,IDENTIFIER=X8,
         LA_LOCATION=X9,START_DATE=X10,END_DATE=X11,X_COORDINATE=X12,Y_COORDINATE=X13,ADDRESS_STRING=X14,LATITUDE=X15,LONGITUDE=X16,
         MIGRATION_DATE=X17,USRN=X18,CLASSIFICATION_LOCATION=X19,CLASSIFICATION_DESCR=X20,CLASSIFICATION_PRIMARY=X21,CLASSIFICATION_SECONDARY=X22,
         CLASSIFICATION_TERTIARY=X23,CLASSIFICATION_QUATERNARY=X24) %>% 
mutate(USRN=as.character(USRN))
coords<-paste0(as.numeric(ad_base$LATITUDE),as.numeric(ad_base$LONGITUDE))

#postcode_urban_rural_2016 <- haven::read_sav("/conf/linkage/output/lookups/Unicode/Geography/Urban Rural Classification/postcode_urban_rural_2016.sav")

ad_base<-ad_base %>%
  #slice(1:10000) %>% 
  #group_by(UPRN) %>% 
  #mutate(COUNT=n()) %>% 
  #filter((COUNT<1)|COUNT==1) %>% 
  distinct(UPRN,.keep_all=T) %>% #up to here
  
  mutate(COUNT=n()) %>% 
  #ungroup() %>% 
  #slice(1:10000) %>% 
  #select(UPRN,ADDRESS_LINE_1,ADDRESS_LINE_2,ADDRESS_LINE_3,POST_TOWN,LATITUDE,LONGITUDE) %>%
  mutate(LAT=as.numeric(LATITUDE)) %>% 
  mutate(LONG=as.numeric(LONGITUDE)) %>% 
  filter(!is.na(LAT))

CO1<-ad_base[, c("LAT", "LONG")]

ad_base<-ad_base %>% 
  mutate(H3_6=h3::geo_to_h3(CO1,res=6)) %>% 
  mutate(H3_7=h3::geo_to_h3(CO1,res=7)) %>%
  mutate(H3_8=h3::geo_to_h3(CO1,res=8)) %>%
  mutate(H3_10=h3::geo_to_h3(CO1,res=10)) %>% 
  mutate(H3_12=h3::geo_to_h3(CO1,res=12)) %>% 
  filter(str_detect(ADDRESS_LINE_1,"^(PROPOSED)")==F,
    CLASSIFICATION_DESCR!="Industrial Applicable to manufacturing, engineering, maintenance, storage / wholesale distribution and extraction sites",
         CLASSIFICATION_DESCR!="Harbour / Port / Dock / Dockyard / Slipway / Landing Stage / Pier / Jetty / Pontoon / Terminal / Berthing / Quay",
        CLASSIFICATION_DESCR!="Agricultural - Applicable to land in farm ownership and not run as a separate business enterprise",
CLASSIFICATION_DESCR!="Airfield / Airstrip / Airport / Air Transport Infrastructure Facility",
CLASSIFICATION_DESCR!="Car / Coach / Commercial Vehicle / Taxi Parking / Park And Ride Site",
CLASSIFICATION_DESCR!="Cemetery / Crematorium / Graveyard. In Current Use.",
CLASSIFICATION_DESCR!="Beach Hut (Recreational, Non-Residential Use Only)",
CLASSIFICATION_DESCR!="Forest / Arboretum / Pinetum (Managed / Unmanaged)",
CLASSIFICATION_DESCR!="Public Household Waste Recycling Centre (HWRC)",
CLASSIFICATION_DESCR!="Amenity - Open areas not attracting visitors",
CLASSIFICATION_DESCR!="Mineral Quarrying / Open Extraction / Active",
CLASSIFICATION_DESCR!="Water / Waste Water / Sewage Treatment Works",
CLASSIFICATION_DESCR!="Pump House / Pumping Station / Water Tower",
CLASSIFICATION_DESCR!="Water / Waste Water / Sewage Treatment Works",
CLASSIFICATION_DESCR!="Historic / Disused Cemetery / Graveyard",
CLASSIFICATION_DESCR!="Water / Waste Water / Sewage Treatment Works",
CLASSIFICATION_DESCR!="Medical / Testing / Research Laboratory",
CLASSIFICATION_DESCR!="Water / Waste Water / Sewage Treatment Works",
CLASSIFICATION_DESCR!="Station / Interchange / Terminal / Halt)",
CLASSIFICATION_DESCR!="Coastguard Rescue / Lookout / Station",
CLASSIFICATION_DESCR!="Mineral / Ore Working / Quarry / Mine",
CLASSIFICATION_DESCR!="Incinerator / Waste Transfer Station",
CLASSIFICATION_DESCR!="Police / Transport Police / Station",
CLASSIFICATION_DESCR!="Public Open Space / Nature Reserve",
CLASSIFICATION_DESCR!="Goods Freight Handling / Terminal",
CLASSIFICATION_DESCR!="Power Station / Energy Production",
CLASSIFICATION_DESCR!="Warehouse / Store / Storage Depot",
CLASSIFICATION_DESCR!="Animal / Bird / Marine Sanctuary",
CLASSIFICATION_DESCR!="Gas / Oil Storage / Distribution",
CLASSIFICATION_DESCR!="Transport Related Infrastructure",
CLASSIFICATION_DESCR!="Electricity Production Facility",
CLASSIFICATION_DESCR!="Automated Teller Machine (ATM)",
CLASSIFICATION_DESCR!="Horse Racing / Breeding Stable",
CLASSIFICATION_DESCR!="Licensed Private Members’ Club",
CLASSIFICATION_DESCR!="Pending Internal Investigation",
CLASSIFICATION_DESCR!="Lock-Up Garage / Garage Court",
CLASSIFICATION_DESCR!="Oil / Gas Extraction / Active",
CLASSIFICATION_DESCR!="Postal Sorting / Distribution",
CLASSIFICATION_DESCR!="Water Distribution / Pumpingraffic Information Signage",
CLASSIFICATION_DESCR!="Water Controlling / Pumping",
CLASSIFICATION_DESCR!="Workshop / Light Industrial",
CLASSIFICATION_DESCR!="Central Government Service",
CLASSIFICATION_DESCR!="Diving / Swimming Facility",
CLASSIFICATION_DESCR!="Emergency / Rescue Service",
CLASSIFICATION_DESCR!="Equestrian Sports Facility",
CLASSIFICATION_DESCR!="Slaughter House / Abattoir",
CLASSIFICATION_DESCR!="Broadcasting (TV / Radio)",
CLASSIFICATION_DESCR!="Greyhound Racing Facility",
CLASSIFICATION_DESCR!="Tenpin Bowling Facility",
CLASSIFICATION_DESCR!="Castle / Historic Ruin",
CLASSIFICATION_DESCR!="Road Freight Transport",
CLASSIFICATION_DESCR!="Vacant / Derelict Land",
CLASSIFICATION_DESCR!="Wholesale Distribution",
CLASSIFICATION_DESCR!="Entertainment Complex",
CLASSIFICATION_DESCR!="Factory/Manufacturing",
CLASSIFICATION_DESCR!="Private Park / Garden",
CLASSIFICATION_DESCR!="Petrol Filling Station",
CLASSIFICATION_DESCR!="Market (Indoor / Outdoor)",
CLASSIFICATION_DESCR!="Racquet Sports Facility",
CLASSIFICATION_DESCR!="Bank / Financial Service",
CLASSIFICATION_DESCR!="Memorial / Market Cross",
CLASSIFICATION_DESCR!="Civilian Firing Facility",
CLASSIFICATION_DESCR!="Electricity Sub-Station",
CLASSIFICATION_DESCR!="Water Quality Monitoring") %>% 
#slice(1:50000) %>% 
  mutate(ADDRESS_LINE_1=gsub(","," ",ADDRESS_LINE_1)) %>%  
  mutate(ADDRESS_LINE_1=gsub("\\(","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=gsub("\\(","",ADDRESS_LINE_1)) %>%#nEW 
  mutate(ADDRESS_LINE_1=gsub("\\."," ",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_1=gsub("'","",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_1=sub("(C/O)+\\s[A-Z]*","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("(C-O)+\\s[A-Z]*","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("(C-0)\\s[A-Z]*","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("(C/0)+\\s[A-Z]*","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("^(C/0)","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("^(C/O)","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("^(C-0)","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("^(C-O)","",ADDRESS_LINE_1)) %>%
  mutate(ADDRESS_LINE_1=sub("  "," ",ADDRESS_LINE_1)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(ADDRESS_LINE_1=as.character(gsub("["]",""))) %>% 
  mutate(ADDRESS_LINE_1=sub("^(FLAT 0-)","0/",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_1=gsub("-","/",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_1=sub("^(FLAT |^F |FLT |FT)","",ADDRESS_LINE_1)) %>% 
  
  mutate(ADDRESS_LINE_2=gsub(","," ",ADDRESS_LINE_2)) %>%  
  mutate(ADDRESS_LINE_2=gsub("\\."," ",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=gsub("'","",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=sub("(C/O)+\\s[A-Z]*","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("(C-O)+\\s[A-Z]*","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("(C-0)\\s[A-Z]*","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("(C/0)+\\s[A-Z]*","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("^(C/0)","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("^(C/O)","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("^(C-0)","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("^(C-O)","",ADDRESS_LINE_2)) %>%
  mutate(ADDRESS_LINE_2=sub("  "," ",ADDRESS_LINE_2)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(ADDRESS_LINE_2=as.character(gsub("["]",""))) %>% 
  mutate(ADDRESS_LINE_2=sub("^(FLAT 0-)","0/",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=gsub("-","/",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=sub("^(FLAT |^F )","",ADDRESS_LINE_2)) %>% 
  
  #Slice(10000:30000) %>% 
  mutate(ADDRESS_STRING_NOPC=case_when(!is.na(ADDRESS_LINE_1)&!is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_3)!=(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_2,", ",ADDRESS_LINE_3,", ",POST_TOWN),
                                                                              !is.na(ADDRESS_LINE_1)&!is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_3)==(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_2,", ",ADDRESS_LINE_3),
                                                                         !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_3)!=(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_3,", ",POST_TOWN),
                                                                    !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_2)==(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_3),
                                  !is.na(ADDRESS_LINE_1)&!is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&is.na(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_2,", ",ADDRESS_LINE_3),
                                  !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&is.na(ADDRESS_LINE_3)&is.na(POST_TOWN)~paste0(ADDRESS_LINE_1),
                                  !is.na(ADDRESS_LINE_1)&!is.na(ADDRESS_LINE_2)&is.na(ADDRESS_LINE_3)&is.na(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_2),
                                                                    !is.na(ADDRESS_LINE_1)&!is.na(ADDRESS_LINE_2)&is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_2)!=(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_2,", ",POST_TOWN),
                                  !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_1)!=(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",POST_TOWN),
                                  !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&is.na(ADDRESS_LINE_3)&!is.na(POST_TOWN)&(ADDRESS_LINE_1)==(POST_TOWN)~ADDRESS_LINE_1,
                                  !is.na(ADDRESS_LINE_1)&is.na(ADDRESS_LINE_2)&!is.na(ADDRESS_LINE_3)&is.na(POST_TOWN)~paste0(ADDRESS_LINE_1,", ",ADDRESS_LINE_3)))%>%
  # mutate(ADDRESS_STRING_NOPC=case_when(!is.na(CURRENT_LINE1)&!is.na(CURRENT_LINE2)&!is.na(CURRENT_LINE3)~paste0(CURRENT_LINE1,", ",CURRENT_LINE2,", ",CURRENT_LINE3),
  #                                      !is.na(CURRENT_LINE1)&is.na(CURRENT_LINE2)&!is.na(CURRENT_LINE3)~paste0(CURRENT_LINE1,", ",CURRENT_LINE3),
  #                                      !is.na(CURRENT_LINE1)&is.na(CURRENT_LINE2)&is.na(CURRENT_LINE3)~paste0(CURRENT_LINE1),
  #                                      !is.na(CURRENT_LINE1)&!is.na(CURRENT_LINE2)&is.na(CURRENT_LINE3)~paste0(CURRENT_LINE1,", ",CURRENT_LINE2))) %>%
    
    mutate(ADDRESS_STRING_NOPC=gsub(","," ",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=gsub("\\."," ",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=gsub("'","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("(C/O)+\\s[A-Z]*","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("(C-O)+\\s[A-Z]*","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("(C-0)\\s[A-Z]*","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("(C/0)+\\s[A-Z]*","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("^(C/0)","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("^(C/O)","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("^(C-0)","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("^(C-O)","",ADDRESS_STRING_NOPC)) %>%
    mutate(ADDRESS_STRING_NOPC=sub("  "," ",ADDRESS_STRING_NOPC)) %>%
    #GET RID OF C/O FOLLOWED BY NAME
    #mutate(ADDRESS_STRING_NOPC=as.character(gsub("["]",""))) %>% 
    mutate(ADDRESS_STRING_NOPC=sub("^(FLAT 0-)","0/",ADDRESS_STRING_NOPC)) %>% 
    mutate(ADDRESS_STRING_NOPC=sub("-","\\/",ADDRESS_STRING_NOPC)) %>% 
    mutate(ADDRESS_STRING_NOPC=sub("^(FLAT |^F )","",ADDRESS_STRING_NOPC)) %>% 
    
    mutate(NUM1=stringr::str_extract(ADDRESS_LINE_1,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
    mutate(NUM2=stringr::str_extract(ADDRESS_LINE_2,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
    mutate(NUM=case_when(!is.na(NUM1)&!is.na(NUM2)~paste0(NUM1," ",NUM2),
                         is.na(NUM1)|NUM1==""&!is.na(NUM2)~NUM2,
                         !is.na(NUM1)&is.na(NUM2)|NUM2==""~NUM1,
                         is.na(NUM1)|NUM1==""&is.na(NUM2)|NUM2==""~"")) %>% 
    mutate(ADD_STRING_WORKING=if_else(!is.na(NUM),str_replace_all(ADDRESS_STRING_NOPC,NUM,""),ADDRESS_STRING_NOPC)) %>%
    
   select(-START_DATE,-END_DATE,-MIGRATION_DATE,-LAT,-LONG,-COUNT,-X_COORDINATE,-Y_COORDINATE) %>% 
   mutate(UPRN=as.character(UPRN)) %>%
  #mutate(URSN=as.character(URSN)) %>%
  mutate(LATITUDE=as.character(LATITUDE)) %>%
  mutate(LONGITUDE=as.character(LONGITUDE)) %>%
  mutate(PARENT_UPRN=as.character(PARENT_UPRN)) %>%
  mutate(PARENT_UPRN=if_else(is.na(PARENT_UPRN),UPRN,PARENT_UPRN)) %>% 
  mutate( across(everything(), ~as.character(.x))) %>%  
  mutate( across(everything(), ~replace_na(.x, 'NULL'))) %>% 
  mutate(ADDRESS_STRING_NOPC=gsub("\\s+"," ",ADDRESS_STRING_NOPC)) %>% 
  mutate(ADDRESS_LINE_1=gsub("\\s+"," ",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_2=gsub("\\s+"," ",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_3=gsub("\\s+"," ",ADDRESS_LINE_3))
#USRN extra sprinkles
Street <- data.table::fread("/chi/(5) Postcode Lookup/UPRN_LOOKUP/StreetBLPUs_20220113.csv")
Street <-Street %>% 
  as.data.frame() %>% 
  select(-uprn,-pao_text,-logical_status,-x_coordinate,-y_coordinate,-custodian_code,-administrative_area) %>% 
  rename(USRN=usrn,REC_TYPE=record_type,DESCRIPTION=descriptor,LOCALITY=locality,TOWN=town,ISLAND=island) %>% 
  mutate(USRN=as.character(USRN)) %>% 
  filter(REC_TYPE!="3"	) %>% 
  filter(USRN!="48199984") %>% 
  mutate( across(everything(), ~toupper(.x))) %>% 
  mutate(DES_LEN=nchar(DESCRIPTION)) %>% 
  arrange(DES_LEN,ISLAND,TOWN)

library(tidytext)
tidy_street <- Street %>%
  unnest_tokens(word, DESCRIPTION) %>%   
  mutate(word=toupper(word)) 
# REMOVE SOME usrn AS RELATED TO FOOTPATH OR WHERE NO ONE LIVES EG ROUNDABOUT
USRN_non_pop<-tidy_street %>% 
  filter(word=="FOOTPATH"|word=="FOOTPATHS"|word=="ROUNDABOUT"|word==" FROM "|(word=="JUNCTION" & DES_LEN<23)|(word=="TO" & nchar(word)==2& !is.na(ISLAND))|
           (REC_TYPE=="2" & !is.na(ISLAND))|
           (REC_TYPE=="2" & word==" TRACK ")|
           (REC_TYPE=="2" & word==" CYCLETRACK ")|
           (REC_TYPE=="2" & word=="PRIVATE")) %>% 
  select(USRN)#geT USRN to remove that won't help
# data(stop_words)
# tidy_street <-tidy_street %>% 
# anti_join(stop_words) %>% 

tidy_street2 <-tidy_street %>% 
  count(word, sort = TRUE) 
#filter(!word %in% "")

str_abr<-data.table::fread("/chi/(5) Postcode Lookup/UPRN_LOOKUP/Street_abbrev.csv")
str_abr<-as.data.frame(str_abr)
street_abr<-left_join(tidy_street,str_abr,by=c("word"="V1"))
street_abr<-street_abr %>% 
  select(USRN,V2,word) %>% 
  rename(Abbr=V2) %>% 
  distinct()

street2<-left_join(Street,street_abr,by="USRN",keep=FALSE)

street2<-street2 %>% # use str_replace(string, pattern, replacement)
  filter(!is.na(Abbr)) %>%
  mutate (abbrv_street=stringr::str_replace(DESCRIPTION,word,Abbr)) %>% 
  #mutate(ABBR_STREET=paste0(abbrv_street," ",Abbr)) %>% 
  select(USRN,abbrv_street) %>% 
  distinct() 

street2<-street2 %>%
  anti_join(USRN_non_pop,by="USRN") %>% 
  distinct()
#check locality situation here
final<-left_join(Street,street2,by="USRN")
final<-final %>% 
  anti_join(USRN_non_pop,by="USRN") %>% 
  mutate(ABBR_STREET = tidyr::replace_na(abbrv_street, 'NULL')) %>% 
  #mutate(LOCALITY = tidyr::replace_na (LOCALITY, 'NULL')) %>% 
  mutate(ABBRV_LEN=nchar(ABBR_STREET)) %>% 
  select(-abbrv_street) %>% 
  mutate( across(everything(), ~as.character(.x))) %>%  
  mutate( across(everything(), ~replace_na(.x, 'NULL')))

# con2 <- dbConnect(odbc(), dsn = "SMRA",
#                  uid = rstudioapi::askForPassword("Database user"),
#                  pwd = rstudioapi::askForPassword("Database password"),
#                  port = "1527",
#                  host = "nssstats01.csa.scot.nhs.uk",
#                  SVC = "SMRA.nss.scot.nhs.uk")

##optional sql table
dbWriteTable(conn = con2, #connection
             "USRN", #table name
             final, #data
             overwrite = TRUE, #overwrite if it exists
             #options(odbc.batch_rows = 1000000),
             field.types = c(USRN = "VARCHAR2 (20)",
                             DESCRIPTION  = "VARCHAR2 (250)",
                             ABBR_STREET = "VARCHAR2 (250)",
                             DES_LEN = "VARCHAR2 (250)",
                             ABBRV_LEN = "VARCHAR2 (250)",
                             LOCALITY   = "VARCHAR2 (250)",
                             TOWN       = "VARCHAR2 (250)",
                             ISLAND    = "VARCHAR2 (250)",
                             REC_TYPE = "VARCHAR2 (1)"))

ad_base<-ad_base %>% 
  mutate(USRN=as.character(USRN))
#full_neigh<-left_join(full_neigh,Street,by="USRN")
full_neigh<-left_join(ad_base,final,by="USRN")

#define username for SQL tables
user_name <- toupper(Sys.info()['user'])
                  
if(dbExistsTable(con2, schema = user_name, name = "AD_BASE")) {dbRemoveTable(con2, "AD_BASE")}                  
                  
dbWriteTable(conn = con2, #connection
             "AD_BASE", #table name
             full_neigh, #data
             overwrite = TRUE, #overwrite if it exists
             #options(odbc.batch_rows = 1000000),
             field.types = c(UPRN = "VARCHAR2 (14)",                     
PARENT_UPRN="VARCHAR2 (14)",
USRN="VARCHAR2 (25)",
DESCRIPTION="VARCHAR2 (250)",
ABBR_STREET="VARCHAR2 (250)",
LOCALITY="VARCHAR2 (250)",
TOWN="VARCHAR2 (250)",
ISLAND="VARCHAR2 (250)",
ADDRESS_LINE_1="VARCHAR2 (90)",       
ADDRESS_LINE_2="VARCHAR2 (90)",
ADDRESS_LINE_3="VARCHAR2 (90)",  
POST_TOWN="VARCHAR2 (40)",     
POSTCODE="VARCHAR2 (8)",        
IDENTIFIER="VARCHAR2 (15)",    
LATITUDE="NUMBER(15)",                     
LONGITUDE="NUMBER(15)",                    
H3_6="VARCHAR2(15)",
H3_7="VARCHAR2(15)",
H3_8="VARCHAR2(15)",               
H3_10="VARCHAR2(15)",                   
H3_12="VARCHAR2(15)",
ADDRESS_STRING_NOPC="VARCHAR2(250)",
ADD_STRING_WORKING="VARCHAR2(250)",
CLASSIFICATION_DESCR="VARCHAR2(250)"))


                    # reformat dates!
                    
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD ADD_NUM varchar(250);")

dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
add SUB_UNIT varchar(250);")

dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
add BUILD_NO varchar(250);")

dbGetQuery(con2, statement ="UPDATE AD_BASE
           SET ADD_NUM = regexp_substr(AD_BASE.ADDRESS_STRING_NOPC,'(LEVEL|LVL|FLOOR|FLR|UNIT|ROOM|RM|FLAT|FLT|APT|APPARTMENT|SUITE)+.*[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+');")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET SUB_UNIT =regexp_substr(add_num, '[^ ]+', 1, 1);")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET BUILD_NO =regexp_substr(add_num, '[^ ]+', 1, 2);")

dbGetQuery(con2, statement ="UPDATE AD_BASE SET ADDRESS_LINE_1 = regexp_replace(AD_BASE.ADDRESS_LINE_1 ,'ROOM|FLAT|RM|FLT|FT|SUITE|BLOCK', '');")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET ADDRESS_STRING_NOPC = regexp_replace(AD_BASE.ADDRESS_STRING_NOPC,'ROOM|FLAT|RM|FLT|FT|SUITE|BLOCK', '');")

dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_ONE varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_TWO varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_THREE varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_FOUR varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_FIVE varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_SIX varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_SEVEN varchar(250);")
dbGetQuery(con2, statement = "ALTER TABLE AD_BASE
ADD WORD_EIGHT varchar(250);")

dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_ONE= REGEXP_SUBSTR(ADDRESS_STRING_NOPC,'\w+[A-Z]',1,1);")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_TWO = REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,2) ;")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_THREE = REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,3) ;")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_FOUR = REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,4) ;")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_FIVE = REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,5);")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET  WORD_SIX =REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,6)  ;")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_SEVEN =REGEXP_SUBSTR(AD_BASE.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,7);")
dbGetQuery(con2, statement = "UPDATE AD_BASE SET WORD_EIGHT=REGEXP_SUBSTR(SECOND_A.ADDRESS_STRING_NOPC,'\w+[A-Z]',1,8)  ;")

                    
                    
                    