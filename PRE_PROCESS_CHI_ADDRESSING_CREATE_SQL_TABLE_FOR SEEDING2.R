library(odbc)
library(data.table)
library(glue)
library(readr)
library(phsmethods)
library(tidylog)
library(dplyr)
library(stringr)
library(h3)
library(geosphere)
library(lubridate)
library(tidylog)
library(tidytext)
#ADD <- read_csv("/chi/Martin/care homes uprn/CI_Carehome_addresses.csv")
#ADD <- read_excel("/chi/Martin/care homes uprn/MDSF_data_28 February 2022 - Filtered for residential.xlsx")

ch_historic_addresses <- read_csv("/chi/Martin/care homes uprn/ch_historic_addresses.csv", 
                                  col_types = cols(COMPRESSED_ADDRESS = col_skip(), 
                                                   UPI_NUMBER_1 = col_skip()))

address_check<-ch_historic_addresses %>% 
  filter(!is.na(POSTCODE)) %>% 
  select(ADDRESS,POSTCODE) %>% 
  distinct() %>% 
  mutate(ID=row_number())

ADD_IN<-left_join(ch_historic_addresses, address_check,by="ADDRESS","POSTCODE",keep_all=F)
ADD <-ADD_IN%>% 
  tidyr::separate(
  ADDRESS,
  into= c("CURRENT_LINE1","CURRENT_LINE2","CURRENT_LINE3"),
  sep = "[$]") %>% 
  select(-POSTCODE.y,-UPI_NUMBER,-UPI_ID,-R_ID,-DATE_ADDRESS_CHANGED,-DOWNLOAD_DATE) %>% 
  rename(CURRENT_POSTCODE=POSTCODE.x) %>% 
  filter(!is.na(CURRENT_POSTCODE))

ADD2<- ADD %>% 
select(ID,CURRENT_LINE1,CURRENT_LINE2,CURRENT_LINE3,CURRENT_POSTCODE) %>% 
  mutate(across(everything(),~ gsub(",","", .))) %>% 
  mutate( across(everything(), ~replace_na(.x, "")))

ADD3<- sapply(apply(ADD2[,-1],1, unique),function(x) paste(na.omit(x),collapse = ", "))
df_new<-as.data.frame(ADD3)
df_new<-df_new %>% 
  mutate(ADD3=as.character(ADD3))
common_words <- df_new %>%
  unnest_tokens(word, ADD3) %>% 
  count(word, sort = TRUE)


ADD<-cbind(ADD,df_new)

ADD<-ADD %>% 
  rename(ADDRESS_STRING=ADD3) %>% 
  mutate( across(everything(), ~toupper(.))) %>%
  mutate(CURRENT_POSTCODE=phsmethods::postcode(CURRENT_POSTCODE,c("pc7"))) %>% 
  mutate(ADDRESS_STRING=as.character(ADDRESS_STRING)) %>%
  #mutate(NUM1=stringr::str_extract(NAME,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
  mutate(NUM1=stringr::str_extract(CURRENT_LINE1,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
  mutate(NUM2=stringr::str_extract(CURRENT_LINE2,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
  mutate(NUM3=stringr::str_extract(CURRENT_LINE3,"((([A-Z]?\\d+)+([A-Z]?)*)(-|\\/|&)?((\\d|[A-Z]?)(\\s)?(\\s)?(\\d+|[A-Z]?)+(\\/)*((\\d|[A-Z{1}])*(\\d+)([A-Z])*))*|((\\d)*(\\s)+[A-Z{1}](\\s)+\\d*|\\d[A-Z{1}](\\s))(\\s[A-Z{1}]\\s))")) %>%
  mutate( across(everything(), ~replace_na(.x, ""))) %>% 
  mutate(NUM=paste0(NUM1," ",NUM2," ",NUM3)) %>% 
  mutate(ADDRESS_STRING_NOPC=if_else(!is.na(CURRENT_POSTCODE),str_replace_all(ADDRESS_STRING,CURRENT_POSTCODE,""),ADDRESS_STRING)) %>% 
  mutate(ADDRESS_STRING_NONUM=if_else(!is.na(NUM),str_replace_all(ADDRESS_STRING_NOPC,NUM,""),ADDRESS_STRING_NOPC)) %>% 
  mutate(across(everything(),~ gsub(", , ", ", ", .)))
#LATEST SPD
spd<-fread("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2022_1.csv")

spd<-spd %>% 
  select(pc7,latitude,longitude,grid_reference_easting,grid_reference_northing,ur8_2016,hb2019) %>% 
  mutate(pc7=phsmethods::postcode(pc7,c("pc7")))

add_coor<-left_join(ADD,spd,by=c("CURRENT_POSTCODE"="pc7"))
add_coor<-add_coor %>% 
  filter(!is.na(latitude)) %>% #drop bad postcodes
  #mutate(KEY=paste0(CURRENT_POSTCODE,ID)) %>% 
  select(ID, CURRENT_POSTCODE,CURRENT_LINE1,CURRENT_LINE2,CURRENT_LINE3,ADDRESS_STRING,ADDRESS_STRING_NOPC,NUM,latitude,longitude) %>% 
  rename(P_LAT=latitude,P_LONG=longitude) %>% 
  filter(!is.na(P_LAT)) 
#addin h3 to reduce serch space FROM uprn

CO1<-add_coor[, c("P_LAT", "P_LONG")]

add_coor<-add_coor %>% 
  mutate(H3_4_P=h3::geo_to_h3(CO1,res=4)) %>% 
  mutate(H3_5_P=h3::geo_to_h3(CO1,res=5)) %>% 
  mutate(H3_6_P=h3::geo_to_h3(CO1,res=6)) %>% 
  mutate(H3_7_P=h3::geo_to_h3(CO1,res=7)) %>% 
  mutate(H3_8_P=h3::geo_to_h3(CO1,res=8)) %>% 
  mutate(H3_9_P=h3::geo_to_h3(CO1,res=9)) %>% 
  mutate(H3_10_P=h3::geo_to_h3(CO1,res=10)) %>% 
  select(ID,CURRENT_POSTCODE,CURRENT_LINE1,CURRENT_LINE2,CURRENT_LINE3,ADDRESS_STRING,ADDRESS_STRING_NOPC,NUM,P_LAT,P_LONG,H3_4_P,H3_5_P,H3_6_P,H3_7_P,H3_8_P,H3_9_P,H3_10_P) %>%
  mutate(P_LAT=as.character(P_LAT)) %>%
  mutate(P_LONG=as.character(P_LONG))  
  
  #neighbouring areas
  h3<-add_coor %>% 
  select(H3_4_P) %>% 
  distinct()

#get coordinates for tiles
CO1<-h3 %>% select(H3_4_P) %>% pull()



master<-tibble(c("check","neighbours","ring_position"))

master_names <- c("check","centre","ring_position")
#Create  summmary data set.
temp <- data.frame(matrix(ncol = 3, nrow = 0))
#Assign variable names to summary data set.
colnames(temp) <- master_names

for (j in seq_along(CO1)){
  c <- CO1[j]
  check<-h3::k_ring(c,3) 
  neighbours<-tibble(check) %>% 
    mutate(centre=c) %>% 
    #filter(!centre==check) %>% 
    mutate(ring_position=as.character(row_number())) %>% 
    tidyr::spread(ring_position,check)
  temp<-rbind(temp,neighbours) 
  rm(neighbours)
}

temp<-temp %>%
  mutate(NEAR_NEIGHBOURS=paste0(`1`,`2`,`3`,`4`,`5`,`6`,`7`)) %>% 
  mutate(FAR_NEIGHBOURS=paste0(`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`)) %>%
  mutate(VFAR_NEIGHBOURS=paste0(`20`,`21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`)) %>% 
  
  mutate(NEIGHBOURS=paste0(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,`21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`)) %>%
  select(-`1`,-`2`,-`3`,-`4`,-`5`,-`6`,-`7`,-`8`,-`9`,-`10`,-`11`,-`12`,-`13`,-`14`,-`15`,-`16`,-`17`,-`18`,-`19`,-`20`,-`21`,-`22`,-`23`,-`24`,-`25`,-`26`,-`27`,-`28`,-`29`,-`30`,-`31`,-`32`,-`33`,-`34`,-`35`,-`36`,-`37`)

add_coor<-left_join(add_coor,temp,by=c("H3_4_P"="centre"))
  
  
  
  
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(NAME=as.character(gsub("["]",""))) %>% 
  # mutate(NAME=sub("^(FLAT 0-)","0/",NAME)) %>% 
  # mutate(NAME=gsub("-","/",NAME)) %>% 
  # mutate(NAME=sub("^(FLAT |^F )","",NAME)) %>% 
  
  #CLEAN SEPERATE LINES
add_coor<-add_coor %>% 
  mutate(CURRENT_LINE1=gsub("@"," ",CURRENT_LINE1)) %>% 
  mutate(CURRENT_LINE1=gsub(","," ",CURRENT_LINE1)) %>%  
  mutate(CURRENT_LINE1=gsub("\\."," ",CURRENT_LINE1)) %>% 
  mutate(CURRENT_LINE1=gsub("'","",CURRENT_LINE1)) %>% 
  mutate(CURRENT_LINE1=sub("(C/O)+\\s[A-Z]*","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("(C-O)+\\s[A-Z]*","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("(C-0)\\s[A-Z]*","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("(C/0)+\\s[A-Z]*","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("^(C/0)","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("^(C/O)","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("^(C-0)","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("^(C-O)","",CURRENT_LINE1)) %>%
  mutate(CURRENT_LINE1=sub("  "," ",CURRENT_LINE1)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(CURRENT_LINE1=as.character(gsub("["]",""))) %>% 
  mutate(CURRENT_LINE1=sub("^(FLAT 0-)","0/",CURRENT_LINE1)) %>% 
  mutate(CURRENT_LINE1=gsub("-","/",CURRENT_LINE1)) %>% 
  mutate(CURRENT_LINE1=sub("^(FLAT |^F )","",CURRENT_LINE1)) %>% 
  
  mutate(CURRENT_LINE2=gsub("@"," ",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=gsub(","," ",CURRENT_LINE2)) %>%  
  mutate(CURRENT_LINE2=gsub("\\."," ",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=gsub("'","",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=sub("(C/O)+\\s[A-Z]*","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("(C-O)+\\s[A-Z]*","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("(C-0)\\s[A-Z]*","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("(C/0)+\\s[A-Z]*","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("^(C/0)","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("^(C/O)","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("^(C-0)","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("^(C-O)","",CURRENT_LINE2)) %>%
  mutate(CURRENT_LINE2=sub("  "," ",CURRENT_LINE2)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(CURRENT_LINE2=as.character(gsub("["]",""))) %>% 
  mutate(CURRENT_LINE2=sub("^(FLAT 0-)","0/",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=gsub("-","/",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=sub("^(FLAT |^F )","",CURRENT_LINE2)) %>% 
  
  mutate(CURRENT_LINE3=gsub("@"," ",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=gsub(","," ",CURRENT_LINE3)) %>%  
  mutate(CURRENT_LINE3=gsub("\\."," ",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=gsub("'","",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=sub("(C/O)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C-O)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C-0)\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C/0)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C/0)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C/O)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C-0)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C-O)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("  "," ",CURRENT_LINE3)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(CURRENT_LINE3=as.character(gsub("["]",""))) %>% 
  mutate(CURRENT_LINE3=sub("^(FLAT 0-)","0/",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=gsub("-","/",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=sub("^(FLAT |^F )","",CURRENT_LINE3)) %>% 
  
  mutate(ADDRESS_STRING=gsub(","," ",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=gsub("\\."," ",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=gsub("'","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("(C/O)+\\s[A-Z]*","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("(C-O)+\\s[A-Z]*","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("(C-0)\\s[A-Z]*","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("(C/0)+\\s[A-Z]*","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("^(C/0)","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("^(C/O)","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("^(C-0)","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("^(C-O)","",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=sub("  "," ",ADDRESS_STRING)) %>%
  #GET RID OF C/O FOLLOWED BY NAME
  #mutate(ADDRESS_STRING=as.character(gsub("["]",""))) %>% 
  mutate(ADDRESS_STRING=sub("^(FLAT 0-)","0/",ADDRESS_STRING)) %>% 
  mutate(ADDRESS_STRING=sub("-","\\/",ADDRESS_STRING)) %>% 
  mutate(ADDRESS_STRING=sub("^(FLAT |^F )","",ADDRESS_STRING)) 
  
  
con <- dbConnect(odbc(), dsn = "SMRA",
                 uid = rstudioapi::askForPassword("user"),
                 pwd = rstudioapi::askForPassword("Database password"),
                 port = "1527",
                 host = "nssstats01.csa.scot.nhs.uk",
                 SVC = "SMRA.nss.scot.nhs.uk")


if(dbExistsTable(con, schema = user_name, name = "CHI_ADD_HIST")) {
  dbRemoveTable(con, "CHI_ADD_HIST")
}

dbWriteTable(conn = con, #connection
             "CHI_ADD_HIST", #table name
             add_coor, #data
             overwrite = TRUE, #overwrite if it exists
             field.types = c(ID= "VARCHAR2(12)",
                             CURRENT_POSTCODE= "VARCHAR2(8)",
                             CURRENT_LINE1= "VARCHAR2(250)",
                             CURRENT_LINE2= "VARCHAR2(250)",
                             CURRENT_LINE3= "VARCHAR2(250)",
                             ADDRESS_STRING="VARCHAR2(250)",
                             ADDRESS_STRING_NOPC="VARCHAR2(250)",
                             H3_4_P="VARCHAR2(15)",
                             H3_5_P="VARCHAR2(15)",
                             H3_6_P="VARCHAR2(15)",
                             H3_7_P="VARCHAR2(15)",
                             H3_8_P="VARCHAR2(15)",
                             H3_9_P="VARCHAR2(15)",
                             H3_10_P="VARCHAR2(15)",
                             NUM="VARCHAR2(25)"),
                              NEAR_NEIGHBOURS="VARCHAR2(105)",
             FAR_NEIGHBOURS="VARCHAR2(180)",
             VFAR_NEIGHBOURS="VARCHAR2(270)")


if(dbExistsTable(con, schema = user_name, name = "SECOND_A")) {dbRemoveTable(con, "SECOND_A")}                  

dbGetQuery(con, statement ="CREATE TABLE SECOND_A AS SELECT DISTINCT 
NUM, CURRENT_LINE1, CURRENT_LINE2,CURRENT_LINE3,CURRENT_POSTCODE,
H3_8_P,H3_5_P,H3_6_P,H3_7_P,H3_9_P,H3_10_P,ADDRESS_STRING,ADDRESS_STRING_NOPC,
replace(ADDRESS_STRING,' ','') as ADD_NO_SPACE,
regexp_substr(ADDRESS_STRING,'(LEVEL|LVL|FLOOR|FLR|UNIT|ROOM|RM|FLAT|FLT|APT|APPARTMENT|SUITE)+.*[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+') AS ADD_NUM
FROM CHI_ADD")

  
  
  
       

