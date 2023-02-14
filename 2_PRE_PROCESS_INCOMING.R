############################
####DESCRIPTION
####CLEAN AND LOAD INCOMING ADDRESSES READY FOR UPRN SEEDING
####WATCH OUT FOR NUMBER OF ADDRESS LINES, IF POSTCODES EXIST - FORMATTING
###################################

############################
####PACKAGES######

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
library(glue)
library(phonics)
library(tidyr)
############################

############################
#######PARAMETERS - check your parameters #####

spd_version<-"2022_2"
user_folder<-"Martin_R/"
scripts<-"UPRN Seeding/1 - Scripts/"
ref_data<-"UPRN Seeding/2 - Ref Data/"
base<-"/conf/linkage/output/"

spd<-data.table::fread(glue("{base}lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_{spd_version}.csv"))

LOCALITY <- readr::read_csv(glue("{base}{user_folder}{ref_data}NON_POSTCODES_LAT_LONG.csv"))

incoming <- fread(glue("{base}{user_folder}UPRN Seeding/care_homes_bk/Feb update/Care_Inspectorate_datastore_2022_11_30.csv"))

#incoming <- read_csv("/conf/linkage/output/Martin_R/UPRN Seeding/care_homes_bk/Dec Update/Care_Inspectorate_datastore_2022_10_31.csv")

#incoming <- read_csv("/conf/linkage/output/Martin_R/UPRN Seeding/care_homes_bk/Dec Update/Care_Inspectorate_datastore_2022_09_30.csv")

#incoming <- read_csv("/conf/linkage/output/Martin_R/UPRN Seeding/care_homes_bk/Dec Update/Care_Inspectorate_datastore_2022_08_31.csv")

incoming <- incoming %>% 
  janitor::clean_names() %>% 
  select( grep("address|town|current|service_name|city|street|postcode|number", colnames(.)))
###################

############################
#####OPTIONAL DO you need to add an ID key#######

#incoming <-incoming %>% 
#mutate(ID=row_number()) 
#fwrite(incoming,glue("{base}{user_folder}UPRN Seeding/care_homes_bk/Feb update/new_unique_cI_nov2022.csv"))
#write out reference file for evaluation phase
#########
############################

###########################################
####### MANUAL EDIT- MAP INCOMING DATA - do you wnat to include company names and map an pre existing ID? ####

address_check<-incoming %>% 
  rename(ID=cs_number,
        NAME=service_name,
         ADDRESS_LINE1=address_line_1,
         ADDRESS_LINE2=address_line_2,
         ADDRESS_LINE3=address_line_3,
         POSTCODE=service_postcode) %>% 
  select(ID,NAME,ADDRESS_LINE1,ADDRESS_LINE2,ADDRESS_LINE3,POSTCODE) %>% 
  mutate(across(everything(), ~replace_na(.x, ""))) %>% 
  mutate(ADDRESS_LINE1=paste0(NAME,", ",ADDRESS_LINE1 ))  #if name exists make it part of incoming
 
###########################

############################
######CREATE ADDRESS STRING #########

ADD2<- address_check %>% 
  select(ID,ADDRESS_LINE1,ADDRESS_LINE2,ADDRESS_LINE3,POSTCODE) %>% 
  #mutate(across(everything(),~ gsub(",","", .))) %>% 
  mutate( across(everything(), ~replace_na(.x, ""))) 
ADD3<-ADD2 %>% 
unite("ADDRESS_STRING", c(ADDRESS_LINE1,ADDRESS_LINE2,ADDRESS_LINE3), sep=", ", remove = FALSE, na.rm = TRUE) %>% 
  select(ADDRESS_STRING)
#ADD3<- sapply(apply(ADD2[,-1],1, unique),function(x) paste(na.omit(x),collapse = ","))


ADD<-cbind(ADD2,ADD3)
############################

############################
#######ASSIGN NEW FEATURES#########

ADD<-ADD %>% 
  rename(CURRENT_LINE1=ADDRESS_LINE1,CURRENT_LINE2=ADDRESS_LINE2,CURRENT_LINE3=ADDRESS_LINE3) %>% 
  mutate( across(everything(), ~toupper(.))) %>%
  mutate(CURRENT_POSTCODE=phsmethods::postcode(POSTCODE,c("pc7"))) %>% 
  mutate(ADDRESS_STRING=as.character(ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=gsub(", ,",", ",ADDRESS_STRING)) %>%
  mutate(ADDRESS_STRING=gsub(",,","",ADDRESS_STRING)) %>% 
  mutate( across(everything(), ~replace_na(.x, ""))) %>% 
  mutate(ADDRESS_STRING_NOPC=if_else(!is.na(CURRENT_POSTCODE),str_replace_all(ADDRESS_STRING,CURRENT_POSTCODE,""),ADDRESS_STRING)) %>% 
  mutate(ADD_STRING_WORKING=gsub('[[:digit:]]+','',ADDRESS_STRING_NOPC)) %>% 
  mutate(ADD_STRING_WORKING=str_replace_all(ADD_STRING_WORKING, "[^[:alpha:]][:whitespace:]", " ")  ) %>% 
  mutate(ADD_STRING_WORKING=trimws (ADD_STRING_WORKING)) %>% 
  mutate(WORD_ONE=word(ADD_STRING_WORKING,1)) %>% 
  mutate(WORD_TWO=word(ADD_STRING_WORKING,2)) %>%
  mutate(WORD_THREE=word(ADD_STRING_WORKING,3)) %>%
  mutate(WORD_FOUR=word(ADD_STRING_WORKING,4)) %>%
  mutate(WORD_FIVE=word(ADD_STRING_WORKING,5)) %>%
  mutate(WORD_SIX=word(ADD_STRING_WORKING,6)) %>%
  mutate(word_m1=metaphone(word=WORD_ONE)) %>% 
  mutate(word_m2=metaphone(word=WORD_TWO)) %>% 
  mutate(word_m3=metaphone(word=WORD_THREE)) %>% 
  mutate(word_m4=metaphone(word=WORD_FOUR)) %>% 
  mutate(word_m5=metaphone(word=WORD_FIVE)) %>% 
  mutate(word_m6=metaphone(word=WORD_SIX)) %>% 
  mutate(WORD_ONE_M = tidyr::replace_na (word_m1, 'NULL')) %>%
  mutate(WORD_TWO_M = tidyr::replace_na (word_m2, 'NULL')) %>%
  mutate(WORD_THREE_M = tidyr::replace_na (word_m3, 'NULL')) %>%
  mutate(WORD_FOUR_M = tidyr::replace_na (word_m4, 'NULL')) %>%
  mutate(WORD_FIVE_M = tidyr::replace_na (word_m5, 'NULL')) %>%
  mutate(WORD_SIX_M = tidyr::replace_na (word_m6, 'NULL')) 
############################

############################
#####ASSIGN LAT N LONG#####

spd<-spd %>% 
  select(pc7,latitude,longitude) 

add_coor<-left_join(ADD,spd,by=c("CURRENT_POSTCODE"="pc7"))
add_coor<-add_coor %>% 
  filter(!is.na(latitude)) %>% #drop bad postcodes
  rename(P_LAT=latitude,P_LONG=longitude) 

#get lat n long from locality / island for non postcoded addresses
add_coor2<-anti_join(ADD,spd,by=c("CURRENT_POSTCODE"="pc7"))
add_coor2<-add_coor2 %>% 
  mutate(add_string_trimmed=stringr::str_replace(ADDRESS_STRING,"ABERDEENSHIRE$*?|CAITHNESS$*?|SUTHERLAND$*?|ROSS AND CROMARTY$*?|INVERNESS-SHIRE$*?|NAIRNSHIRE$*?
|MORAY$*?|BANFFSHIRE$*?|ABERDEENSHIRE$*?|KINCARDINESHIRE$*?|ANGUS$*?|PERTHSHIRE$*?|ARGYLL$*?|AYRSHIRE$*?|RENFREWSHIRE$*?|DUNBARTONSHIRE$*?|STIRLINGSHIRE$*?
|CLACKMANNANSHIRE$*?|KINROSS-SHIRE$*?|FIFE$*?|EAST LOTHIAN$*?|MIDLOTHIAN$*?|WEST LOTHIAN$*?|LANARKSHIRE$*?|PEEBLESSHIRE$*?|SELKIRKSHIRE$*?
|BERWICKSHIRE$*?|ROXBURGHSHIRE$*?|DUMFRIESSHIRE$*?|KIRKCUDBRIGHTSHIRE$*?|WIGTOWNSHIRE$*?","")) %>% 
  mutate(add_string_trimmed=stringi::stri_replace_last(add_string_trimmed, fixed = ", ", "")) %>% 
  mutate (LAST_WORD= str_extract(add_string_trimmed, '\\b[^,]+$'))

LOCALITY <- LOCALITY %>% 
  mutate(name=toupper(name))

add_coor2<-left_join(add_coor2,LOCALITY,by=c("LAST_WORD"="name"))

add_coor2<-add_coor2 %>%
  rename(P_LAT=lat,P_LONG=long) %>% 
select(-LAST_WORD,-FID,-add_string_trimmed )
#addin h3 to reduce serch space FROM uprn
add_coor<-rbind(add_coor,add_coor2) # eg tarbet can introduce duplicates

add_coor<-add_coor %>% 
mutate(P_LAT=as.numeric(P_LAT)) %>% 
  mutate(P_LONG=as.numeric(P_LONG)) 

############################

############################
#####ASSIGN H3 TILES#######


CO1<-add_coor[, c("P_LAT", "P_LONG")]

add_coor<-add_coor %>% 
  mutate(H3_4_P=h3::geo_to_h3(CO1,res=4)) %>% 
  mutate(H3_5_P=h3::geo_to_h3(CO1,res=5)) %>% 
  mutate(H3_6_P=h3::geo_to_h3(CO1,res=6)) %>% 
  mutate(H3_7_P=h3::geo_to_h3(CO1,res=7)) %>% 
  mutate(H3_8_P=h3::geo_to_h3(CO1,res=8)) %>% 
  mutate(H3_9_P=h3::geo_to_h3(CO1,res=9)) %>% 
  mutate(H3_10_P=h3::geo_to_h3(CO1,res=10)) %>% 
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
############################

############################
#######PRE UPLOAD TO ORACLE#######
source(glue("{base}{user_folder}{scripts}/data_clean_UPRN.R"))

############################

############################
#######MAKE A CONNECTION##########
source(glue("{base}{user_folder}{scripts}/Make_SMRA_Connection_v1.R"))
############################

############################
#######WRITE UP DATA TO ORACLE##########

dbWriteTable(conn = con, #connection
             "ADD_IN", #table name
             add_coor, #data
             overwrite = TRUE, #overwrite if it exists
             field.types = c(ID= "VARCHAR2(12)",
                             CURRENT_POSTCODE= "VARCHAR2(8)",
                             CURRENT_LINE1= "VARCHAR2(250)",
                             CURRENT_LINE2= "VARCHAR2(250)",
                             CURRENT_LINE3= "VARCHAR2(250)",
                             ADD_STRING_WORKING="VARCHAR2(250)",
                             ADDRESS_STRING="VARCHAR2(250)",
                             ADD_NUM="VARCHAR2(250)",
                             SUB_UNIT="VARCHAR2(250)",
                             BUILD_NO="VARCHAR2(250)",
                             H3_4_P="VARCHAR2(15)",
                             H3_5_P="VARCHAR2(15)",
                             H3_6_P="VARCHAR2(15)",
                             H3_7_P="VARCHAR2(15)",
                             H3_8_P="VARCHAR2(15)",
                             H3_9_P="VARCHAR2(15)",
                             H3_10_P="VARCHAR2(15)",
                             NEAR_NEIGHBOURS="VARCHAR2(105)",
                             FAR_NEIGHBOURS="VARCHAR2(180)",
                             VFAR_NEIGHBOURS="VARCHAR2(270)",
                             WORD_ONE="VARCHAR2(250)",
                             WORD_TWO="VARCHAR2(250)",
                             WORD_THREE="VARCHAR2(250)",
                             WORD_FOUR="VARCHAR2(250)",
                             WORD_FIVE="VARCHAR2(250)",
                             WORD_SIX="VARCHAR2(250)",
                             WORD_ONE_M="VARCHAR2(250)",
                             WORD_TWO_M="VARCHAR2(250)",
                             WORD_THREE_M="VARCHAR2(250)",
                             WORD_FOUR_M="VARCHAR2(250)",
                             WORD_FIVE_M="VARCHAR2(250)",
                             WORD_SIX_M="VARCHAR2(250)"))
############################

############################
