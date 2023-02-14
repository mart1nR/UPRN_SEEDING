################
##DESCRIPTION##
##UPDATE ADDRESSBASE REF FILE USED FOR UPRN SEEDING TO LATEST VERSION.
##CURRENTLY DATA SOURCED FROM IMPROVEMENT SERVICE.
################

################
#####PACKAGES###########

library(dplyr)
library(odbc)
library(h3)
library(stringdist)
library(readr)
library(stringr)
library(tidytext)
library(tidylog)
library(phonics)
library(glue)
################

################
####PARAMS######
base<-"/conf/linkage/output/"
user_folder<-"GeraldLYH/"
scripts<-"UPRN Seeding/1 - Scripts/"
ref_data<-"UPRN Seeding/2 - Ref Data/"
################

####################
#######CONNECTION############
# con2 <- dbConnect(odbc(), dsn = "SMRA",
#                   uid = rstudioapi::askForPassword("Database user"),
#                   pwd = rstudioapi::askForPassword("Database password"),
#                   port = "1527",
#                   host = "nssstats01.csa.scot.nhs.uk",
#                   SVC = "SMRA.nss.scot.nhs.uk")

#Could be a source called script such as below:
source(glue("{base}{user_folder}{scripts}Make_SMRA_Connection_v1.R"))

#credential <- read.csv(paste0("/home/",uname,"/",toupper(uname),"_credentials.csv"))

#pwd <- credential$Password
#userid <- credential$Username 

#con <- dbConnect(odbc(), dsn = "SMRA",
#                 uid = paste0(userid),
#                 pwd = paste0(pwd),
#                 port = "1527",
#                 host = "nssstats01.csa.scot.nhs.uk",
#                 SVC = "SMRA.nss.scot.nhs.uk")
####################

###############################
########ADDRESSBASE UPDATE #############################
library(readr)
ad_base<- read_delim(glue("{ref_data}Scottish_Address_Data_NHSNSS_EPOCH98_2023-01-22.csv"), 
                     delim = "|", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
ad_base<-ad_base %>% 
  #slice(1:1000) %>% #troubleshooting
  rename(UPRN=X1,PARENT_UPRN=X2,ADDRESS_LINE_1=X3,ADDRESS_LINE_2=X4,ADDRESS_LINE_3=X5,POST_TOWN=X6,POSTCODE=X7,IDENTIFIER=X8,
         LA_LOCATION=X9,START_DATE=X10,END_DATE=X11,X_COORDINATE=X12,Y_COORDINATE=X13,ADDRESS_STRING=X14,LATITUDE=X15,LONGITUDE=X16,
         MIGRATION_DATE=X17,USRN=X18,CLASSIFICATION_LOCATION=X19,CLASSIFICATION_DESCR=X20,CLASSIFICATION_PRIMARY=X21,CLASSIFICATION_SECONDARY=X22,
         CLASSIFICATION_TERTIARY=X23,CLASSIFICATION_QUATERNARY=X24) %>% 
  mutate(UPRN=as.character(UPRN)) %>% 
  mutate(USRN=as.character(USRN)) %>% 
  mutate(LAT=as.numeric(LATITUDE)) %>% 
  mutate(LONG=as.numeric(LONGITUDE))


###############################

##########################
########ASSIGN H3 TILES##########
coords<-paste0(as.numeric(ad_base$LATITUDE),as.numeric(ad_base$LONGITUDE))
CO1<-ad_base[, c("LAT", "LONG")]

ad_base<-ad_base %>% 
  mutate(H3_4=h3::geo_to_h3(CO1,res=4)) %>% 
  mutate(H3_5=h3::geo_to_h3(CO1,res=5)) %>%
  mutate(H3_6=h3::geo_to_h3(CO1,res=6)) %>% 
  mutate(H3_7=h3::geo_to_h3(CO1,res=7)) %>%
  mutate(H3_8=h3::geo_to_h3(CO1,res=8)) %>%
  mutate(H3_9=h3::geo_to_h3(CO1,res=9)) %>%
  mutate(H3_10=h3::geo_to_h3(CO1,res=10)) %>% 
  mutate(H3_12=h3::geo_to_h3(CO1,res=12))
##########################

###################################
######FILTER UNNECESSARY RECORDS######## 
source(glue("{base}{user_folder}{scripts}FILTER_ADDRESSBASE.R"))

###################################

######DATA CLEANING & NEW FEATURES######## 
ad_base<-ad_base %>%   
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
  mutate(ADDRESS_LINE_2=sub("^(FLAT 0-)","0/",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=gsub("-","/",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_2=sub("^(FLAT |^F )","",ADDRESS_LINE_2)) %>% 
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

  mutate(ADDRESS_STRING_NOPC=gsub(","," ",ADDRESS_STRING_NOPC)) %>%
  mutate(ADDRESS_STRING_NOPC=gsub("\\."," ",ADDRESS_STRING_NOPC)) %>%
  mutate(ADDRESS_STRING_NOPC=gsub("\\)"," ",ADDRESS_STRING_NOPC)) %>%
  mutate(ADDRESS_STRING_NOPC=gsub("\\("," ",ADDRESS_STRING_NOPC)) %>%
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
  mutate(ADDRESS_STRING_NOPC=sub("^(FLAT 0-)","0/",ADDRESS_STRING_NOPC)) %>% 
  mutate(ADDRESS_STRING_NOPC=sub("-","\\/",ADDRESS_STRING_NOPC)) %>% 
  mutate(ADDRESS_STRING_NOPC=sub("^(FLAT |^F )","",ADDRESS_STRING_NOPC)) %>% 
  mutate(ADD_NUM=stringr::str_extract(ADDRESS_STRING_NOPC,"^/.*[[:digit:]]+[[:alpha:]]?[[:digit:]]?+
                                      |^.*[[:digit:]]+[[:alpha:]]|^.*[[:digit:]]|[[:digit:]].*[[:digit:]]+[[:alpha:]]*
                                      |[[:digit:]]+[[:alpha:]]|(LEVEL |LVL |FLOOR |FLR |UNIT |ROOM |RM |FLAT |FLT |APT 
                                      |APARTMENT |SUITE )+.*[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+'")) %>% 
  mutate(BUILD_NO=word(ADD_NUM,-1)) %>% 
  mutate(SUB_UNIT=substring(ADD_NUM,1,(nchar(ADD_NUM))-(nchar(BUILD_NO)))) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="UNIT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="UNIT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="ROOM ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="CARAVAN ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="SUITE ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="OFFICE ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="SHOP ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="CHALET ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="APARTMENT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="BEDSIT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(ADD_NUM=sub('\\)+|\\(+','',ADD_NUM)) %>% 
  
  mutate(ADD_STRING_WORKING=if_else(!is.na(ADD_NUM),str_replace_all(ADDRESS_STRING_NOPC,ADD_NUM,""),ADDRESS_STRING_NOPC)) %>% 
  
  #clean<-as.data.frame(table(ad_base$SUB_UNIT))
  
  
  mutate(ADD_STRING_WORKING=trimws (ADD_STRING_WORKING)) %>% 
  mutate(ADD_STRING_WORKING=str_replace_all(ADD_STRING_WORKING, "[^[:alpha:]][:whitespace:]", " ")  ) %>% #new line 
  mutate(UPRN=as.character(UPRN)) %>% 
  select(-START_DATE,-END_DATE,-X_COORDINATE,-Y_COORDINATE) %>% 
  mutate(LATITUDE=as.character(LATITUDE)) %>%
  mutate(LONGITUDE=as.character(LONGITUDE)) %>%
  mutate(PARENT_UPRN=as.character(PARENT_UPRN)) %>%
  mutate(PARENT_UPRN=if_else(is.na(PARENT_UPRN),UPRN,PARENT_UPRN)) %>% 
  mutate( across(everything(), ~as.character(.x))) %>%  
  mutate( across(everything(), ~replace_na(.x, 'NULL'))) %>% 
  mutate(ADDRESS_STRING_NOPC=gsub("\\s+"," ",ADDRESS_STRING_NOPC)) %>% 
  mutate(ADDRESS_LINE_1=gsub("\\s+"," ",ADDRESS_LINE_1)) %>% 
  mutate(ADDRESS_LINE_2=gsub("\\s+"," ",ADDRESS_LINE_2)) %>% 
  mutate(ADDRESS_LINE_3=gsub("\\s+"," ",ADDRESS_LINE_3)) %>% 
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
#####################################

########################################
#### FILTER USRN FOR UNWANTED RECORDS####
Street <- read_csv("/conf/linkage/output/Martin_R/UPRN Seeding/2 - Ref Data/usrndata_EPOCH_98.csv")

Street <-Street %>% 
  as.data.frame() %>% 
  select(usrn,street_description,locality,town_name) %>% 
  rename(USRN=usrn,LOCALITY=locality,TOWN=town_name,DESCRIPTION=street_description) %>% 
  mutate(USRN=as.character(USRN)) %>% 
  mutate(TOWN = tidyr::replace_na (TOWN, 'NULL')) %>% 
  #mutate(ISLAND = tidyr::replace_na (ISLAND, 'NULL')) %>% 
  mutate(LOCALITY = tidyr::replace_na (LOCALITY, 'NULL')) %>% 
  #filter(REC_TYPE!="3"	) %>% 
  #filter(USRN!="48199984") %>% 
  mutate( across(everything(), ~toupper(.x))) %>%
  filter(stringr::str_detect( DESCRIPTION,"FOOTPATH")!=T
         &stringr::str_detect( DESCRIPTION,"FOOTBRIDGE")!=T
         &stringr::str_detect( DESCRIPTION, "FOOTPTH ")!=T
         &stringr::str_detect(DESCRIPTION,"ROUNDABOUT ")!=T
         &stringr::str_detect(DESCRIPTION,"JUNCTION")!=T
         &stringr::str_detect(DESCRIPTION," TO ")!=T
         &stringr::str_detect(DESCRIPTION," ISLAND ")!=T
         &stringr::str_detect(DESCRIPTION," TRACK ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLETRACK ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLEWAY ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLE TRACK ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLETRACK ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLE WAY ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLEPATH ")!=T
         &stringr::str_detect(DESCRIPTION," CYCLE PATH ")!=T
         &stringr::str_detect(DESCRIPTION,"PRIVATE")!=T
         &stringr::str_detect(DESCRIPTION,"CAR PARK")!=T
         &stringr::str_detect(DESCRIPTION,"RAMP ")!=T
         &stringr::str_detect(DESCRIPTION,"ACCESS ROAD ")!=T
         &stringr::str_detect(DESCRIPTION,"SERVICE ROAD ")!=T
         &stringr::str_detect(DESCRIPTION,"SPUR ROAD ")!=T
         &stringr::str_detect(DESCRIPTION,"LAND SERVING ")!=T
         &stringr::str_detect(DESCRIPTION,"LINK ROAD ")!=T
         &stringr::str_detect(DESCRIPTION," ROAD AT ")!=T
         &stringr::str_detect(DESCRIPTION,"BETWEEN ")!=T
         &stringr::str_detect(DESCRIPTION," VIA ")!=T
         &stringr::str_detect(DESCRIPTION," F/P ")!=T
         &stringr::str_detect(DESCRIPTION," PLAY AREA ")!=T
         &stringr::str_detect(DESCRIPTION," PLAYAREA ")!=T
         &stringr::str_detect(DESCRIPTION," CROSSROADS ")!=T
         &stringr::str_detect(DESCRIPTION," CARRIAGEWAY ")!=T
         &stringr::str_detect(DESCRIPTION," SLIP ROAD ")!=T
         &stringr::str_detect(DESCRIPTION," RETAIL ")!=T
         &stringr::str_detect(DESCRIPTION," COACH PARK ")!=T
         &stringr::str_detect(DESCRIPTION," PARKING COURT ")!=T
         &stringr::str_detect(DESCRIPTION," GARAGE COURT ")!=T
         &stringr::str_detect(DESCRIPTION," CAR PARK ")!=T
         &stringr::str_detect(DESCRIPTION," PARKING AREA ")!=T
         &stringr::str_detect(DESCRIPTION," PARK AND RIDE ")!=T
         &stringr::str_detect(DESCRIPTION,"FROM ")!=T) %>%
  mutate( across(everything(), ~toupper(.x))) %>%
  mutate(no_words_street=str_count( DESCRIPTION,"\\w+"))
########################################

#########################################
###########CREATE STREET ABBREVIATIONS#################
tidy_street <- Street %>%
  unnest_tokens(word, DESCRIPTION) %>%   
  mutate(word=toupper(word))


str_abr<- read_csv("/conf/linkage/output/Martin_R/UPRN Seeding/2 - Ref Data/Street_abbrev2.csv")
str_abr<-as.data.frame(str_abr)
street_abr<-left_join(tidy_street,str_abr,by=c("word"="typ"))
street_abr<-street_abr %>% 
  select(USRN,abbr1,abbr2,abbr3,word) %>% 
  distinct()
#########################################

########################################
###########JOIN USRN TO ABBREVIATIONS###################
street2<-left_join(Street,street_abr,by="USRN",keep=FALSE)

street2<-street2 %>% 
  group_by(USRN) %>% 
  mutate(n_words=n()) %>% 
  mutate (abbrv_street1=if_else(!is.na(abbr1)&n_words>1,stringr::str_replace(DESCRIPTION,word,abbr1),""))%>% 
  mutate (abbrv_street2=if_else(!is.na(abbr2)&n_words>1,stringr::str_replace(DESCRIPTION,word,abbr2),""))%>%  
  mutate (abbrv_street3=if_else(!is.na(abbr3)&n_words>1,stringr::str_replace(DESCRIPTION,word,abbr3),""))%>% 
  mutate(abbrv_street=case_when(!is.na(abbr1)&is.na(abbr2)&is.na(abbr3)~paste0(abbrv_street1),
                                is.na(abbr1)&!is.na(abbr2)&is.na(abbr3)~paste0(abbrv_street2),
                                is.na(abbr1)&is.na(abbr2)&!is.na(abbr3)~paste0(abbrv_street3)))%>% 
  
  mutate(abbr_word_count=lengths(gregexpr("\\w+",abbrv_street1))) %>% 
  mutate(ABBR_STREET1=ifelse(abbr_word_count==n_words,abbrv_street1,"")) %>% 
  mutate(abbr_word_count2=lengths(gregexpr("\\w+",abbrv_street2))) %>% 
  mutate(ABBR_STREET2=ifelse(abbr_word_count2==n_words,abbrv_street2,"")) %>% 
  mutate(abbr_word_count3=lengths(gregexpr("\\w+",abbrv_street3))) %>% 
  mutate(ABBR_STREET3=ifelse(abbr_word_count3==n_words,abbrv_street3,"")) %>% 
  distinct() %>%
  mutate(count=n()) %>% 
  filter(count==1|(count>1&ABBR_STREET1!="")) %>% 
  
  select(USRN,DESCRIPTION,ABBR_STREET1,ABBR_STREET2,ABBR_STREET3,LOCALITY,TOWN) %>% 
  ungroup()
########################################

#######################################
############UPLOAD USRN ORACLE TABLE#####################
data.table::fwrite(street2,"/conf/linkage/output/Martin_R/UPRN Seeding/2 - Ref Data/Street_abbrev_extra.csv")

street2<-street2 %>% 
  mutate(across(everything(), ~ifelse(.x=='',NA,.x))) %>% 
  mutate(across(everything(), ~tidyr::replace_na (.x,'NULL'))) 

##optional sql table
dbWriteTable(conn = con2, #connection
             "USRN", #table name
             street2, #data
             overwrite = TRUE, #overwrite if it exists
             #options(odbc.batch_rows = 1000000),
             field.types = c(USRN = "VARCHAR2 (20)",
                             DESCRIPTION  = "VARCHAR2 (250)",
                             ABBR_STREET1 = "VARCHAR2 (250)",
                             ABBR_STREET2 = "VARCHAR2 (250)",
                             ABBR_STREET3 = "VARCHAR2 (250)",
                             LOCALITY   = "VARCHAR2 (250)",
                             TOWN       = "VARCHAR2 (250)"))
#######################################

######################################
###########JOIN USRN TO ADDRESSBASE###############
ad_base<-ad_base %>% 
  mutate(USRN=as.character(USRN))

full_neigh<-left_join(ad_base,street2,by="USRN")

full_neigh<-full_neigh %>%  distinct(ADDRESS_STRING_NOPC,.keep_all=T) %>% 
  select(UPRN ,PARENT_UPRN,USRN,MIGRATION_DATE,ADDRESS_LINE_1,ADDRESS_LINE_2,ADDRESS_LINE_3,POST_TOWN,POSTCODE,DESCRIPTION ,LATITUDE,LONGITUDE,
         ABBR_STREET1,ABBR_STREET2,ABBR_STREET3 ,TOWN,LOCALITY,CLASSIFICATION_TERTIARY,H3_4,H3_6,H3_7,H3_8,  H3_10,ADDRESS_STRING_NOPC,
         WORD_ONE,WORD_TWO,WORD_THREE,WORD_FOUR,WORD_FIVE,WORD_SIX,WORD_ONE_M,
         WORD_TWO_M,WORD_THREE_M,WORD_FOUR_M,WORD_FIVE_M,WORD_SIX_M,SUB_UNIT,BUILD_NO,ADD_NUM) %>% 
  mutate(across(everything(), ~ifelse(.x=='',NA,.x))) %>% 
  mutate(across(everything(), ~tidyr::replace_na (.x,'NULL'))) 
######################################

###############################################
##ISLAND, SETTLEMENT & LOCALITY GEOGRAPAHIES############
settlements2020Byhex10_SIMPLE <- readxl::read_excel("/conf/linkage/output/Martin_R/UPRN Seeding/2 - Ref Data/settlements2020Byhex10_SIMPLE.xlsx")
settlements2020Byhex10_SIMPLE <-settlements2020Byhex10_SIMPLE %>% 
  mutate(name=toupper(name)) %>% 
  rename(SETTLEMENT_NAME=name)
full_neigh2<-left_join(full_neigh,settlements2020Byhex10_SIMPLE,by=c("H3_10"="hex_id"))
###############################################

#############################
#######UPLOAD TO ORACLE DB##############################

#define username for SQL tables
user_name <- toupper(Sys.info()['user'])

dbWriteTable(conn = con2, #connection
             "AD_BASE", #table name
             full_neigh2, #data
             overwrite = TRUE, #overwrite if it exists
             #options(odbc.batch_rows = 1000000),
             field.types = c(UPRN = "VARCHAR2 (14)",                     
                             PARENT_UPRN="VARCHAR2 (14)",
                             USRN="VARCHAR2 (25)",
                             MIGRATION_DATE="VARCHAR2 (25)",
                             ADDRESS_LINE_1="VARCHAR2 (90)",       
                             ADDRESS_LINE_2="VARCHAR2 (90)",
                             ADDRESS_LINE_3="VARCHAR2 (90)",  
                             POST_TOWN="VARCHAR2 (40)",
                             BUILD_NO="VARCHAR2(250)",
                             SUB_UNIT="VARCHAR2(250)",
                             DESCRIPTION  = "VARCHAR2 (250)",
                             ABBR_STREET1 = "VARCHAR2 (250)",
                             ABBR_STREET2 = "VARCHAR2 (250)",
                             ABBR_STREET3 = "VARCHAR2 (250)",
                             TOWN= "VARCHAR2 (250)",
                             LOCALITY= "VARCHAR2 (250)",
                             CLASSIFICATION_TERTIARY= "VARCHAR2 (250)",
                             LATITUDE="VARCHAR2 (25)",
                             LONGITUDE="VARCHAR2 (25)",
                             H3_4="VARCHAR2(15)",
                             H3_6="VARCHAR2(15)",
                             H3_7="VARCHAR2(15)",
                             H3_8="VARCHAR2(15)",               
                             H3_10="VARCHAR2(15)",
                             ADDRESS_STRING_NOPC="VARCHAR2(250)",
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
                             WORD_SIX_M="VARCHAR2(250)"
             ))

#############################

#############################
######EXTRAS#####
#REMOVE UNHELPFUL COMMON WORDS'
dbGetQuery(con2, statement ="UPDATE AD_BASE SET ADDRESS_LINE_1 = regexp_replace(AD_BASE.ADDRESS_LINE_1 ,'AND |ROOM |FLAT|RM |FLT |FT |SUITE |BLOCK ', '');")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET ADDRESS_STRING_NOPC = regexp_replace(AD_BASE.ADDRESS_STRING_NOPC,'AND |ROOM|FLAT|RM|FLT|FT|SUITE|BLOCK', '');")
#COMPRESS REFERENCE ADDRESSBASE TABLE TO SAVE SPACE
dbGetQuery(con2, statement = "ALTER TABLE MARTIR03.AD_BASE COMPRESS") 
#INCREASE AVALIABILITY / PARALLELIZE ACCESS
dbGetQuery(con2, statement = "ALTER TABLE MARTIR03.AD_BASE PARALLEL 12") 
#GET UPPER AND LOWER RANGE WHERE THEY EXIST
dbGetQuery(con2, statement ="ALTER TABLE AD_BASE ADD range_lower varchar(250);")
dbGetQuery(con2, statement ="ALTER TABLE AD_BASE ADD range_upper varchar(250);")

dbGetQuery(con2, statement ="UPDATE AD_BASE SET range_LOWER =CAST(regexp_replace(regexp_substr(AD_BASE.ADD_num, '[^/]+[0-9]', 1, 1), '[^0-9]','')AS INT);")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET range_upper =CAST(regexp_replace(regexp_substr(AD_BASE.ADD_num, '[^/]+[0-9]', 1, 2), '[^0-9]','')AS INT);")
#ALTERNATIVE SEPARATOR SAME METHOD TO COLLECT LOWER AND UPPER RANGE WHERE APPLICABLE
dbGetQuery(con2, statement ="ALTER TABLE AD_BASE ADD range_lower2 varchar(250);")
dbGetQuery(con2, statement ="ALTER TABLE AD_BASE ADD range_upper2 varchar(250);")

dbGetQuery(con2, statement ="UPDATE AD_BASE SET range_LOWER2 =CAST(regexp_replace(regexp_substr(AD_BASE.build_no, '[^-]+[0-9]', 1, 1), '[^0-9]','')AS INT);")
dbGetQuery(con2, statement ="UPDATE AD_BASE SET range_upper2 =CAST(regexp_replace(regexp_substr(AD_BASE.build_no, '[^-]+[0-9]', 1, 2), '[^0-9]','')AS INT);")


#GRANT ACCESS FOR DATAMGT AND GIS TEAMS.

dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO DOUGLF02;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO TARACO01;")
#dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO IAINMA09;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO YEEHIL01;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO NATALM04;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO BEATAN01;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO ANDREG05;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO CLAREC01;")
dbGetQuery(con2, statement ="GRANT SELECT ON MARTIR03.AD_BASE TO HEATHL03;")
###########################

