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
library(readxl)
############################
user_folder<-"Martin_R/"
scripts<-"UPRN Seeding/1 - Scripts/"
ref_data<-"UPRN Seeding/2 - Ref Data/"
base<-"/conf/linkage/output/"
############################
#######MAKE A CONNECTION##########
source(glue("{base}{user_folder}{scripts}/Make_SMRA_Connection_v1.R"))
############################

incoming<-fread(glue("{base}{user_folder}UPRN Seeding/care_homes_bk/Feb update/new_unique_res_add_2023_02_08.csv"))
incoming<-incoming %>% 
  mutate(ID=as.character(ID))

#######Link rate plus workflow checks#######
number_of_recs<-dbGetQuery(con, statement ="SELECT DISTINCT ID FROM ADD_IN")

number_of_recs_seeded<-dbGetQuery(con, statement ="SELECT * FROM UPRNSEEDING2")

#number_of_recs_unseeded<-dbGetQuery(con, statement ="SELECT DISTINCT ID FROM UNSEEDED")
#or if ch_matching.sql
number_of_recs_unseeded<-dbGetQuery(con, statement ="SELECT DISTINCT ID FROM SECOND_A")


total <-rbind(number_of_recs_unseeded,number_of_recs_seeded)
####
final_df<-left_join(incoming,number_of_recs_seeded,by="ID")

final_df<-final_df %>% 
  distinct(ID,.keep_all=T)
fwrite(final_df,glue("{base}{user_folder}UPRN Seeding/care_homes_bk/Feb update/new_unique_res_add_2023_02_08_UPRN_Seeding_result.csv"))
#######EVALUATE RESULTS#######

##GENERATE STAGE MATCH COUNT FROM RESULTS UNION TABLE.

links_by_stage2<-dbGetQuery(con, statement ="SELECT DISTINCT 
STAGE, COUNT( ID) OVER(PARTITION BY STAGE ORDER BY stage) AS COUNT_MATCHES
FROM UPRNSEEDING ORDER BY STAGE;")

seeded_data<-dbGetQuery(con, statement ="SELECT * FROM uprnseeding;")
write.csv(seeded_data,"/conf/linkage/output/Martin_R/UPRN Seeding/CHI_care_Feb_2023.csv")
########For creation of weighted sampling by stage

set.seed(1234) # ensures same sample/repeatable
seeded_data_sample<-seeded_data %>% 
  group_by(STAGE) %>% 
  mutate(STAGE_Count = n()) %>%
  mutate(five_percent=round(STAGE_Count*0.10)) %>% 
  sample_n(five_percent) %>% 
  ungroup()

write.csv(seeded_data_sample,"/conf/linkage/output/Martin_R/UPRN Seeding/CHI_care_sample.csv")

##REVIEW SAMPLES 5-10% NORMALLY DEPENDING ON TIME AND SIZE

####if you want lat n long from seeded results### BIT MORE WORK NEEDED HERE

results<-dbGetQuery(con, statement ="SELECT distinct
                    martir03.uprnseeding.id,
                    martir03.uprnseeding.stage,
                    martir03.uprnseeding.address_string,
                    martir03.uprnseeding.address_string_nopc,
                    martir03.ad_base.latitude,
                    martir03.ad_base.longitude
                    FROM
                    martir03.uprnseeding, martir03.ad_base
                    where martir03.uprnseeding.uprn = martir03.ad_base.uprn")


unseeded<-dbGetQuery(con, statement ="SELECT * from UNSEEDED") #or second_a


#OR READ IN spd
POSTCODES<-dbGetQuery(con, statement ="SELECT DISTINCT
                      POSTCODE_2022_1.POSTCODE,
                      POSTCODE_2022_1.LATITUDE,
                      POSTCODE_2022_1.LONGITUDE
                      FROM martir03.POSTCODE_2022_2;")

POSTCODES<-POSTCODES %>% 
  mutate(POSTCODE=phsmethods::postcode(POSTCODE))

UNSEEDED_PC<-left_join(unseeded,POSTCODES,by=c("CURRENT_POSTCODE"="POSTCODE"))

############################


