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
  mutate(CURRENT_LINE2=sub("^(FLAT 0-)","0/",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=gsub("-","/",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE2=sub("^(FLAT |^F )","",CURRENT_LINE2)) %>% 
  mutate(CURRENT_LINE3=gsub("@"," ",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=gsub(","," ",CURRENT_LINE3)) %>%  
  mutate(CURRENT_LINE3=gsub("\\."," ",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=gsub("'"," ",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=sub("(C/O)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C-O)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C-0)\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("(C/0)+\\s[A-Z]*","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C/0)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C/O)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C-0)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(C-O)","",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("  "," ",CURRENT_LINE3)) %>%
  mutate(CURRENT_LINE3=sub("^(FLAT 0-)","0/",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=gsub("-","/",CURRENT_LINE3)) %>% 
  mutate(CURRENT_LINE3=sub("^(FLAT |^F )","",CURRENT_LINE3)) %>% 
  mutate(ADDRESS_STRING_NOPC=gsub(","," ",ADDRESS_STRING_NOPC)) %>%
  mutate(ADDRESS_STRING_NOPC=gsub("\\."," ",ADDRESS_STRING_NOPC)) %>%
  mutate(ADDRESS_STRING_NOPC=gsub("'"," ",ADDRESS_STRING_NOPC)) %>%
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
  mutate(ADDRESS_STRING_NOPC=str_replace_all(ADDRESS_STRING_NOPC,'\\)+|\\(+',' ')) %>% #remove extra single brackets
  mutate(ADD_NUM=stringr::str_extract(ADDRESS_STRING_NOPC,"^.*[[:digit:]]+[[:alpha:]]?[[:digit:]]?+|^.*[[:digit:]]+[[:alpha:]]|^.*[[:digit:]]|[[:digit:]].*[[:digit:]]+[[:alpha:]]*|[[:digit:]]+[[:alpha:]]|(LEVEL |LVL |FLOOR |FLR |UNIT |ROOM |RM |FLAT |FLT |APT |APARTMENT |SUITE )+.*[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+'")) %>% 
  mutate(BUILD_NO=word(ADD_NUM,-1)) %>% 
  mutate(BUILD_NO2=stringr::str_extract(BUILD_NO,"^.*[[:digit:]]+[[:alpha:]]?[[:digit:]]?+|^.*[[:digit:]]+[[:alpha:]]|^.*[[:digit:]]|[[:digit:]].*[[:digit:]]+[[:alpha:]]*|[[:digit:]]+[[:alpha:]]
                                        |[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+'")) %>% 
  mutate(BUILD_NO2=trimws (BUILD_NO2)) %>% 
  mutate(SUB_UNIT=substring(ADD_NUM,1,(nchar(ADD_NUM))-(nchar(BUILD_NO)))) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="UNIT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(across(everything(), ~ifelse(.x=='',NA,.x))) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="FLAT ",BUILD_NO,SUB_UNIT)) %>% # NEW
  mutate(SUB_UNIT=if_else(SUB_UNIT=="UNIT",BUILD_NO,SUB_UNIT)) %>% # NEWmutate(SUB_UNIT=if_else(SUB_UNIT==" UNIT",BUILD_NO,SUB_UNIT)) %>% # NEW
  mutate(SUB_UNIT=if_else(SUB_UNIT=="ROOM ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="CARAVAN ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="SUITE ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="OFFICE ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="SHOP ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="CHALET ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="APARTMENT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(SUB_UNIT=if_else(SUB_UNIT=="BEDSIT ",BUILD_NO,SUB_UNIT)) %>% 
  mutate(sub_unit2=stringr::str_extract(SUB_UNIT,"^.*[[:digit:]]+[[:alpha:]]?[[:digit:]]?+|^.*[[:digit:]]+[[:alpha:]]|^.*[[:digit:]]|[[:digit:]].*[[:digit:]]+[[:alpha:]]*|[[:digit:]]+[[:alpha:]]
                                        |[[:digit:]]|[[:digit:]].*[[:digit:]][[:alpha:]]*|[[:digit:]]+'")) %>% 
  mutate(sub_unit2=sub("(^FLAT |^F |^HOUSE |^BLOCK|^UNIT|^APARTMENT|^|APT|^ROOM )","",sub_unit2)) %>%
  mutate(sub_unit2=trimws (sub_unit2)) %>% 
  #SWTICH FLATS WHERE FLAT POSTITION WRITTEN SECOND NOT FIRST. 
  mutate(BUILD_NO=case_when(BUILD_NO=="1F1"~SUB_UNIT,
                            BUILD_NO=="1F2"~SUB_UNIT,
                            BUILD_NO=="2F1"~SUB_UNIT,
                            BUILD_NO=="2F2"~SUB_UNIT,
                            BUILD_NO=="3F1"~SUB_UNIT,
                            BUILD_NO=="3F2"~SUB_UNIT,
                            BUILD_NO=="3F1"~SUB_UNIT,
                            BUILD_NO=="3F2"~SUB_UNIT,
                            BUILD_NO=="4F1"~SUB_UNIT,
                            BUILD_NO=="4F2"~SUB_UNIT,
                            BUILD_NO=="1/1"~SUB_UNIT,
                            BUILD_NO=="1/2"~SUB_UNIT,
                            BUILD_NO=="2/1"~SUB_UNIT,
                            BUILD_NO=="2/2"~SUB_UNIT,
                            BUILD_NO=="3/1"~SUB_UNIT,
                            BUILD_NO=="3/2"~SUB_UNIT,
                            BUILD_NO=="3/1"~SUB_UNIT,
                            BUILD_NO=="3/2"~SUB_UNIT,
                            BUILD_NO=="4/1"~SUB_UNIT,
                            BUILD_NO=="4/2"~SUB_UNIT,
                            TRUE ~ BUILD_NO) )%>% 
  mutate(SUB_UNIT=case_when(BUILD_NO2=="1F1"~BUILD_NO2,
                            BUILD_NO2=="1F2"~BUILD_NO2,
                            BUILD_NO2=="2F1"~BUILD_NO2,
                            BUILD_NO2=="2F2"~BUILD_NO2,
                            BUILD_NO2=="3F1"~BUILD_NO2,
                            BUILD_NO2=="3F2"~BUILD_NO2,
                            BUILD_NO2=="3F1"~BUILD_NO2,
                            BUILD_NO2=="3F2"~BUILD_NO2,
                            BUILD_NO2=="4F1"~BUILD_NO2,
                            BUILD_NO2=="4F2"~BUILD_NO2,
                            BUILD_NO2=="1/1"~BUILD_NO2,
                            BUILD_NO2=="1/2"~BUILD_NO2,
                            BUILD_NO2=="2/1"~BUILD_NO2,
                            BUILD_NO2=="2/2"~BUILD_NO2,
                            BUILD_NO2=="3/1"~BUILD_NO2,
                            BUILD_NO2=="3/2"~BUILD_NO2,
                            BUILD_NO2=="3/1"~BUILD_NO2,
                            BUILD_NO2=="3/2"~BUILD_NO2,
                            BUILD_NO2=="4/1"~BUILD_NO2,
                            BUILD_NO2=="4/2"~BUILD_NO2,
                            TRUE~SUB_UNIT)) %>% 
  mutate(ADD_NUM=str_replace_all(ADD_NUM,'\\)+|\\(+','')) %>% 
  mutate(ADD_STRING_WORKING=if_else(!is.na(ADD_NUM),str_replace_all(ADDRESS_STRING_NOPC,ADD_NUM,""),ADDRESS_STRING_NOPC)) %>% 
  mutate(ADD_STRING_WORKING=trimws (ADD_STRING_WORKING)) %>% 
  mutate(ADD_STRING_WORKING=str_replace_all(ADD_STRING_WORKING, "[^[:alpha:]][:whitespace:]", " ")  ) %>% 
  arrange(BUILD_NO,SUB_UNIT) %>% 
  # DOUBLE CHECK RECORD 1 BUILD_NO SO UPLOAD WORKS.
  select(ID,CURRENT_POSTCODE,CURRENT_LINE1,CURRENT_LINE2,CURRENT_LINE3,
         ADD_STRING_WORKING,ADDRESS_STRING,ADD_NUM, SUB_UNIT, BUILD_NO,H3_4_P,H3_5_P,H3_6_P,H3_7_P,H3_8_P,
         H3_9_P,H3_10_P,NEIGHBOURS,NEAR_NEIGHBOURS,FAR_NEIGHBOURS,
         VFAR_NEIGHBOURS,WORD_ONE,WORD_TWO,WORD_THREE,WORD_FOUR,WORD_FIVE,WORD_SIX,
         WORD_ONE_M,WORD_TWO_M,WORD_THREE_M,WORD_FOUR_M,WORD_FIVE_M,WORD_SIX_M)