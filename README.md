UPRN Seeding Workflow.
Premise 
A user has some text strings which are addresses with or without postcodes. There is an assumption here that the postcode does at least vaguely relate to the address string if present. The user wishes to assign UPRN (Unique property Reference number ) to gain benefit of detailed point level information and consistent addressing to potentially accurately group person level records into households.
UPRN Reference File
Our reference file for this sort of seeding is the Address base file. 
Address  base processing every 6 weeks in line with Address base file updates from the improvement service.
I have an R script : h3_matching_SQL_TABLE_FEB22.R
It reads in addressbase as a csv, using UPRN coordinates assigns H3 tiles form level 4,5,6 ,7,8,10 and 12 to each line. 
Filtering happens to remove proposed developments, and non residential and addressable objects we won’t need such as ATMs and other infrastructure.
Data cleaning is applied to standardise the addresses and remove Address base conventions you don’t see in “real life addresses”. Whole address line objects are created for later comparison. Numeric elements of addresses are separated as this is often the differentiating factor between many addresses this is a key metric.
The USRN dataset is reduced , cleaned, filtered for relevant roads to remove slip roads, motorways and other things like roundabouts footpaths and cycleways etc. Next Street abbrevation words file is read in and joined on the basis of street type. Eg avenue or road are street types and the associated abbreviation are rd and ave.
We generate a record for the full street types and any associated abbreviations again to help with joining later. We join this USRN dataset simply to the Address base dataset using the USRN held both sides as the key. All this goes up to the SQL table and further variables are derived on the way.
Resultant Address base table:
Name	Type	Description
UPRN	VARCHAR2(14)	Most unique key - not unique due to street descriptions being mulitplied due to abbreviations
PARENT_UPRN	VARCHAR2(14)	UPRN or Parent_UPRN if present
ADDRESS_LINE_1	VARCHAR2(90)	 
ADDRESS_LINE_2	VARCHAR2(90)	 
ADDRESS_LINE_3	VARCHAR2(90)	 
POST_TOWN	VARCHAR2(40)	 
POSTCODE	VARCHAR2(8)	 
IDENTIFIER	VARCHAR2(15)	 
LA_LOCATION	VARCHAR2(255)	 
ADDRESS_STRING	VARCHAR2(255)	 
LATITUDE	NUMBER(15)	 
LONGITUDE	NUMBER(15)	 
USRN	VARCHAR2(25)	Street Level key
CLASSIFICATION_LOCATION	VARCHAR2(255)	 
CLASSIFICATION_DESCR	VARCHAR2(250)	 
CLASSIFICATION_PRIMARY	VARCHAR2(255)	 
CLASSIFICATION_SECONDARY	VARCHAR2(255)	 
CLASSIFICATION_TERTIARY	VARCHAR2(255)	 
CLASSIFICATION_QUATERNARY	VARCHAR2(255)	 
H3_4	VARCHAR2(15)	 
H3_5	VARCHAR2(15)	 
H3_6	VARCHAR2(15)	 
H3_7	VARCHAR2(15)	 
H3_8	VARCHAR2(15)	 
H3_10	VARCHAR2(15)	 
H3_12	VARCHAR2(15)	 
ADDRESS_STRING_NOPC	VARCHAR2(250)	 
NUM1	VARCHAR2(255)	 
NUM2	VARCHAR2(255)	 
NUM	VARCHAR2(255)	 
ADD_STRING_WORKING	VARCHAR2(250)	 
DESCRIPTION	VARCHAR2(250)	Normal street name written in full
LOCALITY	VARCHAR2(250)	NRS Locality
TOWN	VARCHAR2(250)	NRS Town
ISLAND	VARCHAR2(250)	Islands
REC_TYPE	VARCHAR2(255)	 
DES_LEN	VARCHAR2(255)	 
ABBR_STREET	VARCHAR2(250)	Abbreviated street names
ABBRV_LEN	VARCHAR2(255)	 
ADD_NUM	VARCHAR2(250)	Numbers from address string
SUB_UNIT	VARCHAR2(250)	initial number parsed from address line assuming flat position is written first
BUILD_NO	VARCHAR2(250)	Building or street number assumption street position is written second
WORD_ONE	VARCHAR2(250)	Parsed words in order first to eighth
WORD_TWO	VARCHAR2(250)	 
WORD_THREE	VARCHAR2(250)	 
WORD_FOUR	VARCHAR2(250)	 
WORD_FIVE	VARCHAR2(250)	 
WORD_SIX	VARCHAR2(250)	 
WORD_SEVEN	VARCHAR2(250)	 
WORD_EIGHT	VARCHAR2(250)	 
POINT	PUBLIC.SDO_GEOMETRY	in development

Incoming data:
One of 2 r scripts can be used to get incoming addressing into a similar state / assign equivalent features as above. 2 flavours of this exist one for CHI addressing one for Care inspectorate data. Inn addition areas of neighbouring tiles are defined for increasing search from a given centroid.
CHI ADDRESSING: PRE_PROCESS_CHI_ADDRESSING_CREATE_SQL_TABLE_FOR SEEDING.R"
Care inspectorate: PRE_PROCESS_CARE.R
So Differences are Postcode centroids derived from supplied postcodes are used to assign H3 tiles to records.
Other than that the objective is to get comparable variables where possible form each string as we got from the Address base file. THEN the data goes the SQL table space.

SEEDING.
Written using PL SQL. This was chosen for scalability mainly. 
Use either : 
\\chi\Martin\address_matching\add_matching_CH2.sql
\\chi\Martin\address_matching\add_matching_GP2.sql
The approach is to generate a set of potential pairs in a temp table and then filter that table based on score and uniqueness of match to accept a certain subset of matches. Any accepted matches are then deleted from the incoming table being used for this exercise (another original can be kept) . By deleting accepted matches it avoids A finding the match again, reduces the size of the problem and allow us to go from most strict to less in match key hierarchy without letting in loads of false positives. These iterations of many match keys attempt to account for typical variations in incoming versus reference addressing.
Each iterations logic needs writing down and checking if it is in the optimal order, typically string searching and string distance metrics are combined with spatial proximity using the H3 tile hierarchy. Soundex, key word searching are also used in the care home script.
So a set of accepted matches are accumulated as a series of tables which are assigned a stage key for future traceability. These tables then are bought together by a UNION query and this holds the result. 
Unmatched records remain in the incoming table. The original load table remains to show the total job.
Notes Sometimes an address string can refer to a range of addresses 115-120 high street. 
Buildings get knocked down rebuilt and merged or split into smaller units so subunits can be confusing. 
Things can be the same on the ground but renumbered so what was flats 1,2,3,4 could become a,b,c,d. SO in short what seems easy isn’t nor it is easy to judge accuracy. I tried to code for the base case or the correct usage. Edge case get handled as late as possible. I generally don’t aim for 100% match as this is normally false even with very good data. Change is the only constant, Addressbase is updated 6 weekly. Results in theory then aren’t 100% repeatable with updated data. 
