library(readxl)
library(dplyr) #apply filters
library(tidyverse)#most used library

setwd("C:/Users/aurel/Desktop/Portfolio/Data management projects/SQL configuration table modification")

configPIDI <- readxl::read_xlsx("FA PI DI opti new version.xlsx", sheet="Sheet1")

# Subset the data
filteredconfig <- subset(configPIDI)

# Retrieve Unique Items from INPUTCUBEID Column
unique(configPIDI$INPUTCUBEID)

# Creating subsets based on conditions for SMSEO_BOP_CSDB
# 1. Subset where FUNCTIONAL_CAT is not 'D1'
SMSEO <- subset(filteredconfig, INPUTCUBEID == 'SMSEO_BOP_CSDB')
SMSEODP <- subset(SMSEO, FUNCTIONAL_CAT != "'D1'")
# 2. Subset where FUNCTIONAL_CAT is 'D1'
SMSEOD1 <- subset(SMSEO, FUNCTIONAL_CAT == "'D1'")

# Change the modalities of FUNCTIONAL_CAT to a fixed modality
SMSEODP$FUNCTIONAL_CAT <- 'HOLD(FUNCTIONAL_CAT)'

# Create a matrix of the data set, changing the filter column based on the GSUB definition
exam <- apply(SMSEODP, 2, function(x) gsub("FUNCTIONAL_CAT='D'|FUNCTIONAL_CAT='P'", "FUNCTIONAL_CAT IN ('D','P')", x))

# Change the matrix to a data set
SMESEODPmod <- as.data.frame(exam)

# Remove duplicates based on FILTER column
SMESEODPmod1 <- SMESEODPmod[!duplicated(SMESEODPmod$FILTER), ]

# Combine modified subset and SMSEOD1
SMESEOFINAL <- rbind(SMESEODPmod1, SMSEOD1)

# Subsetting data to keep only TA and remove unwanted tables
REfilter <- subset(configPIDI, INPUTCUBEID != 'SMSEO_BOP_CSDB' & INPUTCUBEID != 'EA_BOP_ADJ')
adj <- subset(configPIDI, INPUTCUBEID == 'EA_BOP_ADJ')

# Combine subsets to create the aggregated dataset
aggregated <- rbind(SMESEOFINAL, REfilter, adj)


# TABLE 6 Operations

# Creating a subset of TA NFC 006
TANFC006 <- subset(aggregated, INPUTCUBEID == 'TANFC_006_UNI_BR_GR')

# Extracting the target string from the FILTER column
extracted_data <- TANFC006 %>% mutate(TYPINS = str_extract(FILTER, "'52790','52790_1','52800','52800_1'|'52810','52810_1'"))

# Removing unnecessary columns for the new rule (keeping only TYPINS)
extracted_data <- subset(extracted_data, select = c(TYPINS, INSTR_ASSET))

# Checking instr_asset - reorganizing the filter but modifying the TYPINS (in 1 row)
TANFC006 <- TANFC006 %>% mutate(FILTER = gsub("'52790','52790_1','52800','52800_1'", "'52790','52790_1','52800','52800_1','52810','52810_1'", FILTER))

# Create a rule id for F511 and F512

# Separate values in TYPINS using a comma as a separator
extracted_data <- extracted_data %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_data <- extracted_data %>% mutate(TYPINS = gsub("'", "", TYPINS))
extracted_data <- extracted_data %>% mutate(INSTR_ASSET = gsub("'", "", INSTR_ASSET))

# Remove duplicates
extracted_data <- extracted_data %>% distinct()

# Dropping F5
extracted_data <- extracted_data[extracted_data$INSTR_ASSET != 'F5',]

# Creating the new rule
NEWRULEINSTRASSET <- extracted_data %>%  mutate(CONVRULE = "INSTRASSET1", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Changing the column names
colnames(NEWRULEINSTRASSET)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET)[2] = "ELEMENTOUT"

# Replacing F511 and F512 by conv rule
TANFC006$INSTR_ASSET[TANFC006$INSTR_ASSET == "'F512'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC006$INSTR_ASSET[TANFC006$INSTR_ASSET == "'F511'"] <- "CONV('INSTRASSET1', TYPINS)"

# Creating the final dataset by excluding specific TYPINS values
TANFC006final <- TANFC006[!grepl("TYPINS IN \\('52810','52810_1'\\)", TANFC006$FILTER), ]

# TABLE 7 Operations

# Creating a subset of TA NFC 007
TANFC007 <- subset(aggregated, INPUTCUBEID == 'TANFC_007_UNI_BR_GR')

# Extracting the target string from the FILTER column
extracted_dataTA007 <- TANFC007 %>%
  mutate(TYPINS = str_extract(FILTER, "'52250','52250_1','52240','52240_1'|'52260','52260_1'"))

# Removing unnecessary columns for the new rule (keeping only TYPINS)
extracted_dataTA007 <- subset(extracted_dataTA007, select = c(TYPINS, INSTR_ASSET))

# Checking instr_asset - reorganizing the filter but modifying the TYPINS (in 1 row)
# Remove the unique '52260' and replace '52260','52260_1' with the full set
TANFC007 <- TANFC007 %>% mutate(FILTER = gsub("'52250','52250_1','52240','52240_1','52260'","",FILTER))
TANFC007 <- TANFC007 %>% mutate(FILTER = gsub("'52260','52260_1'","''52250','52250_1','52240','52240_1','52260','52260_1'",FILTER))
TANFC007 <- TANFC007 %>% mutate(FILTER = gsub("TYPINS IN \\(\\)", "TYPINS IN ('52250','52250_1','52240','52240_1','52260','52260_1')",FILTER))

# Delete rows with 'F511'
TANFC007 <- TANFC007 %>% filter(INSTR_ASSET != "'F511'")

# Check 52260 issue, it should be 'F511' only and 'F5'

# Create a rule id for F511 and F512

# Dropping N/A TYPINS
extracted_dataTA007 <- extracted_dataTA007 %>% drop_na(TYPINS) 

# Separate values in TYPINS using a comma as a separator
extracted_dataTA007 <- extracted_dataTA007 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_dataTA007 <- extracted_dataTA007 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_dataTA007 <- extracted_dataTA007 %>% mutate(INSTR_ASSET = gsub("'","",INSTR_ASSET))

# Remove duplicates
extracted_dataTA007 <- extracted_dataTA007 %>% distinct()

# Dropping 'F5'
extracted_dataTA007 <- extracted_dataTA007[extracted_dataTA007$INSTR_ASSET != 'F5',]

# Inputting in INSTASSET RULE  
NEWRULEINSTRASSET7 <- extracted_dataTA007 %>%  mutate(CONVRULE = "INSTRASSET1", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Changing the column names
colnames(NEWRULEINSTRASSET7)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET7)[2] = "ELEMENTOUT"

# Replacing 'F511' and 'F512' by new CONV Rule 
TANFC007$INSTR_ASSET[TANFC007$INSTR_ASSET == "'F512'"] <- "CONV('INSTRASSET1', TYPINS)"

# TABLE 9 Operations

# Creating a subset of TA NFC 009
TANFC009 <- subset(aggregated, INPUTCUBEID == 'TANFC_009_UNI_BR_GR')

# Extracting the target string from the FILTER column

# Use regex to extract everything between "TYPINS IN (" and ") AND"
extracted_typinsTA009 <- TANFC009 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\) AND") %>%
      str_remove("^TYPINS IN \\(") %>%
      str_remove("\\) AND$")
  )

# Removing unnecessary columns for the new rule (keeping only TYPINS)
extracted_dataTA009 <- subset(extracted_typinsTA009, select = c(TYPINS, INSTR_ASSET))

# Create a rule id for F511 and F512

# Dropping N/A TYPINS
extracted_dataTA009 <- extracted_dataTA009 %>% drop_na(TYPINS) 

# Separate values in TYPINS using a comma as a separator
extracted_dataTA009 <- extracted_dataTA009 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_dataTA009 <- extracted_dataTA009 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_dataTA009 <- extracted_dataTA009 %>% mutate(INSTR_ASSET = gsub("'","",INSTR_ASSET))

# Remove duplicates
extracted_dataTA009 <- extracted_dataTA009 %>% distinct()

# Dropping 'F5' and 'F5A'
# extracted_dataTA009 <- extracted_dataTA009[extracted_dataTA009$INSTR_ASSET != 'F5',]
# extracted_dataTA009 <- extracted_dataTA009[extracted_dataTA009$INSTR_ASSET != 'F5A',]

# Inputting in INSTASSET RULE  
NEWRULEINSTRASSET9 <- extracted_dataTA009 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Modify CONVRULE based on conditions
NEWRULEINSTRASSET9 <- NEWRULEINSTRASSET9 %>%
  mutate(
    CONVRULE = case_when(
      INSTR_ASSET == "F5" ~ "RTYP17",
      INSTR_ASSET == "F511" ~ "INSTRASSET1",
      INSTR_ASSET == "F512" ~ "INSTRASSET1",
      INSTR_ASSET == "F5A" ~ "RTYP18",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEINSTRASSET9)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET9)[2] = "ELEMENTOUT"

# Replacing 'F511' and 'F512' by new CONV Rule 
TANFC009$INSTR_ASSET[TANFC009$INSTR_ASSET == "'F5'"] <- "CONV('RTYP17', TYPINS)"
TANFC009$INSTR_ASSET[TANFC009$INSTR_ASSET == "'F511'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC009$INSTR_ASSET[TANFC009$INSTR_ASSET == "'F512'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC009$INSTR_ASSET[TANFC009$INSTR_ASSET == "'F5A'"] <- "CONV('RTYP18', TYPINS)"


# this is good normally


# TABLE 9 Additional Operations

# Creating a subset of TA NFC 009
TANFC009 <- subset(aggregated, INPUTCUBEID == 'TANFC_009_UNI_BR_GR')

# Extracting the target string from the FILTER column

# Use regex to extract everything between "TYPINS IN (" and ") AND"
extracted_typinsTA009 <- TANFC009 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\) AND") %>%
      str_remove("^TYPINS IN \\(") %>%
      str_remove("\\) AND$")
  )

# Functional_cat operations

# Extracting relevant columns for functional_cat
extracted_funccatTA009 <- subset(extracted_typinsTA009, select = c(TYPINS, FUNCTIONAL_CAT))


# Separate values in TYPINS using comma as a separator
extracted_funccatTA009 <- extracted_funccatTA009 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_funccatTA009 <- extracted_funccatTA009 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_funccatTA009 <- extracted_funccatTA009 %>% mutate(FUNCTIONAL_CAT = gsub("'","",FUNCTIONAL_CAT))

# Remove duplicates
extracted_funccatTA009 <- extracted_funccatTA009 %>% distinct()

# Inputting in INSTASSET RULE  
NEWRULEFUNCTIONALCAT9 <- extracted_funccatTA009 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Modify CONVRULE based on conditions
NEWRULEFUNCTIONALCAT9 <- NEWRULEFUNCTIONALCAT9 %>%
  mutate(
    CONVRULE = case_when(
      FUNCTIONAL_CAT == "D" ~ "RTYP27",
      FUNCTIONAL_CAT == "U2" ~ "RTYP29",
      FUNCTIONAL_CAT == "U1" ~ "RTYP28",
      FUNCTIONAL_CAT == "D1" ~ "RTYP30",
      FUNCTIONAL_CAT == "D2" ~ "RTYP31",
      FUNCTIONAL_CAT == "D3" ~ "RTYP32",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEFUNCTIONALCAT9)[1] = "ELEMNTIN" 
colnames(NEWRULEFUNCTIONALCAT9)[2] = "ELEMENTOUT" 

functionalcat_rule <- NEWRULEFUNCTIONALCAT9

# Remove duplicates
NEWRULEFUNCTIONALCAT9 <- NEWRULEFUNCTIONALCAT9 %>% distinct()

# Replacing F511 and F512 by new CONV Rule 
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'D'"] <- "CONV('RTYP27', TYPINS)"
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'U1'"] <- "CONV('RTYP28', TYPINS)"
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'U2'"] <- "CONV('RTYP29', TYPINS)"
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'D1'"] <- "CONV('RTYP30', TYPINS)"
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'D2'"] <- "CONV('RTYP31', TYPINS)"
TANFC009$FUNCTIONAL_CAT[TANFC009$FUNCTIONAL_CAT == "'D3'"] <- "CONV('RTYP32', TYPINS)"

# Duplicate removal operations

duplicate_removal <- subset(TANFC009)

# Extracting TYPINS from FILTER and performing group-wise operations
duplicate_removal <- duplicate_removal %>%
  mutate(TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\)") %>% str_replace("TYPINS IN \\(", "") %>% str_replace("\\)", "") %>% trimws(),
         FILTER = NULL) %>%
  group_by(FUNCTIONAL_CAT, INSTR_ASSET, FLOW_STOCK_ENTRY, VALUATION) %>%
  summarize(TYPINS = paste(unique(TYPINS), collapse = ","), .groups = 'drop') %>%
  left_join(duplicate_removal, by = c("FUNCTIONAL_CAT", "INSTR_ASSET", "FLOW_STOCK_ENTRY", "VALUATION"))

# Modify FILTER to replace TYPINS with the extracted values
duplicate_removal <- duplicate_removal %>%
  mutate(
    FILTER = str_replace(FILTER, "TYPINS IN \\(.*?\\) AND", "TYPINS IN () AND")
  )

duplicate_removal <- duplicate_removal %>%
  mutate(FILTER = str_replace(FILTER, "(TYPINS IN \\(.*?\\) AND)", paste0("TYPINS IN (", TYPINS, ") AND"))
  ) %>%
  select(-TYPINS)

# Remove duplicates
TANFC009 <- distinct(duplicate_removal)


# TABLE 10 Additional Operations

# Creating a subset of TA NFC 010
TANFC010 <- subset(aggregated, INPUTCUBEID == 'TANFC_010_UNI_BR_GR')

# Extracting the target string from the FILTER column

# Use regex to extract everything between "TYPINS IN (" and ") AND"
extracted_typinsTA010 <- TANFC010 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\) AND") %>%
      str_remove("^TYPINS IN \\(") %>%
      str_remove("\\) AND$")
  )

# Removing unnecessary columns for the new rule (keeping only TYPINS and instr assets)
extracted_dataTA010 <- subset(extracted_typinsTA010, select = c(TYPINS, INSTR_ASSET))

# Create a rule ID

# Dropping N/A TYPINS
extracted_dataTA010 <- extracted_dataTA010 %>% drop_na(TYPINS)

# Separate values in TYPINS using a comma as a separator
extracted_dataTA010 <- extracted_dataTA010 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_dataTA010 <- extracted_dataTA010 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_dataTA010 <- extracted_dataTA010 %>% mutate(INSTR_ASSET = gsub("'","",INSTR_ASSET))

# Remove duplicates
extracted_dataTA010 <- extracted_dataTA010 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEINSTRASSET10 <- extracted_dataTA010 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Modify CONVRULE based on conditions
NEWRULEINSTRASSET10 <- NEWRULEINSTRASSET10 %>%
  mutate(
    CONVRULE = case_when(
      INSTR_ASSET == "F5" ~ "RTYP17",
      INSTR_ASSET == "F511" ~ "INSTRASSET1",
      INSTR_ASSET == "F512" ~ "INSTRASSET1",
      INSTR_ASSET == "F5A" ~ "RTYP18",
      INSTR_ASSET == "F522" ~ "RTYP20",
      INSTR_ASSET == "F521" ~ "RTYP21",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEINSTRASSET10)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET10)[2] = "ELEMENTOUT"

# Replacing F511 and F512 by new CONV Rule 
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F5'"] <- "CONV('RTYP17', TYPINS)"
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F511'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F512'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F5A'"] <- "CONV('RTYP18', TYPINS)"
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F522'"] <- "CONV('RTYP20', TYPINS)"
TANFC010$INSTR_ASSET[TANFC010$INSTR_ASSET == "'F521'"] <- "CONV('RTYP21', TYPINS)"



# TABLE 10 Functional_cat Rule Additional Operations

# Extracting the functional_cat rule from the extracted_typinsTA010
extracted_funccatTA010 <- subset(extracted_typinsTA010, select = c(TYPINS, FUNCTIONAL_CAT))

# Create a rule ID

# Dropping N/A TYPINS
extracted_funccatTA010 <- extracted_funccatTA010 %>% drop_na(TYPINS)

# Separate values in TYPINS using a comma as a separator
extracted_funccatTA010 <- extracted_funccatTA010 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_funccatTA010 <- extracted_funccatTA010 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_funccatTA010 <- extracted_funccatTA010 %>% mutate(FUNCTIONAL_CAT = gsub("'","",FUNCTIONAL_CAT))

# Remove duplicates
extracted_funccatTA010 <- extracted_funccatTA010 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEFUNCTIONALCAT10 <- extracted_funccatTA010 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Identify unique values in FUNCTIONAL_CAT
unique(NEWRULEFUNCTIONALCAT10$FUNCTIONAL_CAT)

# Modify CONVRULE based on conditions
NEWRULEFUNCTIONALCAT10 <- NEWRULEFUNCTIONALCAT10 %>%
  mutate(
    CONVRULE = case_when(
      FUNCTIONAL_CAT == "D" ~ "RTYP27",
      FUNCTIONAL_CAT == "U2" ~ "RTYP29",
      FUNCTIONAL_CAT == "U1" ~ "RTYP28",
      FUNCTIONAL_CAT == "D1" ~ "RTYP30",
      FUNCTIONAL_CAT == "D2" ~ "RTYP31",
      FUNCTIONAL_CAT == "D3" ~ "RTYP32",
      FUNCTIONAL_CAT == "P" ~ "RTYP33",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEFUNCTIONALCAT10)[1] = "ELEMNTIN" 
colnames(NEWRULEFUNCTIONALCAT10)[2] = "ELEMENTOUT"

# Replacing by new CONV Rule 
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'D'"] <- "CONV('RTYP27', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'U1'"] <- "CONV('RTYP28', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'U2'"] <- "CONV('RTYP29', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'D1'"] <- "CONV('RTYP30', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'D2'"] <- "CONV('RTYP31', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'D3'"] <- "CONV('RTYP32', TYPINS)"
TANFC010$FUNCTIONAL_CAT[TANFC010$FUNCTIONAL_CAT == "'P'"] <- "CONV('RTYP33', TYPINS)"

# Handle duplicate removal for functional_cat
duplicate_removal10 <- subset(TANFC010)

duplicate_removal10 <- duplicate_removal10 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\)") %>% str_replace("TYPINS IN \\(", "") %>% str_replace("\\)", "") %>% trimws(),
    FILTER = NULL
  ) %>%
  group_by(FUNCTIONAL_CAT, INSTR_ASSET, FLOW_STOCK_ENTRY, VALUATION) %>%
  summarize(TYPINS = paste(unique(TYPINS), collapse = ","), .groups = 'drop') %>%
  left_join(duplicate_removal10, by = c("FUNCTIONAL_CAT", "INSTR_ASSET", "FLOW_STOCK_ENTRY", "VALUATION"))

duplicate_removal10 <- duplicate_removal10 %>%
  mutate(
    FILTER = str_replace(FILTER, "TYPINS IN \\(.*?\\) AND", "TYPINS IN () AND")
  )

duplicate_removal10 <- duplicate_removal10 %>%
  mutate(
    FILTER = str_replace(FILTER, "(TYPINS IN \\(.*?\\) AND)", paste0("TYPINS IN (", TYPINS, ") AND"))
  ) %>%
  select(-TYPINS)

TANFC010<- distinct(duplicate_removal10)



# TABLE 11 INSTRASSET Rule Additional Operations

TANFC011 <- subset(aggregated, INPUTCUBEID == 'TANFC_011_UNI_BR_GR')

# Extracting the target string from the FILTER column
extracted_typinsTA011 <- TANFC011 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\) AND") %>%
      str_remove("^TYPINS IN \\(") %>%
      str_remove("\\) AND$")
  )

# Removing unnecessary columns for the new rule (keeping only TYPINS and instr assets)
extracted_dataTA011 <- subset(extracted_typinsTA011, select = c(TYPINS, INSTR_ASSET))

# Create a rule ID

# Dropping N/A TYPINS
extracted_dataTA011 <- extracted_dataTA011 %>% drop_na(TYPINS)

# Separate values in TYPINS using comma as a separator
extracted_dataTA011 <- extracted_dataTA011 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_dataTA011 <- extracted_dataTA011 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_dataTA011 <- extracted_dataTA011 %>% mutate(INSTR_ASSET = gsub("'","",INSTR_ASSET))

# Remove duplicates
extracted_dataTA011 <- extracted_dataTA011 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEINSTRASSET11 <- extracted_dataTA011 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Identify unique values in INSTR_ASSET
unique(NEWRULEINSTRASSET11$INSTR_ASSET)

# Modify CONVRULE based on conditions
NEWRULEINSTRASSET11 <- NEWRULEINSTRASSET11 %>%
  mutate(
    CONVRULE = case_when(
      INSTR_ASSET == "FL" ~ "RTYP26",
      INSTR_ASSET == "F3" ~ "RTYP35",
      INSTR_ASSET == "F4" ~ "RTYP36",
      INSTR_ASSET == "F5" ~ "RTYP17",
      INSTR_ASSET == "F511" ~ "INSTRASSET1",
      INSTR_ASSET == "F512" ~ "INSTRASSET1",
      INSTR_ASSET == "F5B" ~ "RTYP37",
      INSTR_ASSET == "F581" ~ "RTYP38",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEINSTRASSET11)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET11)[2] = "ELEMENTOUT"

# Replacing by new CONV Rule 
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'FL'"] <- "CONV('RTYP26', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F3'"] <- "CONV('RTYP35', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F4'"] <- "CONV('RTYP36', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F5'"] <- "CONV('RTYP17', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F511'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F512'"] <- "CONV('INSTRASSET1', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F5B'"] <- "CONV('RTYP37', TYPINS)"
TANFC011$INSTR_ASSET[TANFC011$INSTR_ASSET == "'F581'"] <- "CONV('RTYP38', TYPINS)"

# TABLE 11 Functional_cat Rule Additional Operations

####### functional_cat rule
extracted_funccatTA011 <- subset(extracted_typinsTA011, select = c(TYPINS, FUNCTIONAL_CAT))

# Create a rule ID

# Dropping N/A TYPINS
extracted_funccatTA011 <- extracted_funccatTA011 %>% drop_na(TYPINS)

# Separate values in TYPINS using comma as a separator
extracted_funccatTA011 <- extracted_funccatTA011 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_funccatTA011 <- extracted_funccatTA011 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_funccatTA011 <- extracted_funccatTA011 %>% mutate(FUNCTIONAL_CAT = gsub("'","",FUNCTIONAL_CAT))

# Remove duplicates
extracted_funccatTA011 <- extracted_funccatTA011 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEFUNCTIONALCAT11 <- extracted_funccatTA011 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Identify unique values in FUNCTIONAL_CAT
unique(NEWRULEFUNCTIONALCAT11$FUNCTIONAL_CAT)

# Modify CONVRULE based on conditions
NEWRULEFUNCTIONALCAT11 <- NEWRULEFUNCTIONALCAT11 %>%
  mutate(
    CONVRULE = case_when(
      FUNCTIONAL_CAT == "D" ~ "RTYP27",
      FUNCTIONAL_CAT == "U2" ~ "RTYP29",
      FUNCTIONAL_CAT == "U1" ~ "RTYP28",
      FUNCTIONAL_CAT == "D1" ~ "RTYP30",
      FUNCTIONAL_CAT == "D2" ~ "RTYP31",
      FUNCTIONAL_CAT == "D3" ~ "RTYP32",
      FUNCTIONAL_CAT == "P" ~ "RTYP33",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEFUNCTIONALCAT11)[1] = "ELEMNTIN" 
colnames(NEWRULEFUNCTIONALCAT11)[2] = "ELEMENTOUT"

# Replacing by new CONV Rule 
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'D'"] <- "CONV('RTYP27', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'U1'"] <- "CONV('RTYP28', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'U2'"] <- "CONV('RTYP29', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'D1'"] <- "CONV('RTYP30', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'D2'"] <- "CONV('RTYP31', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'D3'"] <- "CONV('RTYP32', TYPINS)"
TANFC011$FUNCTIONAL_CAT[TANFC011$FUNCTIONAL_CAT == "'P'"] <- "CONV('RTYP33', TYPINS)"

# Handle duplicate removal for functional_cat
duplicate_removal11 <- subset(TANFC011)

duplicate_removal11 <- duplicate_removal11 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\)") %>% str_replace("TYPINS IN \\(", "") %>% str_replace("\\)", "") %>% trimws(),
    FILTER = NULL
  ) %>%
  group_by(FUNCTIONAL_CAT, INSTR_ASSET, FLOW_STOCK_ENTRY, VALUATION) %>%
  summarize(TYPINS = paste(unique(TYPINS), collapse = ","), .groups = 'drop') %>%
  left_join(duplicate_removal11, by = c("FUNCTIONAL_CAT", "INSTR_ASSET", "FLOW_STOCK_ENTRY", "VALUATION"))

duplicate_removal11 <- duplicate_removal11 %>%
  mutate(
    FILTER = str_replace(FILTER, "TYPINS IN \\(.*?\\) AND", "TYPINS IN () AND")
  )

duplicate_removal11 <- duplicate_removal11 %>%
  mutate(
    FILTER = str_replace(FILTER, "(TYPINS IN \\(.*?\\) AND)", paste0("TYPINS IN (", TYPINS, ") AND"))
  ) %>%
  select(-TYPINS)

TANFC011<- distinct(duplicate_removal11)


# TABLE 19 INSTRASSET Rule Additional Operations


# Creating a subset of TA NFC 019
TANFC019 <- subset(aggregated, INPUTCUBEID == 'TANFC_019_UNI_BR_GR')

# Extracting the target string from the FILTER column
extracted_typinsTA019 <- TANFC019 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\) AND") %>%
      str_remove("^TYPINS IN \\(") %>%
      str_remove("\\) AND$")
  )

# Removing unnecessary columns for the new rule (keeping only TYPINS and instr assets)
extracted_dataTA019 <- subset(extracted_typinsTA019, select = c(TYPINS, INSTR_ASSET))

# Create a rule ID

# Dropping N/A TYPINS
extracted_dataTA019 <- extracted_dataTA019 %>% drop_na(TYPINS)

# Separate values in TYPINS using a comma as a separator
extracted_dataTA019 <- extracted_dataTA019 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_dataTA019 <- extracted_dataTA019 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_dataTA019 <- extracted_dataTA019 %>% mutate(INSTR_ASSET = gsub("'","",INSTR_ASSET))

# Remove duplicates
extracted_dataTA019 <- extracted_dataTA019 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEINSTRASSET19 <- extracted_dataTA019 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Identify unique values in INSTR_ASSET
unique(NEWRULEINSTRASSET19$INSTR_ASSET)

# Modify CONVRULE based on conditions
NEWRULEINSTRASSET19 <- NEWRULEINSTRASSET19 %>%
  mutate(
    CONVRULE = case_when(
      INSTR_ASSET == "F3" ~ "RTYP22",
      INSTR_ASSET == "F4" ~ "RTYP23",
      INSTR_ASSET == "F51B" ~ "RTYP24",
      INSTR_ASSET == "F81" ~ "RTYP25",
      INSTR_ASSET == "FL" ~ "RTYP26",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEINSTRASSET19)[1] = "ELEMNTIN" 
colnames(NEWRULEINSTRASSET19)[2] = "ELEMENTOUT"



# Replacing F511 and F512 by new CONV Rule 
TANFC019$INSTR_ASSET[TANFC019$INSTR_ASSET == "'F3'"] <- "CONV('RTYP22', TYPINS)"
TANFC019$INSTR_ASSET[TANFC019$INSTR_ASSET == "'F4'"] <- "CONV('RTYP23', TYPINS)"
TANFC019$INSTR_ASSET[TANFC019$INSTR_ASSET == "'F51B'"] <- "CONV('RTYP24', TYPINS)"
TANFC019$INSTR_ASSET[TANFC019$INSTR_ASSET == "'F81'"] <- "CONV('RTYP25', TYPINS)"
TANFC019$INSTR_ASSET[TANFC019$INSTR_ASSET == "'FL'"] <- "CONV('RTYP26', TYPINS)"



# TABLE 19 Functional_cat Rule Additional Operations

####### functional_cat rule
extracted_funccatTA019 <- subset(extracted_typinsTA019, select = c(TYPINS, FUNCTIONAL_CAT))

# Create a rule ID

# Dropping N/A TYPINS
extracted_funccatTA019 <- extracted_funccatTA019 %>% drop_na(TYPINS)

# Separate values in TYPINS using a comma as a separator
extracted_funccatTA019 <- extracted_funccatTA019 %>% separate_rows(TYPINS, sep = ',')

# Remove the ''
extracted_funccatTA019 <- extracted_funccatTA019 %>% mutate(TYPINS = gsub("'","",TYPINS))
extracted_funccatTA019 <- extracted_funccatTA019 %>% mutate(FUNCTIONAL_CAT = gsub("'","",FUNCTIONAL_CAT))

# Remove duplicates
extracted_funccatTA019 <- extracted_funccatTA019 %>% distinct()

## INPUTTING IN INSTASSET RULE  
NEWRULEFUNCTIONALCAT19 <- extracted_funccatTA019 %>%  mutate(CONVRULE = "", DOMAINID = "TYPINS", STARTDATE = "1900-01-01", ENDDATE = "9999-12-31", SIGN = "1")

# Identify unique values in FUNCTIONAL_CAT
unique(NEWRULEFUNCTIONALCAT19$FUNCTIONAL_CAT)

# Modify CONVRULE based on conditions
NEWRULEFUNCTIONALCAT19 <- NEWRULEFUNCTIONALCAT19 %>%
  mutate(
    CONVRULE = case_when(
      FUNCTIONAL_CAT == "D" ~ "RTYP27",
      FUNCTIONAL_CAT == "U2" ~ "RTYP29",
      FUNCTIONAL_CAT == "U1" ~ "RTYP28",
      FUNCTIONAL_CAT == "D1" ~ "RTYP30",
      FUNCTIONAL_CAT == "D2" ~ "RTYP31",
      FUNCTIONAL_CAT == "D3" ~ "RTYP32",
      TRUE ~ CONVRULE  # Keep the original value if none of the conditions match
    )
  )

# Changing the column names
colnames(NEWRULEFUNCTIONALCAT19)[1] = "ELEMNTIN" 
colnames(NEWRULEFUNCTIONALCAT19)[2] = "ELEMENTOUT"


# Replacing by new CONV Rule 
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'D'"] <- "CONV('RTYP27', TYPINS)"
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'U1'"] <- "CONV('RTYP28', TYPINS)"
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'U2'"] <- "CONV('RTYP29', TYPINS)"
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'D1'"] <- "CONV('RTYP30', TYPINS)"
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'D2'"] <- "CONV('RTYP31', TYPINS)"
TANFC019$FUNCTIONAL_CAT[TANFC019$FUNCTIONAL_CAT == "'D3'"] <- "CONV('RTYP32', TYPINS)"

# Handle duplicate removal for functional_cat
duplicate_removal19 <- subset(TANFC019)

duplicate_removal19 <- duplicate_removal19 %>%
  mutate(
    TYPINS = str_extract(FILTER, "TYPINS IN \\((.*?)\\)") %>% str_replace("TYPINS IN \\(", "") %>% str_replace("\\)", "") %>% trimws(),
    FILTER = NULL
  ) %>%
  group_by(FUNCTIONAL_CAT, INSTR_ASSET, FLOW_STOCK_ENTRY, VALUATION) %>%
  summarize(TYPINS = paste(unique(TYPINS), collapse = ","), .groups = 'drop') %>%
  left_join(duplicate_removal19, by = c("FUNCTIONAL_CAT", "INSTR_ASSET", "FLOW_STOCK_ENTRY", "VALUATION"))

duplicate_removal19 <- duplicate_removal19 %>%
  mutate(
    FILTER = str_replace(FILTER, "TYPINS IN \\(.*?\\) AND", "TYPINS IN () AND")
  )

duplicate_removal19 <- duplicate_removal19 %>%
  mutate(
    FILTER = str_replace(FILTER, "(TYPINS IN \\(.*?\\) AND)", paste0("TYPINS IN (", TYPINS, ") AND"))
  ) %>%
  select(-TYPINS)

TANFC019<- distinct(duplicate_removal19)




# Combine all data frames: SMESEOFINAL, TANFC006, TANFC007, TANFC009, TANFC010, TANFC011, TANFC019, adj
newPADIconfig <- rbind(SMESEOFINAL, TANFC006, TANFC007, TANFC009, TANFC010, TANFC011, TANFC019, adj)

# Change start and end date for all rows
newPADIconfig$STARTDATE <- '1900-01-01'
newPADIconfig$ENDDATE <- '9999-12-31'



setwd("C:/Users/aurel/Desktop/Portfolio/Data management projects/SQL configuration table modification")

dir.create('Output2')

setwd("C:/Users/aurel/Desktop/Portfolio/Data management projects/SQL configuration table modification/Output2")

# Save the combined and modified data to a CSV file
write.csv(newPADIconfig, file = "NEWPIDI2.csv", row.names = FALSE)

# Combining both data frames in 1
COMBINED_FUNCTIONALCAT <- bind_rows(NEWRULEFUNCTIONALCAT9, NEWRULEFUNCTIONALCAT10, NEWRULEFUNCTIONALCAT11, NEWRULEFUNCTIONALCAT19 )

# Write combined data to a CSV file
write.csv(COMBINED_FUNCTIONALCAT, file = "functionalcat_rule2.csv", row.names = FALSE)


combined_data <- bind_rows(NEWRULEINSTRASSET, NEWRULEINSTRASSET7, NEWRULEINSTRASSET9, NEWRULEINSTRASSET10, NEWRULEINSTRASSET11, NEWRULEINSTRASSET19)
combined_data <- distinct(combined_data)
# Write combined data to a CSV file
write.csv(combined_data, file = "INSTRASSETS_NEWRULE2.csv", row.names = FALSE)


