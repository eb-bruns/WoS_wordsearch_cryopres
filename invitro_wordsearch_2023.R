################################################################################

### Author: Emily Beckman Bruns
### Last Update: _______

### Funding: Institute of Museum and Library Services
#            grant number _______________________
#            Awarded to Cincinnati Zoo & Botanical Garden

### DESCRIPTION:
  # Word search analysis of Web of Science data; searching for family, genus,
  #  and keywords related to in vitro propagation.

### INPUTS:
  # Exports from Web of Science (Excel files)
  # Keyword lists, including (available in GitHub repository):
  #  - Common names matched with scientific names
  #     (Keyword searches - common_names.csv)
  #  - Keywords to check the article is relevant topic of interest
  #     (Keyword searches - focus_check.csv)
  # World Flora Online static backbone v.2021.01
  #   http://www.worldfloraonline.org/downloadData
  # Exceptional Status List (Pence et al. 2022)
  #   https://cincinnatizoo.org/system/assets/uploads/2022/02/Supplementary-data.xlsx
  # Seed/non-seed and higher classification from The Plant List and manual
  #  searching (available in GitHub repository: "seed_nonseed_families.csv")

### OUTPUTS:
  ## List of articles from Web of Science, with columns indicating which
  #   families, genera, and keywords were identified. This output can then be
  #   manually reviewed and edited before analysis.


#################
### LIBRARIES ###
#################

# this code chunk is from Shannon M. Still
# rm(list=ls())
my.packages <- c('tidyverse','textclean','data.table','rebus','readxl',
                 'arsenal','countrycode')
lapply(my.packages, require, character.only=TRUE)
#  install.packages(my.packages) #Turn on to install current versions

# package citations

#Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes
#A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J,
#Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H
#(2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43),
#1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

#Rinker, T. W. (2018). textclean: Text Cleaning Tools version 0.9.3. Buffalo, New
#York. https://github.com/trinker/textclean

#Dowle M, Srinivasan A (2023). _data.table: Extension of `data.frame`_. R package
#version 1.14.8, <https://CRAN.R-project.org/package=data.table>.

#Cotton R (2017). _rebus: Build Regular Expressions in a Human Readable Way_. R
#package version 0.1-3, <https://CRAN.R-project.org/package=rebus>.

#Wickham H, Bryan J (2023). _readxl: Read Excel Files_. R package version 1.4.2,
#<https://CRAN.R-project.org/package=readxl>.


#################
### FUNCTIONS ###
#################

# find keyword (e.g., genus name) matches in text;
#   return all (separated by a comma), one cell for each row
find.all.matches <- function(search.col,pat){
  captured <- str_match_all(search.col,pattern = pat)
  #t <- lapply(captured, str_squish)
  t <- lapply(captured, function(x) gsub("[^a-zA-Z]","",x))
  t2 <- sapply(t, unique)
  t3 <- sapply(t2, str_to_title)
  t4 <- lapply(t3, toString)
  found.col <- unlist(t4)
  return(found.col)
}

find.all.matches.keywords <- function(search.col,pat){
  captured <- str_match_all(search.col,pattern = pat)
  t <- lapply(captured, str_trim)
  t2 <- lapply(t, function(x) gsub("[^a-zA-Z0-9 -]","",x))
  t3 <- sapply(t2, unique)
  t4 <- lapply(t3, toString)
  found.col <- unlist(t4)
  return(found.col)
}

find.all.matches.nospace <- function(search.col,pat){
  captured <- str_match_all(search.col,pattern = pat)
  #t <- lapply(captured, str_squish)
  t <- lapply(captured, function(x) gsub("[^a-zA-Z]","",x))
  t2 <- sapply(t, unique)
  t3 <- lapply(t2, toString)
  found.col <- unlist(t3)
  return(found.col)
}

freq.table <- function(target.col){
	# see max number of items (genera/families/etc.) for one record
	see_max <- sapply(target.col,function(x) str_count(x, pattern = ", "))
	# create array of separated items
	sep_items <- str_split_fixed(target.col, ", ", n = (max(see_max)+1))
	# sum to calculate frequency
	count_freq <- as.data.frame(table(sep_items))
	count_freq <- count_freq[-1,]
  count_freq <- count_freq %>% dplyr::arrange(desc(Freq))
	return(count_freq)
}




###############################################################################
# Set working directory and folders with WoS downloads
###############################################################################

# working directory
main_dir <- "/Users/emily/Library/CloudStorage/GoogleDrive-ebeckman@mortonarb.org/.shortcut-targets-by-id/1_chgTVGeSxYq_-Ce7LYz4zrZxIMSdyzi/Micropropagation Downloads/Cryo InVitro Wordsearch Project - Beckman Bruns/For_R_analysis"

# folder with raw WOS exports (.xlsx files)
data_folder <- "InVitro_2023"

###############################################################################
# Load all data
###############################################################################

##
### WEB OF SCIENCE EXPORTS
##

# read in raw Excel downloads from Web of Science
path <- file.path(main_dir,"WebOfScience_Downloads_Excel",data_folder)
file_list <- list.files(path, pattern = ".xls", full.names = T)
file_dfs <- lapply(file_list, read_xls, col_types = "text")
length(file_dfs) #29
# add file name as column, to see which keyword search it was from
for(file in seq_along(file_dfs)){
  df <- file_dfs[[file]]
  df$filename <- rep(file_list[file],nrow(df))
  df$filename <- mgsub(df$filename,c(paste0(path,"/"),".xls"),"")
  file_dfs[[file]] <- df
}

# stack all data; 'Reduce' iterates through list and merges with previous
all_data_raw <- Reduce(dplyr::bind_rows, file_dfs)
nrow(all_data_raw) #25513

# remove columns with nothing but NA
remove <- vector()
for(i in 1:ncol(all_data_raw)){
  if(nrow(unique(all_data_raw[,i])) == 1 & is.na(unique(all_data_raw[1,i]))){
    print(paste(i,":",names(all_data_raw[,i])))
    remove <- c(remove,i)
  }
}
all_data_raw <- all_data_raw[,-remove]

# rename some key columns to remove spaces in the names (not allowed in R)
setnames(all_data_raw,
  old = c("Article Title","Abstract","UT (Unique WOS ID)",
          "Publication Year","Publisher Address","Addresses"),
  new = c("title_orig","abstract_orig","uid",
          "publication_year","publisher_address","addresses"))

# remove duplicate articles
all_data <- all_data_raw %>%
  group_by(uid) %>%
  mutate(filename = paste(filename, collapse = '; ')) %>%
  distinct(uid,.keep_all=T)
nrow(all_data) #22251

# keep only necessary columns (can add rest back in at end)
all_data <- all_data %>%
  select(uid,title_orig,abstract_orig,filename,publication_year,
         publisher_address,addresses)
head(as.data.frame(all_data))

# add space at beginning and end of title, for matching genus names later
all_data$title_orig <- paste0(" ",all_data$title_orig," ")
all_data$abstract_orig <- paste0(" ",all_data$abstract_orig," ")
head(all_data)

# create lowercase version of title and abstract
all_data$title_l <- str_to_lower(all_data$title_orig)
all_data$abstract_l <- str_to_lower(all_data$abstract_orig)

# replace common names with scientific names in title and abstract
common_names <- read.csv(file.path(main_dir,
  "Keyword searches - common_names.csv"), colClasses = "character")
search <- paste0("[^a-rt-z]",common_names[,1],"[^a-rt-z]")
replace <- paste0(" ",common_names[,2]," ")
all_data$title_l_rep <- mgsub(all_data$title_l, search, replace, fixed = F,
  ignore.case = T)
all_data$abstract_l_rep <- mgsub(all_data$abstract_l, search, replace, fixed = F,
  ignore.case = T)

  # make lowercase one final time
all_data$title_l_rep <- str_to_lower(all_data$title_l_rep)
all_data$abstract_l_rep <- str_to_lower(all_data$abstract_l_rep)

rm(all_data_raw, file_dfs)
#head(as.data.frame(all_data))

# write file
#write.csv(all_data,file.path(main_dir,
#  paste0("metadata_", Sys.Date(), ".csv")), row.names=F)

##
### GENUS, FAMILY, AND EXCEPTIONAL SPECIES LISTS
##

# WORLD FLORA ONLINE (WFO)

# read in WFO backbone
# download WFO taxonomic data
#   go to:  http://www.worldfloraonline.org/downloadData;jsessionid=94916E1F29B8ADFF5353032114B66D0E
#   scroll down to "Latest Static Version:" and click link to download
#   unzip the folder

## 2023 note: I'm using v.2021.12 because that is what's aligned with the 
##  Exceptional Species List.. otherwise I would use a newer version.

# read in table
wfo_all <- read.delim(file.path(main_dir,"WFO_Backbone",
                                # replace the version number as needed:
                                "v.2021.01","classification.txt"),
                      colClasses = "character")
# keep just genus and family columns
wfo_gen <- wfo_all %>% distinct(genus,.keep_all=T) %>%
  filter(genus != "") %>% dplyr::select(family,genus)
str(wfo_gen)
rm(wfo_all)
# replace non-standard family name
unique(wfo_gen$genus); nrow(wfo_gen) #37112 (v.2019.05 is 35856)
unique(wfo_gen$family)
# for v.2019.05
#wfo_gen$family[which(wfo_gen$family == "FRANCOACEAE ")] <- "Francoaceae"

# LIST OF GENERA

gen <- wfo_gen %>% distinct(genus) %>% arrange(genus)
gen_list <- gen[,1]
gen_list <- str_to_title(gen_list)
# remove some genera that only find other things
gen_list <- gen_list[which(gen_list != "Medium" &
                           gen_list != "India" &
                           gen_list != "Cotyledon" &
                           gen_list != "Aa" &
                           gen_list != "Ion" &
                           gen_list != "Cuba" &
                           # to add if running again:
                           gen_list != "Basal" &
                           gen_list != "Laser" &
                           gen_list != "Panda" &
                           gen_list != "Unknown" 
                           )]
length(gen_list) #42668 (v.2021.01 is 37106)

# LIST OF FAMILIES

fam <- wfo_gen %>% distinct(family) %>% arrange(family)
fam_list <- fam[,1]
fam_list <- str_to_title(fam_list)
length(fam_list) #698 (v.2019.05 694 with TPL too ; 661 WFO only)

# create subset for testing word searches
data_ex <- all_data[sample(nrow(all_data), 300),]

# LIST OF EXCEPTIONAL SPECIES

# create list of Exceptional species names
excep_status <- read.csv(file.path(main_dir,
  "Exceptional Status List - No Justification.csv"), colClasses = "character")
excep_status <- excep_status %>%
  filter(Exceptional.status == "Exceptional")
  ## INLUDES SYNONYMS FROM WFO !
excep_list_plus_syn <- c(excep_status$Species.name,excep_status$WFO.taxon.name)
excep_list <- unique(excep_list_plus_syn)
length(excep_list) #863 (775 without synonyms)

###############################################################################
## Word searching
###############################################################################

##
### SEARCH FOR GENUS NAMES (!!CAN TAKE A WHILE!!)
##

# create pattern then search for genera in title and abstract text

## --- IN TITLE --- ##

# PATTERN 1: genus name with non-alpha character before and after,
#            in title only and NO common names replaced
gen_list1 <- gen_list
gen_list1[1] <- paste0("[^a-zA-Z]",gen_list1[1])
gen_list1[length(gen_list1)] <- paste0(gen_list1[length(gen_list1)],"[^a-zA-Z]")
gen_pat1 <- paste(gen_list1,collapse="[^a-zA-Z]|[^a-zA-Z]")
  # view test matches
str_view_all(data_ex$title_orig, pattern = gen_pat1, match = T)
  # get matches
all_data$genera <- find.all.matches(all_data$title_orig,gen_pat1)
try2 <- all_data[which(all_data$genera==""),]; nrow(try2) #10247

# PATTERN 2: all-caps genus name with non-alpha character before and after,
#            in title only and NO common names replaced
gen_list2 <- str_to_upper(gen_list)
gen_list2[1] <- paste0("[^a-zA-Z]",gen_list2[1])
gen_list2[length(gen_list2)] <- paste0(gen_list2[length(gen_list2)],"[^a-zA-Z]")
gen_pat2 <- paste(gen_list2,collapse="[^a-zA-Z]|[^a-zA-Z]")
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try2$title_orig,gen_pat2)
try3 <- all_data[which(all_data$genera==""),]; nrow(try3) #8063

# PATTERN 3: genus name with following character not a-z (lowercase),
#            in title only and NO common names replaced
gen_list3 <- gen_list
gen_list3[length(gen_list3)] <- paste0(gen_list3[length(gen_list3)],"[^a-z]")
gen_pat3 <- paste(gen_list3,collapse="[^a-z]|")
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try3$title_orig,gen_pat3)
try4 <- all_data[which(all_data$genera==""),]; nrow(try4) #7983

# PATTERN 4: genus name with non-alpha character before and after,
#            in title only and YES common names replaced
gen_list4 <- str_to_lower(gen_list)
gen_list4[length(gen_list4)] <- paste0(gen_list4[length(gen_list4)],"[^a-zA-Z]")
gen_pat4 <- paste(gen_list4,collapse="[^a-zA-Z]|[^a-zA-Z]")
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try4$title_l_rep,gen_pat4)
try5 <- all_data[which(all_data$genera==""),]; nrow(try5) #3264

## --- IN ABSTRACT --- ##

# PATTERN 5: genus name with non-alpha character before and after,
#            in abstract and NO common names replaced
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try5$abstract_orig,gen_pat1)
try6 <- all_data[which(all_data$genera==""),]; nrow(try6) #2211

# PATTERN 6: all-caps genus name with non-alpha character before and after,
#            in abstract and NO common names replaced
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try6$abstract_orig,gen_pat2)
try7 <- all_data[which(all_data$genera==""),]; nrow(try7) #2207

# PATTERN 7: genus name with following character not a-z (lowercase),
#            in abstract and NO common names replaced
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try7$abstract_orig,gen_pat3)
try8 <- all_data[which(all_data$genera==""),]; nrow(try8) #2202

# PATTERN 8: genus name with non-alpha character before and after,
#            in abstract and YES common names replaced
  # get matches
all_data[which(all_data$genera==""),]$genera <-
  find.all.matches(try8$abstract_l_rep,gen_pat4)
try9 <- all_data[which(all_data$genera==""),]; nrow(try9) #1765

rm(try2,try3,try4,try5,try6,try7,try8,try9)

##
### SEARCH FOR FAMILY NAMES
##

  # create pattern
    # family names are more distinctive, so we don't have to be as careful
    #   about how we search for them (no need for non-alpha before/after, etc.)
fam_list_l <- str_to_lower(fam_list)
fam_pattern <- paste(fam_list_l,collapse="|")

# SEARCH 1: family name in title and NO common names replaced
  # extract matches
all_data$families_found <-
  find.all.matches(all_data$title_l,fam_pattern)
try2 <- all_data[which(all_data$families_found==""),]; nrow(try2) #21665

# SEARCH 2: family name in title and YES common names replaced
  # extract matches
all_data[which(all_data$families_found==""),]$families_found <-
  find.all.matches(try2$title_l_rep,fam_pattern)
try3 <- all_data[which(all_data$families_found==""),]; nrow(try3) #21665

# SEARCH 3: family name in abstract and NO common names replaced
  # extract matches
all_data[which(all_data$families_found==""),]$families_found <-
  find.all.matches(try3$abstract_l,fam_pattern)
try4 <- all_data[which(all_data$families_found==""),]; nrow(try4) #21665

# SEARCH 4: family name in abstract and YES common names replaced
  # extract matches
all_data[which(all_data$families_found==""),]$families_found <-
  find.all.matches(try4$abstract_l_rep,fam_pattern)
try5 <- all_data[which(all_data$families_found==""),]; nrow(try5) #20082

rm(try2,try3,try4,try5)

# match each genus found to its corresponding WFO family
  # create matrix with all genus names found
genera <- str_split(all_data$genera,", ")
  # use lookup table (wfo_gen) to replace genus names with family names
for(i in 1:length(genera)){
  genera[[i]] <- wfo_gen$family[match(unlist(genera[[i]]), wfo_gen$genus)]
}
fam <- genera
fam <- sapply(fam, unique)
fam <- lapply(fam, toString)
fam <- unlist(fam)
fam[fam=="NA"] <- ""
  # add column with all families from genus names found
all_data$families_genera <- fam

# merge families found in text and genera-matched families
fam_final <- all_data %>%
  select(uid,families_found,families_genera) %>%
  mutate_all(na_if,"") %>%
  tidyr::unite("families",c("families_found","families_genera"),sep=", ",remove=T,na.rm=T)
fam_final <- str_split(fam_final$families,", ")
fam <- sapply(fam_final, unique)
fam <- sapply(fam, str_to_title)
fam <- lapply(fam, toString)
fam <- unlist(fam)
# add to data frame
all_data$families <- fam

# create column with just the first family found, for adding seed status
all_data$first_family <- unlist(lapply(str_split(all_data$families,", "),"[[",1))

# add seed/non-seed and higher classification from TPL and manual searching
seed_yn <- read.csv(file.path(main_dir,"seed_nonseed_families.csv"),
  header = T, na.strings = c("","NA"), colClasses="character")
higher_group <- left_join(wfo_gen,seed_yn)
higher_group_add <- higher_group %>%
  dplyr::select(family,major_group,seed_status) %>%
  distinct(family,.keep_all=T) %>%
  rename(first_family = family)
all_data <- dplyr::left_join(all_data,higher_group_add)

head(as.data.frame(all_data))

  # this should return nothing:
as.data.frame(all_data[which(all_data$first_family != "" & is.na(all_data$major_group)),])$first_family
  # add anything returned manually:
  all_data[all_data$first_family=="Asclepiadaceae",]$major_group <- "Angiosperms"
  all_data[all_data$first_family=="Asclepiadaceae",]$seed_status <- "seed"

##
### SEARCH FOR KEY WORDS
##

# create pattern
keywords <- read.csv(file.path(main_dir,
  "Keyword searches - focus_check.csv"), colClasses = "character")
search <- str_squish(keywords[,1])
main_category <- str_squish(keywords[,2])
key_search <- paste0(search, collapse="[^a-rt-zA-RT-Z]|[^a-zA-Z]")
key_search <- paste0("[^a-zA-Z]",key_search,"[^a-rt-zA-RT-Z]")

# extract matches
all_data$title_abstract_l <- paste(all_data$title_l,all_data$abstract_l,sep=" :: ")
all_data$key_capture <- find.all.matches.keywords(all_data$title_abstract_l,key_search)

# add column with more general keyword category
all_data$key_category <- all_data$key_capture
t <- str_split(all_data$key_category,", ")
t <- lapply(t,function(x) gsub("^-|-$","",x))
t <- lapply(t,function(x) mgsub(unlist(x),search,main_category))
t <- lapply(t,function(x) gsub("s|[0-9]","",x))
t <- lapply(t,sort)
t <- lapply(t,unique)
all_data$key_category <- unlist(lapply(t, toString))
unique(all_data$key_category)

# rows with no keyword
nrow(all_data[which(all_data$key_capture == ""),]) #549

##
### SEARCH FOR CRYO TYPE
##

# create pattern
cryo_keywords <- read.csv(file.path(main_dir,
  "Keyword searches - cryo.csv"), colClasses = "character")
search <- str_squish(cryo_keywords[,1])
cryo_category <- str_squish(cryo_keywords[,2])
cryo_search <- paste(search, collapse="[^a-rt-zA-RT-Z]|[^a-zA-Z]")
cryo_search <- paste("[^a-zA-Z]",cryo_search,"[^a-rt-zA-RT-Z]")

# extract matches
all_data$cryo_capture <- find.all.matches.keywords(all_data$title_abstract_l,cryo_search)

# add column with more general cryo category
all_data$cryo_category <- all_data$cryo_capture
t <- str_split(all_data$cryo_category,", ")
t <- lapply(t,function(x) gsub("^-|-$","",x))
t <- lapply(t,function(x) mgsub(unlist(x),search,cryo_category))
t <- lapply(t,function(x) gsub("s|[0-9]","",x))
t <- lapply(t,str_trim)
t <- lapply(t,sort)
t <- lapply(t,unique)
all_data$cryo_category <- unlist(lapply(t, toString))
unique(all_data$cryo_category)

# rows with no cryo keyword
nrow(all_data[which(all_data$cryo_capture == ""),]) #1630

##
### FLAG OTHER KEYWORDS
##

# create patterns
non_keywords <- paste0("[^a-zA-Z]",
  paste("sperm","semen","oocyte","nematode","human","mammal","review",
  sep="[^a-rt-zA-RT-Z]|[^a-zA-Z]"),"[^a-rt-zA-RT-Z]")
nonseed_keywords <- paste0("[^a-zA-Z]",
  paste("algal","seaweed","rhodophyta","algae","alga",
  "bryophyte","moss","mosses","gametophyte",
  "pteridophyte","fern","spore",
  sep="[^a-rt-zA-RT-Z]|[^a-zA-Z]"),"[^a-rt-zA-RT-Z]")
#broad_keywords <- paste0("[^a-zA-Z]",
#  paste("tree","shrub","berry","flora","fruit","conifer","agricultur","horticultur","seeds","plant",
#  sep="[^a-rt-zA-RT-Z]|[^a-zA-Z]"),"[^a-rt-zA-RT-Z]")

# extract matches
all_data$non_capture <- find.all.matches.nospace(all_data$title_abstract_l,non_keywords)
all_data$nonseed_capture <- find.all.matches.nospace(all_data$title_abstract_l,nonseed_keywords)
#all_data$broad_capture <- find.all.matches.nospace(all_data$title_abstract_l,broad_keywords)

##
### SEARCH FOR EXCEPTIONAL SPECIES
##

# create pattern
excep_list_l <- str_to_lower(excep_list)
excep_pattern <- paste(excep_list_l,collapse="|")

# view matches
#str_view_all(data_ex$title_abstract_l_rep, pattern = excep_pattern)

# extract matches
all_data$title_abstract_l_rep <- paste(all_data$title_l_rep,all_data$abstract_l_rep,sep=" :: ")
captured <- str_match_all(all_data$title_abstract_l_rep,pattern = excep_pattern)
t <- sapply(captured, unique)
t2 <- sapply(t, str_to_sentence)
t3 <- lapply(t2, toString)
all_data$excep_sp <- unlist(t3)
  # OLD VERSION that leaves names lowercase:
  #all_data$excep_sp <- find.all.matches.keywords(all_data$title_abstract_l,excep_pattern)

# take a look
head(as.data.frame(all_data))

##
### SEARCH FOR PUBLICATION & AUTHOR COUNTRIES
##

# use countrycode package to search for and standardize country names
#   from the publisher & author address columns; add to main df
  ## publisher addresses
  # search for countries
t <- as.data.frame(countryname(all_data$publisher_address))
colnames(t)[1] <- "publisher_country"
all_data <- cbind(all_data,t)
  # assign countries for those that are ambiguous (may need to add more)
all_data[grep("ENGLAND|SCOTLAND|WALES", 
              all_data$publisher_address),]$publisher_country <- "United Kingdom"
  # assign US based on state and zip, when doesn't say US
all_data[grep(", [A-Z][A-Z] [0-9][0-9][0-9][0-9][0-9]$", 
              all_data$publisher_address),]$publisher_country <- "US"
  ## author addresses
  # need to separate out each address, search for country, then combine
    # first determine which separator to use (there seem to be two common ones)
    sep1 <- all_data[grep("\\[",all_data$addresses),]; nrow(sep1)
    # remove first character so we don't get a blank column first
    sep1$addresses <- sub('.', '', sep1$addresses)
    # now separate
    sep1 <- sep1 %>% 
      select(addresses,uid) %>%
      separate(addresses, 
               into = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11",
                        "a12","a13","a14","a15","a16","a17","a18","a19","a20",
                        "a21","a22","a23","a24","a25","a26","a27","a28","a29",
                        "a30","a31","a32","a33","a34","a35","a36","a37","a38",
                        "a39","a40","a41","a42","a43","a44","a45"), 
               sep = "\\[") %>%
      replace(is.na(.), "None")
    sep2 <- all_data[which(!(all_data$uid %in% unique(sep1$uid))),]; nrow(sep2)
    sep2 <- sep2 %>% 
      select(addresses,uid) %>%
      separate(addresses, 
               into = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11",
                        "a12","a13","a14","a15","a16","a17","a18","a19","a20",
                        "a21","a22","a23","a24","a25","a26","a27","a28","a29",
                        "a30","a31","a32","a33","a34","a35","a36","a37","a38",
                        "a39","a40","a41","a42","a43","a44","a45"), 
               sep = ";") %>%
      replace(is.na(.), "None")
  sep_t <- as.data.frame(rbind(sep1,sep2))
  # search for countries
t <- data.frame(uid = all_data$uid)
for(i in 1:(ncol(sep_t)-1)){
  # get column to work with now
  piece_now <- as.data.frame(sep_t)[,c(46,i)]
  # find country name in that column
  t2 <- as.data.frame(countryname(piece_now[,2]))
  t2 <- cbind(piece_now, t2)
  colnames(t2)[3] <- paste0("c",i)
    # assign country for common issues
    t2[grep("ENGLAND|SCOTLAND|WALES",t2[,2],ignore.case=T),3] <- "United Kingdom"
    t2[grep("South Korea",t2[,2],ignore.case=T),3] <- "South Korea"
    t2[grep("Peoples R China",t2[,2],ignore.case=T),3] <- "China"
    likely_us <- grep("[,| ][A-Z][A-Z] [0-9][0-9][0-9][0-9][0-9]", t2[,2])
    no_ctry <- which(is.na(t2[,3]))
    t2[intersect(likely_us,no_ctry),3] <- "US"
      # see where there is data but no country was identified
      no_ctry <- which(is.na(t2[,3]))
      no_data <- which(t2[,2]=="None")
      need_manual_edit <- no_ctry[!(no_ctry %in% no_data)]
      # flag these for a manual look
      t2[need_manual_edit,3] <- "<CHECK>"
  # keep only uid and final country assignment
  t2 <- t2[,c(1,3)]
  # join to df where we will gather results for all columns
  t <- left_join(t,t2)
  print(paste("finished",i))
}
  # now combine all the author countries across each row and remove duplicates
auth_ctrys <- t %>%
  rowwise %>%
  mutate(author_countries = toString(unique(na.omit(c_across(2:46))))) %>%
  ungroup
auth_ctrys <- auth_ctrys[,c(1,47)]; head(auth_ctrys)
  # append to main df
all_data <- left_join(all_data,auth_ctrys)

# save to add to old version, if doing after-the-fact
add <- all_data[,c("uid",
                   "publisher_address","publisher_country",
                   "addresses","author_countries")]
write.csv(add,file.path(main_dir,"add_countries_invitro.csv"), row.names=F)

###############################################################################
## Export results
###############################################################################

# select and order columns
data_sel <- all_data %>%
  select(uid,filename,publication_year,
         publisher_country,#author_countries,
         title_orig,abstract_orig,
         title_l_rep,abstract_l_rep,
         genera,
         families,families_found,families_genera,
         major_group,seed_status,
         excep_sp,
         key_capture,key_category,
         cryo_capture,cryo_category,
         non_capture,nonseed_capture) #broad_capture
nrow(data_sel)
head(as.data.frame(data_sel))

# replace NA with ""
data_sel[is.na(data_sel)] <- ""
  # THIS IS THE OPPOSITE, IN CASE YOU'D LIKE ALL NA INSTEAD
  #data_sel <- apply(data_sel, 2, function(x) gsub("^$|^ $", NA, x))

# flag rows that need review or should be removed
data_sel$flag <- ""
  # no key_category
data_sel[which(data_sel$key_category == ""),]$flag <- "No key_category"
length(which(grepl("key",data_sel$flag) == TRUE)) #112
  # no cryo_category
data_sel[which(grepl("Cryo",data_sel$key_category) &
               data_sel$cryo_category == ""),]$flag <- "No cryo_category"
length(which(grepl("cryo",data_sel$flag) == TRUE)) #617
  # check cryo_category
data_sel[which(grepl("Cryo",data_sel$key_category) &
               grepl("Check",data_sel$cryo_category)),]$flag <- "Check cryo_category"
length(which(grepl("Check",data_sel$flag) == TRUE)) #103
  # no genus or family found
data_sel[which(data_sel$genera == "" &
               data_sel$families == ""),]$flag <- "No genus or family found"
length(which(grepl("genus",data_sel$flag) == TRUE)) #710

head(as.data.frame(data_sel))

# write file
write.csv(data_sel,file.path(main_dir,
  paste0("invitro_wordsearch_matches_", Sys.Date(), ".csv")), row.names=F)


###############################################################################
## Add manual edits from old version
###############################################################################

# read full dataset back in if starting from here (saved in previous section)
#data_sel <- read.csv(file.path(main_dir,
#  "invitro_wordsearch_matches_2023-03-15.csv"), colClasses = "character")
#nrow(data_sel) # 22251

# read in manual edits
edits_df <- read.csv(file.path(main_dir,"Edits from previous versions",
  "manual_edits_v2021-12-17.csv"),colClasses = "character")
nrow(edits_df) #3451

# add column with Valerie Pence comments
data_sel <- left_join(data_sel,edits_df[,c("uid","VP_NOTES")])

data_edited <- data_sel
  
# update each edited column separately
colnames(edits_df)
for(i in 1:(ncol(edits_df)-2)){ #number of columns minus uid and VP_NOTES
  # select UID column and one other
  update <- edits_df[,c(1,i+2)]
  # remove rows that are blank (no edits)
  update <- update[which(update[,2]!="" | is.na(update[,2])),]
  print(nrow(update))
  # keep only rows that are in new dataset
  update <- update %>% filter(uid %in% data_edited$uid)
  print(nrow(update))
  # update target cells with matching uid
  data_edited <- data_edited %>% rows_update(update, by = "uid")
}

# order columns for final dataset and replace NA with "" for easier reading
data_edited <- data_edited %>%
  select(VP_NOTES,flag,uid,filename,publication_year,
         title_orig,abstract_orig,
         genera,families,families_found,families_genera,
         major_group,seed_status,excep_sp,
         key_capture,key_category,
         cryo_capture,cryo_category,
         non_capture,nonseed_capture) %>%
  mutate_if(is.character, ~replace_na(.,""))
str(as.data.frame(data_edited))

# write file
write.csv(data_edited,file.path(main_dir,
  paste0("invitro_wordsearch_matches_", Sys.Date(), ".csv")), row.names=F)



### NOT UPDATED YET...

###############################################################################
## Calculate stats
###############################################################################

# read in data
all_data_edited <- read.csv(file.path(main_dir,
  "cryo_wordsearch_matches_2022-03-23 - For R.csv"), colClasses = "character")
# recode a couple manual mistakes
unique(all_data_edited$flag)
unique(all_data_edited$key_category)
unique(all_data_edited$cryo_category)
freq.table(cryo$cryo_category)
all_data_edited <- all_data_edited %>%
  mutate(flag = recode(flag,
    "NON_Capture: Review/rec" = "NON_CAPTURE: Review/rec",
    "NON_CAPTURE: Not Plant" = "NON_CAPTURE: Not plant",
    "NON_CAPTURE: Fungi" = "NONSEED: Fungi")) %>%
  mutate(key_category = recode(key_category,
    "Cryo " = "Cryo",
    "Cryo-DT" = "Pre-cry")) %>%
  mutate(cryo_category = recode(cryo_category,
    "Embryo " = "Embryo",
    "Embyro, Seed" = "Embryo, Seed"))
table(all_data_edited$flag)
table(all_data_edited$key_category)
nrow(all_data_edited) #3015
#head(as.data.frame(all_data_edited))

# in vitro subset
invitro <- all_data_edited %>%
  filter(grepl("In Vitro",key_category)==TRUE & flag == "")
nrow(invitro) #1682

# cryo subset
cryo <- all_data_edited %>%
  filter(grepl("Cryo",key_category)==TRUE & flag == "")
nrow(cryo) #1798

##
### GENUS COUNTS
##

# takes into account all genera found in various searches

  # in vitro
iv_gen <- freq.table(invitro$genera)
iv_gen <- iv_gen %>%
  rename(InVitro_freq = Freq)
head(iv_gen,n=20)

  # cryo
cy_gen <- freq.table(cryo$genera)
cy_gen <- cy_gen %>%
  rename(Cryo_freq = Freq)
head(cy_gen,n=20)

  # add both next to each other for saving
top_genera <- full_join(iv_gen,cy_gen)
top_genera$InVitro_freq[which(is.na(top_genera$InVitro_freq))] <- 0
top_genera$Cryo_freq[which(is.na(top_genera$Cryo_freq))] <- 0
top_genera

##
### FAMILY COUNTS
##

# takes into account all the families found in various searches

  # in vitro
iv_fam <- freq.table(invitro$families)
iv_fam <- iv_fam %>%
  rename(InVitro_freq = Freq)
head(iv_fam,n=20)

  # cryo
cy_fam <- freq.table(cryo$families)
cy_fam <- cy_fam %>%
  rename(Cryo_freq = Freq)
head(cy_fam,n=20)

  # add both next to each other for saving
top_families <- full_join(iv_fam,cy_fam)
top_families$InVitro_freq[which(is.na(top_families$InVitro_freq))] <- 0
top_families$Cryo_freq[which(is.na(top_families$Cryo_freq))] <- 0
top_families

##
### EXCEPTIONAL SPECIES COUNTS
##

# read in Exceptional species list
excep_status <- read.csv(file.path(main_dir,
  "Exceptional Status List - No Justification.csv"), colClasses = "character")

  # in vitro
iv_excep <- freq.table(invitro$excep_sp)
iv_excep <- iv_excep %>%
  rename(InVitro_freq = Freq)
head(iv_excep,n=20)
iv_excep
iv_genus <- unlist(lapply(str_split(iv_excep$sep_items," "),"[[",1))
iv_genera <- as.data.frame(table(iv_genus))
iv_genera

  # cryo
cy_excep <- freq.table(cryo$excep_sp)
cy_excep <- cy_excep %>%
  rename(Cryo_freq = Freq)
head(cy_excep,n=20)
cy_excep
cy_genus <- unlist(lapply(str_split(cy_excep$sep_items," "),"[[",1))
cy_genera <- as.data.frame(table(cy_genus))
cy_genera
  # check number of species in specific family
excep_status %>% filter(WFO.family == "Passifloraceae") %>% select(Species.name)

  # add both next to each other for saving
top_excep_sp <- full_join(iv_excep,cy_excep)
top_excep_sp$InVitro_freq[which(is.na(top_excep_sp$InVitro_freq))] <- 0
top_excep_sp$Cryo_freq[which(is.na(top_excep_sp$Cryo_freq))] <- 0
top_excep_sp

# exceptional species genera
top_excep_sp$genus <- unlist(lapply(str_split(top_excep_sp$sep_items," "),"[[",1))
excep_genera <- as.data.frame(table(top_excep_sp$genus))

# compare families
excep <- excep_status %>% filter(Exceptional.status == "Exceptional")
length(unique(excep$WFO.family)) #111
length(unique(cy_fam$sep_items)) #128
length(which(
  (unique(cy_fam$sep_items) %in% unique(excep$WFO.family))==TRUE)) #111
length(which(
  (unique(cy_fam$sep_items) %in% unique(excep$WFO.family))==FALSE)) #74
length(which(
  (unique(excep$WFO.family) %in% unique(cy_fam$sep_items))==FALSE)) #37


##
### NON-SEED COUNTS
##

# in vitro subset
invitro_ns <- all_data_edited %>%
  filter(grepl("In Vitro",key_category)==TRUE &
         (flag == "NONSEED"))
nrow(invitro_ns) #146
# freq
iv_nseed <- table(invitro_ns$nonseed_capture)
iv_nseed

# cryo subset
cryo_ns <- all_data_edited %>%
  filter(grepl("Cryo",key_category)==TRUE &
        (grepl("NONSEED",flag)==TRUE))
nrow(cryo_ns) #77
# freq
table(cryo_ns$flag)
# genera and families found for bryophytes
unique(cryo_ns[which(grepl("Bryophyte",cryo_ns$flag)),]$genera)
unique(cryo_ns[which(grepl("Bryophyte",cryo_ns$flag)),]$families)
# genera and families found for pteridophytes
unique(cryo_ns[which(grepl("Pteridophyte",cryo_ns$flag)),]$genera)
unique(cryo_ns[which(grepl("Pteridophyte",cryo_ns$flag)),]$families)

  # add both next to each other for saving
nseed <- cbind(iv_nseed,cy_nseed)
nseed

##
### CRYO CATEGORY COUNTS
##

# freq
cy_catfreq <- freq.table(cryo$cryo_category)
cy_catfreq

# by family
  # create dataframe with duplicate row for each cryo_category listed
cryo_catsep <- cryo %>%
  mutate(cryo_category = strsplit(as.character(cryo_category), ", ")) %>%
  unnest(cryo_category)
str(cryo_catsep)
categories <- unique(cryo_catsep$cryo_category)
  # loop through and print family frequency for each cryo category
for(i in 1:length(categories)){
  sel <- cryo_catsep %>% filter(cryo_category == categories[i])
  sel_fam <- unlist(lapply(str_split(sel$families,", "),"[[",1))
  print(categories[i])
  print(as.data.frame(table(sel_fam)))
}

# by genus
  # loop through and print genus frequency for each cryo category
for(i in 1:length(categories)){
  sel <- cryo_catsep %>% filter(cryo_category == categories[i])
  sel_gen <- unlist(lapply(str_split(sel$genera,", "),"[[",1))
  print(categories[i])
  print(as.data.frame(table(sel_gen)))
}

##
### FABACEAE
##




##
### GENERA IN COMMON IN TARGET CRYO FAMILIES
##

# create list of genera found in lit search and match the family from WFO
all_gen_found <- unique(unlist(str_split(cryo$genera,", ")))
all_gen_df <- data.frame(genus = all_gen_found)
gen_fam_found <- left_join(all_gen_df,wfo_gen)

# list of families you want to search for
search_fam <- c("Arecaceae","Fabaceae","Orchidaceae","Rutaceae")

# loop through each family to get stats
for(i in 1:length(search_fam)){
  # print current family name
  print(paste0("Family: ",search_fam[i]))
  # Exceptional species genera
  ex <- excep_status %>% filter(WFO.family == search_fam[i]) %>% select(WFO.genus) %>% distinct(WFO.genus)
  print(paste0("Num Exceptional Species genera in target family: ",nrow(ex)))
  # Lit search genera
  ls <- gen_fam_found %>% filter(family == search_fam[i]) %>% select(genus) %>% distinct(genus)
  print(paste0("Num Literature Search genera in target family: ",nrow(ls)))
  # in common
  comm <- ex$WFO.genus[which(ex$WFO.genus %in% ls$genus)]
  print(paste0("Num in common: ",length(comm)))
  print(comm)
}



##
### CRYO FAMILIES FOR TOP EXCEP SPP
##

search_fam <- c("Caricaceae","Rutaceae","Arecaceae","Malvaceae","Fagaceae","Rubiaceae","Arecaceae",
"Anacardiaceae","Fagaceae","Ebenaceae","Euphorbiaceae","Lauraceae","Brassicaceae",
"Araucariaceae","Moraceae","Arecaceae","Rutaceae","Apocynaceae","Musaceae")

for(i in 1:length(search_fam)){
  ls <- cryo %>% filter(grepl(search_fam[i],families))
  print(nrow(ls))
}










wfo_fab_acc_sp <- wfo_all %>%
  filter(family == "Fabaceae") %>%
  #filter(genus == "Marmaroxylon")
  filter(taxonomicStatus == "ACCEPTED" | taxonomicStatus == "UNCHECKED") %>%
  filter(taxonRank == "SPECIES") %>%
  distinct(scientificName,genus) %>%
  group_by(genus) %>%
  count()
as.data.frame(wfo_fab_acc_sp)


wfo_fab_sp <- wfo_all %>%
  filter(family == "Fabaceae") %>%
  filter(taxonRank == "SPECIES") %>%
  distinct(scientificName,genus,taxonomicStatus) %>%
  group_by(genus) %>%
  count(taxonomicStatus) %>%
  pivot_wider(names_from = taxonomicStatus, values_from = n)
as.data.frame(wfo_fab_sp)











# THE PLANT LIST (TPL)

# read in TPL genera and families
  # pulled from this list:
  #   http://www.theplantlist.org/1.1/browse/-/-/
  #   and formatted into CSV document with col for genus and col for family
  # read in table
tpl_gen <- read.csv(file.path(main_dir,"TPL_genera_families_Mar2021.csv"),
  colClasses = "character")
str(tpl_gen)
# replace family name with non-ascii character
tpl_gen$family[which(tpl_gen$family == "Isoëtaceae")] <- "Isoetaceae"

# MERGE WFO AND TPL

# join WFO and TPL genera and families
gen_fam <- full_join(wfo_gen,tpl_gen); nrow(gen_fam) #41308





#### OLD SECTION for getting manual changes from original version
###############################################################################

original_version <- read.csv(file.path(main_dir,
                                       "invitro_cryo_wordsearch_matches_2021-03-12_Original.csv"),
                             colClasses = "character")

edited_version <- read.csv(file.path(main_dir,
                                     "invitro_cryo_wordsearch_matches_2021-03-12_AfterManualEdits.csv"),
                           colClasses = "character")

df_diffs <- diffs(comparedf(original_version, edited_version, by = "uid"))
unique(df_diffs$var.x)

t1 <- df_diffs %>%
  filter(var.x == "genus") %>%
  select(uid,values.y) %>%
  rename(VPgenus = values.y) %>%
  mutate_all(na_if,"")
t2 <- df_diffs %>%
  filter(var.x == "family") %>%
  select(uid,values.y) %>%
  rename(VPfamily = values.y) %>%
  mutate_all(na_if,"")
t3 <- df_diffs %>%
  filter(var.x == "major_group") %>%
  select(uid,values.y) %>%
  rename(VPmajor_group = values.y) %>%
  mutate_all(na_if,"")
t4 <- df_diffs %>%
  filter(var.x == "seed_status") %>%
  select(uid,values.y) %>%
  rename(VPseed_status = values.y) %>%
  mutate_all(na_if,"")
t5 <- df_diffs %>%
  filter(var.x == "key_category") %>%
  select(uid,values.y) %>%
  rename(VPkey_category = values.y) %>%
  mutate_all(na_if,"")
t6 <- df_diffs %>%
  filter(var.x == "cryo_category") %>%
  select(uid,values.y) %>%
  rename(VPcryo_category = values.y) %>%
  mutate_all(na_if,"")
t7 <- df_diffs %>%
  filter(var.x == "non_keys") %>%
  select(uid,values.y) %>%
  rename(VPnon_capture = values.y) %>%
  mutate_all(na_if,"")
t8 <- df_diffs %>%
  filter(var.x == "nonseed_keys") %>%
  select(uid,values.y) %>%
  rename(VPnonseed_capture = values.y) %>%
  mutate_all(na_if,"")

edits <- list(t1,t2,t3,t4,t5,t6,t7,t8)
edits_dfs <- lapply(edits,as.data.frame)
edits_df <- Reduce(full_join,edits_dfs)
edits_df[is.na(edits_df)] <- ""
edits_df <- as.data.frame(lapply(edits_df,function(x) gsub(",",";",x)),stringsAsFactors=F)
head(edits_df)

write.csv(edits_df,file.path(main_dir,
                             paste0("VP_3-12_edits.csv")), row.names=F)

