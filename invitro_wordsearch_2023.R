################################################################################
### Author: Emily Beckman Bruns
### Last Update: 26 July 2023

### Funding: Institute of Museum and Library Services
#            grant number MG-30-17-0055-17
#            Awarded to Cincinnati Zoo & Botanical Garden, Valerie Pence

### DESCRIPTION:
  # Word search analysis of Web of Science data; searching for family, genus,
  #   and keywords related to in vitro propagation.
  # Results published in:
  #   _______________________

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
my.packages <- c('tidyverse','stringr','readxl','textclean','countrycode','data.table'
                 # packages used if making heatmaps:
                 ,'terra','leaflet','BAMMtools','RColorBrewer'
)
lapply(my.packages, require, character.only=TRUE)
#  install.packages(my.packages) #Turn on to install current versions

# package citations...

#Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,
#Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller
#E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V,
#Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to
#the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
#doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

#Wickham H (2022). _stringr: Simple, Consistent Wrappers for Common
#String Operations_. R package version 1.5.0,
#<https://CRAN.R-project.org/package=stringr>.

#Wickham H, Bryan J (2023). _readxl: Read Excel Files_. R package
#version 1.4.2, <https://CRAN.R-project.org/package=readxl>.

#Rinker, T. W. (2018). textclean: Text Cleaning Tools version 0.9.3.
#Buffalo, New York. https://github.com/trinker/textclean

#Arel-Bundock V, Enevoldsen N, Yetman C (2018). “countrycode: An R
#package to convert country names and country codes.” _Journal of Open
#Source Software_, *3*(28), 848.
#<https://doi.org/10.21105/joss.00848>.

#Dowle M, Srinivasan A (2023). _data.table: Extension
#of `data.frame`_. R package version 1.14.8,
#<https://CRAN.R-project.org/package=data.table>.

#Hijmans R (2023). _terra: Spatial Data Analysis_. R package version
#1.7-29, <https://CRAN.R-project.org/package=terra>.

#Cheng J, Karambelkar B, Xie Y (2023). _leaflet: Create Interactive
#Web Maps with the JavaScript 'Leaflet' Library_. R package version
#2.1.2, <https://CRAN.R-project.org/package=leaflet>.

#Rabosky Daniel L, Michael C Grundler, Carlos J Anderson, Pascal O
#Title, Jeff J Shi, Joseph W Brown, Huateng Huang and Joanna G Larson.
#2014. BAMMtools: an R package for the analysis of evolutionary
#dynamics on phylogenetic trees. Methods in Ecology and Evolution
#5:701-707.

#Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package
#version 1.1-3, <https://CRAN.R-project.org/package=RColorBrewer>.

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

# function for calculating total frequency (e.g. for genera), for columns
#   with multiple items in each cell; used in final stats section
freq.table <- function(target.col,data_col,freq_col){
	# see max number of items (genera/families/etc.) for one record
	see_max <- sapply(target.col,function(x) str_count(x, pattern = ", "))
	# create array of separated items
	sep_items <- str_split_fixed(target.col, ", ", n = (max(see_max)+1))
	# sum to calculate frequency
	count_freq <- as.data.frame(table(sep_items))
	count_freq <- count_freq[-1,]
  count_freq <- count_freq %>% dplyr::arrange(desc(Freq))
  # add sequential number column, for joining and exporting later
  count_freq$order <- 1:nrow(count_freq)
  count_freq <- count_freq %>% select(order,sep_items,Freq)
  # change column names to be more meaningful
  setnames(count_freq, old = c("sep_items","Freq"), new = c(data_col,freq_col))
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

  # save list of all journals, for reference
  journals <- unique(all_data_raw$"Journal ISO Abbreviation")
  write.csv(journals,file.path(main_dir,"ListOfJournals_inWoSExports.csv"))

  # see now many records are from each search
  nrow(all_data_raw[which(grepl("Cryopreservation and Plant",all_data_raw$filename)),]) #2424
  nrow(all_data_raw[which(grepl("Cryopreservation and Seed",all_data_raw$filename)),]) #1277
  nrow(all_data_raw[which(grepl("Microprop",all_data_raw$filename)),]) #10519
  nrow(all_data_raw[which(grepl("SE",all_data_raw$filename)),]) #10256
  nrow(all_data_raw[which(grepl("Zygotic emb",all_data_raw$filename)),]) #1037
  
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
all_data_raw <- all_data_raw %>% 
  rename("title_orig" = "Article Title",
         "abstract_orig" = "Abstract",
         "uid" = "UT (Unique WOS ID)",
         "publication_year" = "Publication Year",
         "publisher_address" = "Publisher Address",
         "addresses" = "Addresses")

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
unique(wfo_gen$genus); nrow(wfo_gen) #37112 (v.2019.05 is 35856)
unique(wfo_gen$family)
# for v.2019.05 -- replace non-standard family name
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
                           # to add if running again (fixed by hand this time):
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

# note that the current workflow is slow and heavy - could be streamlined by
#   adding iterations for each pattern search. right now we look for all 
#   keywords (family, genus, etc.) at the same time in one giant expression. 
#   each keywork could instead be searched for individually in an interative
#   process

## --- IN TITLE --- ##

# PATTERN 1: genus name with non-alpha character before and after,
#            in title only and NO common names replaced
gen_list1 <- gen_list
gen_list1[1] <- paste0("[^a-zA-Z]",gen_list1[1])
gen_list1[length(gen_list1)] <- paste0(gen_list1[length(gen_list1)],"[^a-zA-Z]")
gen_pat1 <- paste(gen_list1,collapse="[^a-zA-Z]|[^a-zA-Z]")
  # view test matches
str_view(data_ex$title_orig, pattern = gen_pat1, match = T)
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
  # note that the "[^a-rt-zA-RT-Z]" expression allows for "s" to be matched;
  #  this means we can find plural versions of some keywords
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


### !!!
### NOW REVIEW AND EDIT THE DATA MANUALLY AS DESIRED
###  we did this in Google Sheets / Excel
### !!!


###############################################################################
###############################################################################
## Calculate stats...

###############################################################################
# Frequency stats
###############################################################################

# read in manually-edited data
all_data_edited <- read.csv(file.path(main_dir,
  "POST-MANUAL-EDITS_invitro_wordsearch_matches_2023-03-17_FOR-R.csv"), 
  colClasses = "character")
# recode a couple manual mistakes (not strictly necessary; just curious)
unique(all_data_edited$flag)
unique(all_data_edited$key_category)
all_data_edited <- all_data_edited %>%
  mutate(flag = recode(flag,
                       "NO GENUS/FAMILY" = "NO GENUS/FAMILY FOUND")) %>%
  mutate(key_category = recode(key_category,
                               "in Vitro" = "In Vitro"))
table(all_data_edited$flag)
table(all_data_edited$key_category)
nrow(all_data_edited) #22253

# subset of records to use for analyses (not flagged)
invitro <- all_data_edited %>% filter(flag == "")
nrow(invitro) #18935

##
### FAMILY COUNTS
##

# takes into account all the families found in various searches
iv_fam <- freq.table(invitro$families,"families","fam_freq")
head(iv_fam,n=25)

##
### GENUS COUNTS
##

# takes into account all genera found in various searches
iv_gen <- freq.table(invitro$genera,"genera","gen_freq")
head(iv_gen,n=25)

##
### EXCEPTIONAL SPECIES COUNTS
##

# read in Exceptional species list
excep_status <- read.csv(file.path(main_dir,
  "Exceptional Status List - No Justification.csv"), colClasses = "character")

# get frequency of exceptional species found in titles & abstracts
iv_excep <- freq.table(invitro$excep_sp,"excep_sp","excep_freq")
head(iv_excep,n=25)

# number of exceptional species in each family
excep_fam <- as.data.frame(table(excep_status$WFO.family))
colnames(excep_fam) <- c("excep_fam","excep_sp_fam")
t <- head(excep_fam %>% arrange(desc(excep_sp_fam)), n=25); t
top_excep_fam <- as.character(t[,1])

# number of exceptional species in each genus
excep_gen <- as.data.frame(table(excep_status$WFO.genus))
colnames(excep_gen) <- c("excep_gen","excep_sp_gen"); head(excep_gen)
t <- head(excep_gen %>% arrange(desc(excep_sp_gen)), n=25); t
top_excep_gen <- as.character(t[,1])

# not sure we used these? commenting out for now...
  # get frequency of exceptional genera found in titles & abstracts
#iv_genus <- unlist(lapply(str_split(iv_excep$sep_items," "),"[[",1))
#iv_genera <- as.data.frame(table(iv_genus))
#iv_genera
  # exceptional species genera
#top_excep_sp$genus <- unlist(lapply(str_split(top_excep_sp$sep_items," "),"[[",1))
#excep_genera <- as.data.frame(table(top_excep_sp$genus))
  # compare families
#excep <- excep_status %>% filter(Exceptional.status == "Exceptional")
#length(unique(excep$WFO.family)) #
#length(unique(cy_fam$sep_items)) #
#length(which((unique(cy_fam$sep_items) %in% unique(excep$WFO.family))==TRUE)) #111
#length(which((unique(cy_fam$sep_items) %in% unique(excep$WFO.family))==FALSE)) #74
#length(which((unique(excep$WFO.family) %in% unique(cy_fam$sep_items))==FALSE)) #37

##
### NON-SEED COUNTS
##

# create subset of records flagged as non-seed
nonseed <- all_data_edited %>% filter(flag == "NONSEED")
nrow(nonseed) #182

# calculate frequency of each type
ns_type <- table(nonseed$nonseed_capture)
ns_type

# genera and families found in each category
  # bryophytes
ns_bryo <- nonseed[which(nonseed$nonseed_capture == "bryophyte"),]
bryo_gen <- freq.table(ns_bryo$genera,"bryo_gen","bryo_gen_freq"); bryo_gen
bryo_fam <- freq.table(ns_bryo$families,"bryo_fam","bryo_fam_freq"); bryo_fam
  # pteridophytes
ns_pteri <- nonseed[which(nonseed$nonseed_capture == "pteridophyte"),]
pteri_gen <- freq.table(ns_pteri$genera,"pteri_gen","pteri_gen_freq"); pteri_gen
pteri_fam <- freq.table(ns_pteri$families,"pteri_fam","pteri_fam_freq"); pteri_fam
  # algae
ns_algae <- nonseed[which(nonseed$nonseed_capture == "algae"),]
algae_gen <- freq.table(ns_algae$genera,"algae_gen","algae_gen_freq"); algae_gen
algae_fam <- freq.table(ns_algae$families,"algae_fam","algae_fam_freq"); algae_fam

##
### PASTE ALL RESULTS TOGETHER THEN EXPORT
##

# data to paste together
results_dfs <- list(iv_fam,iv_gen,iv_excep,
                    bryo_fam,bryo_gen,pteri_fam,pteri_gen,algae_fam,algae_gen)
# paste all side-by-side
all_results <- Reduce(function(x, y) full_join(x, y, by = "order"), 
                      results_dfs)
# replace NA with "" for easier viewing
all_results[is.na(all_results)] <- ""

head(all_results)
tail(all_results)

# write file
write.csv(all_results,
          file.path(main_dir,paste0("invitro_wordsearch_stats_", 
                                    Sys.Date(), ".csv")), row.names=F)

###############################################################################
# Other stats
###############################################################################

##
### PERCENT TREES BY GENUS
##

# read in World Flora Online backbone
wfo_all <- read.delim(file.path(main_dir,"WFO_Backbone",
                                # replace the version number as needed:
                                "v.2021.01","classification.txt"),
                      colClasses = "character")
# summarize to get number of species in each genus
wfo_sel <- wfo_all %>%
  filter(taxonRank == "SPECIES" & 
         (taxonomicStatus == "ACCEPTED" | taxonomicStatus == "UNCHECKED")) %>%
  count(genus)

# read in GlobalTreeSearch data (CSV file of all tree taxa in the database)
gts <- read.csv(file.path(main_dir,"global_tree_search_trees_1_6.csv"), 
                colClasses = "character")
# summarize to get number of species in each genus
gts_gen <- gts %>% 
  separate(TaxonName, into = c("genus","species"), sep = " ") %>%
  count(genus)

# calculate % trees for top genera in lit review and exceptional species list
  # top 25 lit review genera
litgen_25 <- as.data.frame(as.character(iv_gen[1:25,2]))
colnames(litgen_25) <- "genus"
litgen_25 <- left_join(litgen_25,wfo_sel,by="genus")
litgen_25 <- left_join(litgen_25,gts_gen,by="genus")
litgen_25[which(is.na(litgen_25$n.y)),]$n.y <- 0
litgen_25$trees <- litgen_25$n.y / litgen_25$n.x
litgen_25
  # top 25 exceptional genera
excgen_25 <- as.data.frame(c("Shorea","Cyanea","Quercus","Artocarpus","Melicope",
                           "Coprosma","Lysimachia","Dipterocarpus","Citrus",
                           "Cyrtandra","Pittosporum","Araucaria","Hopea","Inga",
                           "Syzygium","Clermontia","Aesculus","Garcinia",
                           "Bruguiera","Coffea","Diospyros","Rhizophora",
                           "Trichilia"))
colnames(excgen_25) <- "genus"
excgen_25 <- left_join(excgen_25,wfo_sel)
excgen_25 <- left_join(excgen_25,gts_gen,by="genus")
excgen_25[which(is.na(excgen_25$n.y)),]$n.y <- 0
excgen_25$trees <- excgen_25$n.y / excgen_25$n.x
excgen_25

##
### GENERA IN COMMON IN TARGET FAMILIES
##

# keep just genus and family columns in WFO dataset
wfo_gen <- wfo_all %>% distinct(genus,.keep_all=T) %>%
  filter(genus != "") %>% dplyr::select(family,genus)

# create list of genera found in lit search and match the family from WFO
all_gen_found <- unique(unlist(str_split(invitro$genera,", ")))
all_gen_df <- data.frame(genus = all_gen_found)
gen_fam_found <- left_join(all_gen_df,wfo_gen)

# list of families you want to search for
search_fam <- c("Arecaceae","Asteraceae","Fabaceae","Orchidaceae","Rutaceae")

# loop through each family to get stats
for(i in 1:length(search_fam)){
  # print current family name
  print(paste0("Family: ",search_fam[i]))
  # Exceptional species genera
  ex <- excep_status %>% 
    filter(WFO.family == search_fam[i] & Exceptional.status == "Exceptional") %>% 
    select(WFO.genus) %>% distinct(WFO.genus)
  print(paste0("Num Exceptional Species genera in target family: ",nrow(ex)))
  # Lit search genera
  ls <- gen_fam_found %>% 
    filter(family == search_fam[i]) %>% 
    select(genus) %>% distinct(genus)
  print(paste0("Num Literature Search genera in target family: ",nrow(ls)))
  # in common
  comm <- ex$WFO.genus[which(ex$WFO.genus %in% ls$genus)]
  print(paste0("Num in common: ",length(comm)))
  print(comm)
}

##
### PUBLISHER AND AUTHOR COUNTRIES FOR TOP EXCEPTIONAL FAMILIES/GENERA
##

# get publisher/author country frequency data for target families/genera
ctry.freq <- function(top_list,top_list_colnum,target_freq_colnum){
  # create blank dataframes we will fill in the loop below
  top <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Var1", "Freq", "category"))
  # loop through each family/genus to get countries and frequency for each
  for(i in 1:length(top_list)){
    temp <- as.data.frame(table(invitro[which(grepl(
      top_list[i],invitro[,top_list_colnum])),target_freq_colnum]))
    # when no countries, create manually
    if(nrow(temp) == 0){ 
      temp <- data.frame(Var1 = "None", Freq = 0)
    }
    # if more than once country in a cell, we need to split again and sum
    if("TRUE" %in% as.character(grepl(",",temp$Var1))){ 
      # split by comma, aggregate by country, and sum number of articles
      temp <- temp %>% 
        separate_rows(Var1, sep = ", ") %>%
        aggregate(Freq ~ Var1, sum)
    }
    temp$Var1 <- as.character(temp$Var1)
    temp$Var1[which(temp$Var1 == "N/A")] <- "None"
    temp$category <- top_list[i]
    top <- rbind(top,temp)
  }
  # reshape from long into a wide format
  top_sum <-reshape(top, idvar = "category", timevar = "Var1", direction = "wide")
  # replace prefixes in column names
  colnames(top_sum) <- gsub(x = colnames(top_sum),
                            pattern = "Freq\\.", replacement = "")
  # replace NA with 0
  top_sum[is.na(top_sum)] <- 0
  # transpose
  top_sum_t <- data.table::transpose(top_sum,make.names="category")
  rownames(top_sum_t) <- colnames(top_sum)[2:length(colnames(top_sum))]
  # add sum column & sort high to low
  ROW_SUM <- rowSums(top_sum_t); top_sum_t <- cbind(ROW_SUM,top_sum_t)
  top_sum_t <- top_sum_t %>% arrange(desc(ROW_SUM))
  return(as.data.frame(top_sum_t))
}

## publisher country
top_excep_fam_pubctry <- ctry.freq(top_excep_fam,11,6)
  write.csv(top_excep_fam_pubctry,file.path(main_dir,"invitro_top_excep_fam_pubctry.csv"))
top_excep_gen_pubctry <- ctry.freq(top_excep_gen,10,6)
  write.csv(top_excep_gen_pubctry,file.path(main_dir,"invitro_top_excep_gen_pubctry.csv"))

## author countries
top_excep_fam_authctry <- ctry.freq(top_excep_fam,11,7)
  write.csv(top_excep_fam_authctry,file.path(main_dir,"invitro_top_excep_fam_authctry.csv"))
top_excep_gen_authctry <- ctry.freq(top_excep_gen,10,7)
  write.csv(top_excep_gen_authctry,file.path(main_dir,"invitro_top_excep_gen_authctry.csv"))

##
### HEATMAP OF PUBLISHER AND AUTHOR COUNTRIES
##

# read in countries shapefile
  # https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0?geometry=23.192%2C13.203%2C-13.370%2C79.425
world_shp <- vect(file.path(main_dir,"UIA_World_Countries_Boundaries/World_Countries__Generalized_.shp"))
  # make into object that can be mapped in leaflet
world_shp_sf <- sf::st_as_sf(world_shp)

# function to calculate country richness and join to polygon data
richness.poly <- function(df,polygons,richness_colnum){
  # if more than once country in a cell, split first then sum
  if("TRUE" %in% as.character(grepl(",",df[,richness_colnum]))){ 
    # see max number of countries for one row
    count_ctry <- sapply(df[,richness_colnum],function(x) 
      str_count(x, pattern = ", "))
    # create array of separated country names
    COUNTRY <- str_split_fixed(df[,richness_colnum], ", ", n = (max(count_ctry)+1))
  } else {
    COUNTRY <- df[,richness_colnum]
  }
  # sum to calculate richness
  richness <- as.data.frame(table(COUNTRY))
  print(richness)
  # merge polygons with richness data
  merged <- merge(polygons,richness)
  #merged@data$Freq[which(is.na(merged@data$Freq))] <- 0
  #merged <- merged[merged@data$Freq > 0,]
  # see countries that didn't match
  print("Unmatched country names:")
  print(setdiff(unique(richness$COUNTRY),unique(merged$COUNTRY)))
  return(merged)
}

# countries in invitro data that don't have exact match in country shapefile
# edit manually to match
unique(world_shp$COUNTRY)
invitro <- invitro %>%
  mutate(publisher_country=recode(publisher_country,
                                  "Czechia"="Czech Republic",
                                  "Hong Kong"="China",
                                  "Russia"="Russian Federation",
                                  "Taiwan"="China",
                                  "Trinidad & Tobago"="Trinidad and Tobago",
                                  "Turkey"="Turkiye",
                                  "US"="United States"))
invitro$author_countries <- mgsub(invitro$author_countries,
  c("Bosnia","Brunei","Cote Ivoire","Czechia",
    "Palestine","Russia","Taiwan","Trinidad & Tobago","Turkey",
    "US","Yemen Arab Republic"),
  c("Bosnia and Herzegovina","Brunei Darussalam","Côte d'Ivoire","Czech Republic",
    "Israel","Russian Federation","China","Trinidad and Tobago","Turkiye",
    "United States","Yemen"))

# calculate richness for each country
map_pubctry <- richness.poly(invitro,world_shp,6)
map_authctry <- richness.poly(invitro,world_shp,7)
  # for exceptional species articles only
  invitro_excep <- invitro[invitro$excep_sp!="",]
  map_pubctry_excep <- richness.poly(invitro_excep,world_shp,6)
  map_authctry_excep <- richness.poly(invitro_excep,world_shp,7)
# make into object that can be mapped in leaflet
map_pubctry <- sf::st_as_sf(map_pubctry)
map_authctry <- sf::st_as_sf(map_authctry)
map_pubctry_excep <- sf::st_as_sf(map_pubctry_excep)
map_authctry_excep <- sf::st_as_sf(map_authctry_excep)

# mapping function
map.countries <- function(countries,pal,legend_text,legend_labels){ 
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addPolygons(data = world_shp_sf,
                color = "grey", weight = 0.5, opacity = 1,
                fillColor = "white",fillOpacity = 1) %>%
    addPolygons(data = countries,
                color = "grey", weight = 1.7, opacity = 1,
                fillColor = ~pal(countries$Freq),
                fillOpacity = 1) %>%
    addLegend(values = countries$Freq,
              pal = pal, opacity = 1,
              title = legend_text,
              labFormat = function(type, cuts, p) {paste0(legend_labels)},
              position = "bottomleft")
  return(map)
}

# create color bins and labels, create color palette, create map

## publisher country
  # look at data distribution
hist(map_pubctry$Freq,breaks=90,xlim=c(0,5000),ylim=c(0,40))
max(map_pubctry$Freq) #4499
  # see where natural breaks are (Jenks) - can either use these or your own
getJenksBreaks(map_pubctry$Freq,9) # number is how many breaks you want
  # change bins and labels manually
bins <- c(0,50,100,200,300,600,1000,2000,4000,Inf) # natural breaks
labels <- c("1 - 49","50 - 99","100 - 199","200 - 299","300 - 599","600 - 999",
            "1000 - 1999","2000 - 3999","4000+")
bins <- c(0,500,1000,1500,2000,2500,3000,3500,4000,Inf) # even breaks
labels <- c("1 - 499","500 - 999","1000 - 1499","1500 - 1999","2000 - 2499",
            "2500 - 2999","3000 - 3499","3500 - 3999","4000+")
  # see color palette options with display.brewer.all()
palette_ctry <- colorBin(palette = "RdPu", bins = bins,
                         domain = map_pubctry$Freq, reverse = F, 
                         na.color = "white")
  # legend title
legend <- paste0("Number of articles","<br/>",
                 "published in the country")
  # create and view map using function above
map_heat_pubctry <- map.countries(map_pubctry,palette_ctry,legend,labels)
map_heat_pubctry
  # save map
#htmlwidgets::saveWidget(file.path(map_heat_pubctry,"PublisherCountries_map.html"))

## author countries
hist(map_authctry$Freq,breaks=90,xlim=c(0,3000),ylim=c(0,40))
max(map_authctry$Freq) #2651
getJenksBreaks(map_authctry$Freq,9)
labels <- c("1 - 49","50 - 99","100 - 199","200 - 299","300 - 499","500 - 699",
            "700 - 999","1000 - 1999","2000+")
bins <- c(0,250,500,750,1000,1250,1500,1750,2000,Inf) # even breaks
labels <- c("1 - 249","250 - 499","500 - 749","750 - 999","1000 - 1249",
            "1250 - 1499","1500 - 1749","1750 - 1999","2000+")
palette_ctry <- colorBin(palette = "RdPu", bins = bins,
                         domain = map_authctry$Freq, reverse = F, 
                         na.color = "white")
legend <- paste0("Number of articles with","<br/>",
                 "at least one author's","<br/>",
                 "institution in the country")
map_heat_authctry <- map.countries(map_authctry,palette_ctry,legend,labels)
map_heat_authctry
#htmlwidgets::saveWidget(file.path(map_heat_authctry,"AuthorCountries_map.html"))

## publisher country - EXCEPTIONAL SPECIES
hist(map_pubctry_excep$Freq,breaks=90,xlim=c(0,400),ylim=c(0,40))
max(map_pubctry_excep$Freq) #368
# skipping natural breaks since I don't think we are using
#getJenksBreaks(map_pubctry$Freq,9) # number is how many breaks you want
#bins <- c(0,50,100,200,300,600,1000,2000,4000,Inf) # natural breaks
#labels <- c("1 - 49","50 - 99","100 - 199","200 - 299","300 - 599","600 - 999",
#            "1000 - 1999","2000 - 3999","4000+")
bins <- c(0,40,80,120,160,200,240,280,320,Inf) # even breaks
labels <- c("1 - 39","40 - 79","80 - 119","120 - 159","160 - 199",
            "200 - 239","240 - 279","280 - 319","320+")
palette_ctry <- colorBin(palette = "RdPu", bins = bins,
                         domain = map_pubctry_excep$Freq, reverse = F, 
                         na.color = "white")
legend <- paste0("Number of articles","<br/>",
                 "on exceptional species","<br/>",
                 "published in the country")
map_heat_pubctry_excep <- map.countries(map_pubctry_excep,palette_ctry,legend,labels)
map_heat_pubctry_excep
#htmlwidgets::saveWidget(file.path(map_heat_pubctry_excep,"PublisherCountriesExceptional_map.html"))

## author countries - EXCEPTIONAL SPECIES
hist(map_authctry_excep$Freq,breaks=90,xlim=c(0,300),ylim=c(0,40))
max(map_authctry_excep$Freq) #252
# skipping natural breaks since I don't think we are using
#getJenksBreaks(map_authctry$Freq,9)
#bins <- c(0,50,100,200,300,500,700,1000,2000,Inf) # natural breaks
#labels <- c("1 - 49","50 - 99","100 - 199","200 - 299","300 - 499","500 - 699",
#            "700 - 999","1000 - 1999","2000+")
bins <- c(0,30,60,90,120,150,180,210,240,Inf) # even breaks
labels <- c("1 - 29","30 - 59","60 - 89","90 - 119","120 - 149",
            "150 - 179","180 - 209","210 - 239","240+")
palette_ctry <- colorBin(palette = "RdPu", bins = bins,
                         domain = map_authctry_excep$Freq, reverse = F, 
                         na.color = "white")
legend <- paste0("Number of articles","<br/>",
                 "on exceptional species","<br/>",
                 "with at least one author's","<br/>",
                 "institution in the country")
map_heat_authctry_excep <- map.countries(map_authctry_excep,palette_ctry,legend,labels)
map_heat_authctry_excep
#htmlwidgets::saveWidget(file.path(map_heat_authctry_excep,"AuthorCountriesExceptional_map.html"))
