# 0_read_carlisle.R
# read in the hand-entered data on trials by John Carlisle
# data available in supplement here https://associationofanaesthetists-publications.onlinelibrary.wiley.com/doi/full/10.1111/anae.13938
# find the Pubmed ID
library(rentrez)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)

#entrez_fetch(db='pubmed', id=17635433, rettype='xml')

## section 1: download John's data ##
file = 'https://associationofanaesthetists-publications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fanae.13938&file=anae13938-sup-0001-AppendixS1.xlsx'
download.file(url = file, destfile = 'data/temp.xlsx', mode = "wb") # create local temporary file
# find the sheets, one for each journal
journals = excel_sheets(path='data/temp.xlsx')
journals = journals[!str_detect(journals, pattern='^_')] # remove sheet that is not a journal
# loop through journals
carlisle_data = NULL
for (this_journal in journals){
  carlisle = read_excel(path='data/temp.xlsx', sheet=this_journal, .name_repair='minimal') %>%
    clean_names() %>%
    mutate(page = as.numeric(page)) # for consistency
  # rename for consistency
  if(any(names(carlisle) == 'vol')){
    carlisle = rename(carlisle, 'volume' = 'vol')
  }
  #
  carlisle = select(carlisle, trial, year, volume, page) %>%
    mutate(journal = this_journal)
  carlisle_data = bind_rows(carlisle_data, carlisle)
}

# restrict to more recent papers (full range is 2000 to 2015)
carlisle_data = filter(carlisle_data, year >= 2010)
# rename journals to give better match with pubmed
carlisle_data = mutate(carlisle_data,
                        journal = case_when(
                          journal == 'AA' ~ 'Anesthesia and Analgesia',
                          journal == 'CJA' ~ 'Canadian Journal of Anesthesia',
                          journal == 'EJA' ~ 'European Journal of Anaesthesiology',
                          TRUE ~ as.character(journal)
                        ))

## section 2: search for specific trials ##
trial_data = NULL
for (k in 1:nrow(carlisle_data)){
  this = carlisle_data[k,]
  search_term = paste(this$journal, '[JOUR] AND ', 
    this$volume, '[VOL] AND ',
    this$page, '[PAGE]', sep='')
  pubmed_search <- entrez_search(db="pubmed", term = search_term)
  if(pubmed_search$count==1){ # only if single match
    this = mutate(this, pubmed = pubmed_search$ids) 
    trial_data = bind_rows(trial_data, this)
  }
}
# number missed
cat('Out of ', nrow(carlisle_data), ' there were ',  nrow(carlisle_data) - nrow(trial_data) ,' not found in pubmed.\n', sep='')

# save
save(trial_data, file='data/carlisle_trials.RData')