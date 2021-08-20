### ------------------- WP6 Data  ----------------------- ###
### Script_04_Recruiting.R                                ###
### Author: Patrizia Maier                                ###


# get packages 
library(tidyverse)
library(openxlsx)
library(readxl)


# function for reading all sheets of an excel document 
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, skip=2))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


# get recruitment list 
# sheets were cleaned in excel (same number of columns)
filename <- readline(prompt = "Enter path to recruitment document: ")
mysheets <- read_excel_allsheets(filename)

invited <- do.call(rbind.data.frame, mysheets) %>% 
  select(-c(Alter, Adresse, `Termin Tag`, `Termin Uhrzeit`, Rückmeldung, Notizen )) %>% 
  filter(`Brief verschickt` != "nein") %>% 
  select(-c(`Brief verschickt`)) %>% 
  unique() %>% 
  unite(Vorname, Nachname, col="Name", sep=" ") %>% 
  arrange(Name) 
# strings were cleaned in excel sheet (Dr., Dr. med. and Fr. removed)


# get ID list 
filename_2 <- readline(prompt = "Enter path to id list: ")
participated <- read_excel(filename_2, skip=1, n_max=52, trim_ws = T) %>% 
  select(Name) %>% 
  arrange(Name)


# People that received a letter and participated 
length(invited$Name) # n letters 
length(invited$Name[invited$Name %in% participated$Name]) # n respondents
length(invited$Name[invited$Name %in% participated$Name])/length(invited$Name)*100 # percent

# People that participated but did not receive a letter
length(participated$Name[!participated$Name %in% invited$Name])
participated$Name[!participated$Name %in% invited$Name]
