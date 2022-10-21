

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)
library(rvest)


all_states <- datasets::state.name
all_states <- gsub(" ", "_", all_states)


get_stations <- function(state){

root <- paste0("https://en.wikipedia.org/wiki/List_of_radio_stations_in_", state) 
 tables <- read_html(root) %>% html_nodes("table")
  stations <- tables[1] %>% # ideally all pages will have same format
              html_table(header = TRUE) %>% 
              do.call(rbind, .) %>% 
              clean_names() %>% 
              mutate(frequency = as.character(frequency)) %>%                    # handling Ohio special case (frequency / band split)
              rename_if(startsWith(names(.), "city"), ~ ("city")) %>%            # handling naming issues, citation handler
              rename_at(vars(matches("format")), ~ "format") %>%                 # handling naming issues, Oklahoma handler
              rename_if(startsWith(names(.), "licensee"), ~ ("licensee")) %>%    # handling naming issues, citation handler
              rename_if(startsWith(names(.), "owner"), ~ ("licensee")) %>%       # South Dakota handler
              select(call_sign, frequency, city, licensee, format) %>% 
              mutate(
                state = state
              )
  
  return(stations)
}

state_stations <- map_dfr(all_states, get_stations)
