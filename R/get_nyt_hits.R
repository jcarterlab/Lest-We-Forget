library(dplyr)
library(tidyverse)
library(tidyr)
library(tidytext)
library(jsonlite)

# key. 
nyt_key <- "02EVKWAkGy7oFyJtjKgjD847l5K3HpXq"

# creates a data frame of start and end dates. 
dates <- tibble(war = c("crimea", "ukraine", "syria", "iraq", "afghanistan", "yemen"),
                start_date = c("20140220", "20220224", "20110315", "20030320", "20011007", "20140916"))

# gets start and end dates. 
get_dates <- function(date, months, label, end = FALSE) {
  
  # creates separate objects for year, month and day. 
  year <- as.numeric(substr(date, 1, 4))
  month <- as.numeric(substr(date, 5, 6))
  day <- as.numeric(substr(date, 7, 8))
  
  # gets the end dates if the end parameter is marked TRUE. 
  if(end==TRUE) {
    month <- month + 1
    day <- day - 1
  }
  
  # limits days to 28 to deal with shorter months (like February). 
  if(day > 28) day <- 28
  
  # loops through the start dates and updates and resets the
  # the month and years if the month count goes over 12. 
  dates <- list()
  for(i in 1:months) {
    if(month >= 13) {
      year <- year + 1
      month <- 1
    }
    
    # assigns i start dates while pasting an extra zero before those 
    # months and days less than 10 (i.e. 9 - (only has 1 digit)). 
    if(month >= 10 & day >= 10) {
      dates[i] <- as.numeric(paste0(year, month, day)) 
      month <- month +1
    } else if(month >= 10 & day < 10) {
      dates[i] <- as.numeric(paste0(year, month, paste0(0,day))) 
      month <- month +1
    } else if(month < 10 & day >= 10) {
      dates[i] <- as.numeric(paste0(year, paste0(0,month), day)) 
      month <- month +1
    } else {
      dates[i] <- as.numeric(paste0(year, paste0(0,month), paste0(0,day))) 
      month <- month +1
    }
  }
  return(unlist(dates))
}

# stores the start and end dates. 
start <- lapply(dates$start_date, get_dates, months = 6)
end <- lapply(dates$start_date, get_dates, months = 6, end = TRUE)
 
# transforms the data into a clean data frame. 
dfs <- list()
for(i in 1:nrow(dates)) {
  dfs[[i]] <- tibble("war" = dates$war[i],
                     "start_dates" = unlist(start[i]),
                     "end_dates" = unlist(end[i]),
                     "month" = 1:6)
}
clean_dates_df <- rbind_pages(dfs)


# find out how many results are returned for a given term and date range.  
get_hits_data <- function(start_dates, end_dates, terms) {
  url <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=%22",
                terms,
                "%22&begin_date=",
                start_dates,
                "&end_date=",
                end_dates,
                "&facet_filter=true&api-key=",
                nyt_key, 
                sep="")
  # query. 
  results_counter <- 1L
  results <- list()
  search <- repeat{try({query <- fromJSON(url, flatten = TRUE)})
    # error handling. 
    if(exists("query")) {
      results <- query
      rm(query)
      break 
    } else {
      if(results_counter <= 45L) {
        message("Re-trying query: attempt ", results_counter, " of 45.")
        results_counter <- results_counter +1L
        Sys.sleep(1)
      } else {
        message("Retry limit reached: initial query unsuccessful.")
        break
      }
    }
  }
  return(results$response$meta$hits)
}

# loops through each start/end date. 
get_all_hits <- function(start_dates, end_dates, month, terms) {
  
  list <- list()
  for(i in 1:length(terms)){
    
    message("Month ", i, " of ", length(start_dates))
    
    list[[i]] <- tibble(Hits = get_hits_data(start_dates[i], end_dates[i], terms[i])) %>%
      mutate("War" = terms[i],
             "Month" = month[i])
    
    Sys.sleep(10)
  }
  return(tibble(rbind_pages(list)))
}

# stores the number of hits for each month of each conflict.  
hits <- get_all_hits(start_dates = clean_dates_df$start_dates,
                     end_dates = clean_dates_df$end_dates,
                     month = clean_dates_df$month,
                     terms = clean_dates_df$war)

# saves the data to a csv file. 
write_csv(hits, "C:/Users/HUAWEI/Desktop/Projects/Lest-We-Forget/Data/hits.csv")
