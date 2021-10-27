# title:   Lab 02
# author:  Robert M. Trehern
# date:    10/27/2021

# theme search function ----
# searches for user defined themes in the category column

theme_search <- function(data, themes){ 
  themes.search <- grepl(themes, data$category, ignore.case=T)
  data.themes <- data[themes.search, c("root", "root2", "category", "definition", "X1970.f",
                                       "X1970.s", "X1980.f", "X1980.s", "X1990.f", "X1990.s",
                                       "X2000.f", "X2000.s", "X2010.f", "X2010.s")]
  return(data.themes)
}


#-------------------------------------------------------------------------------


# keyword search function ----
# searches for any keyword entered by the user in the first four columns of the data set
keyword_search <- function(data, keywords){ 
  keywords.search <- grepl(keywords, data$definition, ignore.case=T)
  data.keywords <- data[keywords.search, c("root", "root2", "category", "definition", "X1970.f",
                                           "X1970.s", "X1980.f", "X1980.s", "X1990.f", "X1990.s",
                                           "X2000.f", "X2000.s", "X2010.f", "X2010.s")]
  return(data.keywords)
}

#-------------------------------------------------------------------------------


# time period function ----

dd$`1970` <- coalesce(dd$`X1970.f`, dd$`X1970.s`)
dd$`1980` <- coalesce(dd$`X1980.f`, dd$`X1980.s`)
dd$`1990` <- coalesce(dd$`X1990.f`, dd$`X1990.s`)
dd$`2000` <- coalesce(dd$`X2000.f`, dd$`X2000.s`)
dd$`2010` <- coalesce(dd$`X2010.f`, dd$`X2010.s`)

time.df <- dd %>% 
  select(root, root2, category, definition, `1970`, `1980`, `1990`, `2000`, `2010`)

time <- "1970|1980|1990|2000|2010"

time_search <- function(time.df, time){
  time.search <- unique(grep(pattern = time, colnames(time.df), value = TRUE))
  time.data <- time.df %>%
    select(root, root2, category, definition, time.search)
  time.data <- na.omit(time.data)
  return (time.data)
}
