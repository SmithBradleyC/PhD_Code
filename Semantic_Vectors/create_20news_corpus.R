library(dplyr)
library(tidyr)
library(purrr)
library(readr)

#untar("20news-bydate.tar.gz")

training_folder <- "20news-bydate-train/"

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

# Use unnest() and map() to apply read_folder to each subfolder
raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text
#> # A tibble: 511,655 x 3
#>    newsgroup   id    text                                                       
#>    <chr>       <chr> <chr>                                                      
#>  1 alt.atheism 49960 "From: mathew <mathew@mantis.co.uk>"                       
#>  2 alt.atheism 49960 "Subject: Alt.Atheism FAQ: Atheist Resources"              
#>  3 alt.atheism 49960 "Summary: Books, addresses, music -- anything related to a…
#>  4 alt.atheism 49960 "Keywords: FAQ, atheism, books, music, fiction, addresses,…
#>  5 alt.atheism 49960 "Expires: Thu, 29 Apr 1993 11:57:19 GMT"                   
#>  6 alt.atheism 49960 "Distribution: world"                                      
#>  7 alt.atheism 49960 "Organization: Mantis Consultants, Cambridge. UK."         
#>  8 alt.atheism 49960 "Supersedes: <19930301143317@mantis.co.uk>"                
#>  9 alt.atheism 49960 "Lines: 290"                                               
#> 10 alt.atheism 49960 ""                                                         
#> # … with 511,645 more rows

library(ggplot2)

raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(messages, newsgroup)) +
  geom_col() +
  labs(y = NULL)


library(stringr)

# must occur after the first occurrence of an empty line,
# and before the first occurrence of a line starting with --
cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()


cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))

cleaned_text$text<-paste0(cleaned_text$text," bradsnonsensewordtoflagendofsentence")

library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word
         )

head(usenet_words)

final_text<-paste(usenet_words$word,collapse = " ")
library(stringr)
final_text<-str_replace_all(string = final_text,pattern = " bradsnonsensewordtoflagendofsentence ",replacement = "\n")
final_text<-str_replace_all(string = final_text,pattern = "bradsnonsensewordtoflagendofsentence ",replacement = "\n")
final_text<-str_replace_all(string = final_text,pattern = " bradsnonsensewordtoflagendofsentence",replacement = "\n")
final_text<-str_replace_all(string = final_text,pattern = "bradsnonsensewordtoflagendofsentence",replacement = "\n")
final_text<-str_replace_all(string = final_text,pattern = "  ",replacement = " ")

write(x = final_text,file = "final_text.txt")

usenet_words %>%
  count(word, sort = TRUE)
