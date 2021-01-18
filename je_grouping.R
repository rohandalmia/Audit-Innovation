library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# function to return the number of words in a sentence
str_words = function(x) {
  split_words = str_split(x, " ")
  return(sapply(split_words, length))
}

# function to return the percentage match - question: should we take the min or max? mean?
word_match = function(x, y){
  list_x = sapply(x, str_split, " ", USE.NAMES = FALSE)
  list_y = sapply(y, str_split, " ", USE.NAMES = FALSE)
  return(map2(list_x, list_y, function(i, j) length(intersect(i, j))/mean(length(i), length(j))))
}

# data import
desc_data = read_excel("je_grouping.xlsx", sheet = "Raw Data")
je_desc = desc_data$Descriptions


# remove all puctuations for the grouping part
je_desc = gsub("\\s+"," ",str_remove_all(je_desc, "[:punct:]"))
groups = rep(NA, length(je_desc))
j = 1
# set threshold based on length - naive
threshold = ifelse(str_words(je_desc) > 6, (str_words(je_desc) - 3)/ str_words(je_desc), 0.50)

# compare a description to the next, skip if the match is less than threshold and set the
# unmatched desc as the primary comparing description
for(i in 1:length(je_desc)){
  i = j
  for(j in (i+1):length(je_desc)){
    if(any(unlist(word_match(je_desc[i], je_desc[j])) < threshold[i])) {
      print(paste(i, j, "break"))
      break
    } else {
      print(paste(i, j, "match"))
      groups[i] = paste0("Group: ", i)
      groups[j] = paste0("Group: ", i)   
    }
  }
  if(j >= length(je_desc)) break
}

groups[is.na(groups)] = which(is.na(groups))

desc_data = desc_data %>% 
  mutate(groupings = groups) 

