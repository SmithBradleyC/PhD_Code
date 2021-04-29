# https://childes-db.stanford.edu/api.html

library(childesr)
library(dplyr)

# get all transcripts
d_transcripts <- get_transcripts()
head(d_transcripts)

# just english-american transcripts
d_eng_na <- get_transcripts(collection = "Eng-NA")
head(d_eng_na)
nrow(d_eng_na)

d_participants <- get_participants()
head(d_participants)

d_target_child <- get_participants(role = "target_child")
head(d_target_child)
