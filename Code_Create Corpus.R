#########################################################
##      Creating a corpus from Nexis Uni articles     ###
#########################################################


install.packages("LexisNexisTools")
library("LexisNexisTools")
library(tidyverse)

#Open my data
setwd("C:/Users/leale/Desktop/Thesis/Articles/Full corpus 2007_19_2")
LNToutput <- lnt_read("C:/Users/leale/Desktop/Thesis/Articles/Full corpus 2007_19_2")

meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs


# Print meta to get an idea of the data
head(meta_df, n = 3)

#Find (very) similar articles: 
duplicates_df <- lnt_similarity(LNToutput = LNToutput,
                                threshold = 0.97)
lnt_diff(duplicates_df, min = 0, max = Inf)


duplicates_df <- duplicates_df[duplicates_df$rel_dist < 0.2]
LNToutput <- LNToutput[!LNToutput@meta$ID %in% duplicates_df$ID_duplicate, ]

#Generate new dataframes without highly similar duplicates
meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs


#convert LNToutput objects to formats common in other packages 
quanteda_corpus <- lnt_convert(LNToutput, to = "quanteda")

save(quanteda_corpus, file = "quanteda_corpus.RData")
