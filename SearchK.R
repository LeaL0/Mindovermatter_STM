######################################################
#                Search ideal K                     ##
######################################################

#Load data
library(stm)
library(quanteda)
library(plyr)
library(ggplot2)
library(tidytext)

setwd("C:/Users/leale/Desktop/Thesis/Articles/Full corpus 2007_19_2")
load("quanteda_corpus.RData")
#View(quanteda_corpus)
#View(data)

stopwords('german')
#Prepare data
data <- dfm(quanteda_corpus, tolower = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE, 
            remove = c(stopwords('german'), "dass", "of", "foto", "end", "document", 
                       "and",  "the", "wurde", "wurden", "worden", "zeit", "welt", "zeitung", 
                       "online", "züricher", "spiegel", "dpa", "bildunterschrift" ), stem = TRUE)


dfm.trim <- dfm_trim(data, min_termfreq = 28, max_docfreq = 0.99, docfreq_type = "prop") # randomly selected, maybe other treshholds
#dfm.un.trim

####Show most frequent terms to know if some extremely frequent #####
length(data)
length(dfm.trim)
topfeatures(
  dfm.trim,
  n = 20,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = NULL
)
### eigentlich noch "patienten", etc. weg? td-idf?
colls <- textstat_collocations(quanteda_corpus,
                               min_count = 800) # minimum frequency
colls
#eigentlich keine auffallend häufigen bigrams

out <- convert(dfm.trim, to = 'stm')

documents = out$documents
vocab = out$vocab
meta = out$meta


######Deal with NAs ##########
nrow(meta) #1970
nrow(meta) - sum(is.na(meta$Date)) #1926

#find the missing values
keep <- !is.na(meta$Date)
#keep only observed values in meta and docs
meta <- meta[keep,]
docs <- documents[keep]
#rerun prepDocuments to deal with missing vocab
out <- prepDocuments(docs, vocab, meta, lower.thresh=2)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

View(quanteda_corpus)
Corpus <- quanteda_corpus[-c(76, 87, 92, 153, 187, 196, 209, 258, 290, 315, 402, 
                             426, 481, 491, 492, 514, 584, 646, 686, 698, 705, 
                             741, 799, 978, 1047, 1126, 1160, 1216, 1238, 1257, 1395, 1479, 
                             1572, 1641, 1701, 1836)]


#Transform Dates to numeric
out$meta$datum <- as.numeric(as.character(as.Date(out$meta$Date, format = "%Y/%m/%d"), format="%Y%m%d"))
out$meta$datum
str(out$meta$datum)

############################ Search K  ###########################

out$meta$datum <- as.Date(out$meta$Date, format = "%y/%m/%d")
kResult <- searchK(docs, vocab, K = c(10,15,20,25,30,35), init.type="Spectral", prevalence =~ Date, data=meta) 

# Plot diagnostic results
plot(kResult)
# Semantic coherence-exclusivity plot using function plot()
plot(kResult$results$semcoh, kResult$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
# Add labels to semantic coherence-exclusivity plot using function text()
text(kResult$results$semcoh, kResult$results$exclus, labels = paste("K", kResult$results$K), pos = 1)
# Semantic coherence-exclusivity table
knitr::kable(kResult$results)

#Run models with different number of K
model10 <- stm(docs, vocab, K=10, init.type = "Spectral", prevalence =~ datum, data=out$meta)
model15 <- stm(docs, vocab, K=15, init.type = "Spectral", prevalence =~ datum, data=out$meta)
model20<-stm(docs,vocab, K=20, init.type = "Spectral", prevalence =~ datum, data=out$meta)
model25<-stm(docs, vocab, K=25, init.type = "Spectral", prevalence =~ datum, data=out$meta)


#Plot semantic coherence and exclusivity for each model
M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(model10), semanticCoherence(model=model10, docs), "Mod10"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model15), semanticCoherence(model=model15, docs), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), semanticCoherence(model=model20, docs), "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model25), semanticCoherence(model=model25, docs), "Mod25"))


ModsExSem<-rbind(M15ExSem, M20ExSem, M25ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)


install.packages("ggplot2")
library(ggplot2)

plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

plotexcoer

#Calculate and plot mean and median SemCo and Excl. for different stm models
library(stm)
library(dplyr)
library(ggplot2)
library(quanteda)
library(stminsights)

diag <- get_diag(models = list(
  stm_10 = model15, 
  stm_20 = model20,
  stm_30 = model25), out)

diag %>% 
  ggplot(aes(x = coherence, y = exclusivity, color = statistic))  + 
  geom_text(aes(label = name), nudge_x = 2) + geom_point() + 
  labs(x = 'Semantic Coherence', y = 'Exclusivity')
