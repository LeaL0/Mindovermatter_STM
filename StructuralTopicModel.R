##################################################################
###         Preparation for STM: text pre-processing           ###
##################################################################

install.packages("quanteda")
install.packages("stminsights")
library(stm)
library(quanteda)
library(plyr)
library(ggplot2)
library(tidytext)

setwd("C:/Users/XXXXXXXX/XXXXXXXX/Full corpus 2007_19_2")
load("quanteda_corpus.RData")
View(quanteda_corpus)


? stopwords('german')
#Prepare data
data <- dfm(quanteda_corpus, tolower = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE, 
            remove = c(stopwords('german'), "dass", "of", "foto", "end", "document", 
                       "and",  "the", "wurde", "wurden", "worden", "zeit", "welt", "zeitung", 
                       "online", "züricher", "spiegel", "dpa", "bildunterschrift" ), stem = TRUE)


dfm.trim <- dfm_trim(data, min_termfreq = 28, max_docfreq = 0.5, docfreq_type = "prop") # randomly selected, maybe other treshholds


#Show most frequent terms
length(data)
length(dfm.trim)
topfeatures(
  dfm.trim,
  n = 100,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = NULL
)

out <- convert(dfm.trim, to = 'stm')

documents = out$documents
vocab = out$vocab
meta = out$meta


#Deal with missing publication dates
nrow(meta) 
nrow(meta) - sum(is.na(meta$Date)) 

#Find the missing values
keep <- !is.na(meta$Date)
#Keep only observed values in meta and docs
meta <- meta[keep,]
docs <- documents[keep]
#Rerun prepDocuments to deal with missing vocab
out <- prepDocuments(docs, vocab, meta, lower.thresh=2)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

View(quanteda_corpus)
Corpus <- quanteda_corpus[-c(76, 87, 92, 153, 187, 196, 209, 258, 290, 315, 402, 
                             426, 481, 491, 492, 514, 584, 646, 686, 698, 705, 
                             741, 799, 978, 1047, 1126, 1160, 1216, 1238, 1257, 1395, 1479, 
                             1572, 1641, 1701, 1836)]

#Number of articles from each newspaper
library(plyr)
y = count(meta$Newspaper)
y
#Transform Dates to numeric
out$meta$datum <- as.numeric(as.character(as.Date(out$meta$Date, format = "%Y/%m/%d"), format="%Y%m%d"))
out$meta$datum
str(out$meta$datum)

##################################################################
###                  Structural topic model                    ###
##################################################################

n.topics <- 20
mod <- stm(docs, vocab, K= n.topics, init.type="Spectral", prevalence =~ datum, data=out$meta)
modell.stm.labels <- labelTopics(mod, 1:n.topics)
par(fig=c(0.05,0.95,0.05,0.95))
mod
#Distribution and top 5 words per topic:
plot.STM(mod, "summary", n=5)
#Table with topic proportions
td_gamma <- tidy(mod, matrix = "gamma",
                 document_names = rownames(docs))
td_gamma

td_beta <- tidy(mod)
td_beta

#Overview over the topics, prevalences and top 7 terms
install.packages("purrr")
library(purrr)
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
install.packages("tidytext")
library(tidytext)
library(tidyverse)
library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))


gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))
####################################################################
#####             Explore STM results                          #####
####################################################################

#Show STM results in data frame:
as.data.frame(t(labelTopics(mod, n = 10)$prob))

#Topic shares on the whole corpus
plot(mod, type = "summary", text.cex = 0.5, main = "Topic shares on the corpus as a whole", xlab = "estimated share of topics")

#Topic shares within the documents 
plot(mod, type = "hist", topics = sample(1:15, size = 9), main = "histogram of the topic shares within the documents")

#Topic terms
plot(mod, type = "labels", topics = c(6, 11, 10, 13), main = "Topic terms")

#Plot topic contrast
plot(mod, type = "perspectives", topics = c(4,15), main = "Topic contrasts")
plot.STM(mod, type = 'perspective', topics = c(4,5))

#Plot nicely
#Beta refers to the word probabilities per topic
AP_topics <- tidy(mod)
ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic")


#Beta values for ONE topic
betaT1<- AP_topics %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 16")

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 13")#plot word probabilities higher than 0.003 for topic 1
betaplotT1

#Complete list of top 10 words per topics, Highest Prob, FREX, lift
labelTopics(mod, topics=c(1:20), n=10)
#Top 10 FREX words per topics 5-6-9:
plot.STM(mod, type = 'labels', n = 8, text.cex = 0.8, 
         width = 100, topics = 1:15, labeltype = 'frex')

#Find passages of text that are assigned to topic
thoughts7 <- findThoughts(mod, texts = Corpus, 
             n = 25, topics = 7)

print(thoughts7["docs"], max_ndoc = 25, max_nchar = 300)

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts7, width = 30, maxwidth=120, text.cex=1.25, main = "Topic 1")
print(quanteda_corpus[276])

#Alternative: wider excerpt of documents:
thoughts1 <- findThoughts(mod, 
                         texts = Corpus, # unprocessed documents
                         topics = 1,  n = 15) 
thoughts1
plotQuote(thoughts1$docs[[1]][1], 
          width = 80, text.cex = 0.75) 

#Create sup-corpus with documents from one topic
df_Corpus<-data.frame(Corpus)
Corpus20 <- df_Corpus[c("209", "82", "1465", "1464", "118", "1880", "1306", "1022", "34", "285", "1740", "58", "1694", "1895", "594"),]
head(Corpus20)

write.table(Corpus20, "Corpus20.txt", sep=",")


###########################################################
##                  Estimate Effekts                    ##
##########################################################

#Estimates effects of the covariate "publication date" on the proportion of each
#document about a topic in an STM model
modell.stm.effekt <- estimateEffect(1:n.topics ~ s(datum), mod, meta = out$meta)

#Summary of regression on topic 1-20
summary(modell.stm.effekt, topics=c(1:20), nsim=1000)

save(out, mod, modell.stm.effekt, file = "stm_20_effects.RData")

#Plot estimated effects, i.e. topic prevalence over time
par(mfrow=c(2,2))
for (i in 1:n.topics)
{
  plot(modell.stm.effekt, "datum", method = "continuous", topics = i, 
       main = paste0(modell.stm.labels$prob[i,1:3], collapse = ", "), ylab = "", printlegend = F)
}


#Plot individual topic
plot(modell.stm.effekt, "datum", method = "continuous", topics = 15, 
     main = paste0(modell.stm.labels$prob[15,1:3], collapse = ", "), ylab = "", printlegend = F)

######################################
## Topic Correlation Matrix        ##
######################################

#Calculate correlation matrix
topicCorr(mod, method = c("simple", "huge"), cutoff = 0.01,
          verbose = TRUE)

corr <- round(topicCorr(mod)$cor, 2)
corr



#Plot correlation matrix
library(DT)
datatable(upper)

#Correlation matrix with colours indicating the strength of correlation
install.packages("GGally")
library(GGally)
ggcorr(corr, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')

#Heatmap correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr,  hc.order = TRUE, type = "lower",
           outline.col = "white")

#Correlation network of topics
library(stminsights)
library(ggraph)
library(igraph)
library(extrafont)
stm_corrs <- get_network(model = mod,
                         method = 'simple', # correlation criterion,
                         cutoff = 0.05, # minimum correlation
                         labels = paste(c("RCTs", "Household remedies", "Events", "Pain treatments", "Body products", "'Placebo-measures'",
                                          "Neuropsychology", "Research & studies", "Politics & Econ.", "Families", "Mental health", "12", 
                                          "Other experiments", "Vaccines", "Placebo research", "Women", "17", "Homeopathy", "Belief & spirituality", "Medications")), #topic.names=c("My Label:", "My Label2:", "Another Label:")) 
                         cutiso = FALSE) # isolated nodes
network <- ggraph(stm_corrs, layout = 'fr') + geom_edge_link(
              aes(edge_width = weight), label_colour = '#fc8d62', 
              edge_colour = '#377eb8') + geom_node_point(size = 4, colour = 'black')  +
              geom_node_label(aes(label = name, size = props),
                  colour = 'black',  repel = TRUE, alpha = 0.85) +
              scale_size(range = c(2, 10), labels = scales::percent) +  
              labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') + theme_graph()


#Calculating degree centrality, i.e. how many connections the nodes/topics have to others
library(igraph)
degree.cent <- centr_degree(stm_corrs, mode = "all")
degree.cent$res

#######################################################################
