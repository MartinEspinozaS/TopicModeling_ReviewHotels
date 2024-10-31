
# libraries
library("dplyr") # basic data manipulation
library("tm") # package for text mining package
library("stringr") # package for dealing with strings
library("RColorBrewer")# package to get special theme color
library("wordcloud") # package to create wordcloud
library("topicmodels") # package for topic modelling
library("ggplot2") # basic data visualization
library("LDAvis") # LDA specific visualization 
library("servr") # interactive support for LDA visualization
library("cld2") # for remove non-english reviews

# functions that will be used:

# this function eliminates reviews that are not in English (Parameters: dataset,
#   and a name of column of the dataset)
eliminate_nonEnglish_words <- function(dataset, nameColumn){
  # detect language
  language <- detect_language(nameColumn)
  language_df <- data.frame(language = language)
  dataset <- cbind(dataset, language_df)
  # filter no english reviews
  english_reviews <- subset(dataset, language == "en")
  non_english_reviews <- subset(dataset, !(language %in% c("en")))
  english_reviews$language <- NULL
  return(english_reviews)
}

# function that create a corpus (Parameters: columname of the dataset)
create_corpus <- function(dataset_nameColumn){
  reviews <- stringr::str_conv(dataset_nameColumn, "UTF-8")
  # create Corpus
  corpus_reviews <- Corpus(VectorSource(reviews))
  return(corpus_reviews)
}

# function that make lemmatization, removal of punctuation, numbers, stopword, 
#     and lowercase all tokens (Parameters: corpus)
clean_reviews <- function(corpus, non_significant_words){
  dtm_reviews <- DocumentTermMatrix(corpus, control = list(
    lemma=TRUE,removePunctuation = TRUE, removeNumbers = TRUE,
    stopwords = TRUE,tolower = TRUE))
  # Remove non-significant words
  dtm_reviews <- dtm_reviews[, !(colnames(dtm_reviews) %in% non_significant_words)]
  raw.sum=apply(dtm_reviews ,1,FUN=sum)
  dtm_reviews = dtm_reviews [raw.sum!=0,]
  return(dtm_reviews)
}

# Function that create DTM with term frequency (Parameter: dtmdocs)
create_DTM_termFrequency <- function(dtmdocs){
  # frequency table
  dtm.new <- as.matrix(dtmdocs)
  frequency <- colSums(dtm.new)
  frequency <- sort(frequency, decreasing=TRUE)
  doc_length <- rowSums(dtm.new)
  #frequency[1:10] 
  #return(frequency)
  return(list(frequency = frequency, dtm.new = dtm.new, doc_length = doc_length))
}

# function that create a wordcloud (Parameters: frequency table)
create_Wordcloud <- function(frequency){
  words <- names(frequency)# get back the word
  wordcloud(words[1:100], frequency[1:100], rot.per=0.15, random.order = FALSE,
            scale=c(4,0.5), random.color = FALSE, colors=brewer.pal(8,"Dark2"))
}

#function that determine the number of topics (Parameters: dtm.new, the initial
#   and the last number of range for topics)
Determining_Number_Topics <- function(dtm.new, initial, final){
  library("ldatuning")
  result <- FindTopicsNumber(dtm.new,
                             topics = seq(from = initial, to = final, by = 1),
                             metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
                             method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE)
  #make the plot
  FindTopicsNumber_plot(result)
}


# function that make the Topic Modelling LDA (Parameters: dtm and number of topics)
topic_modelling_LDA <- function(dtm,numberTopics){
  #Topic Modelling: Latent Dirichlet Allocation
  ldaOut <-LDA(dtm,numberTopics,method="Gibbs",control=list(iter=1000,seed=1000))
  phi <- posterior(ldaOut)$terms %>% as.matrix 
  #matrix, with each row containing the distribution over terms for a topic,
  theta <- posterior(ldaOut)$topics %>% as.matrix 
  #matrix, with each row containing the probability distribution over topics
  return(list(ldaOut = ldaOut, phi = phi, theta = theta))
}

# function that group by topics founded (Parameters: dataset, and ldaOutas)
grouping_by_topics <- function(dataset, ldaOut){
  #To get grouping documents by the topics that was found:
  # Which 'topic' is the review in (highest probability)
  ldaOut.topics <- data.frame(topics(ldaOut))
  ldaOut.topics$index <- as.numeric(row.names(ldaOut.topics))
  dataset$index <- as.numeric(row.names(dataset))
  #datawithtopic <- merge(dataset, ldaOut.topics, by='index',all.x=TRUE)
  #datawithtopic <- datawithtopic[order(datawithtopic$index), ]
  #return(datawithtopic)
}

# function that get probabilities (Parameters: ldaOut)
get_table_probabilities <- function(ldaOut){
  #To get grouping documents by the topics that was found for all topics
  # For each review, how closely it associate with each topics
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  topicProbabilities[0:10,1:5]
}

# function that visializing the LDA (Parameters: phi, theta, doc_length, and frequency )
Visualising_LDA <- function(phi, theta, doc_length, frequency){
  # Visualizing with LDAVis
  vocab <- colnames(phi) #vocab list in DTM
  # create the JSON object to feed the visualization in LDAvis:
  json_lda <- createJSON(phi = phi, theta = theta, vocab = vocab, 
                         doc.length = doc_length, term.frequency = frequency)
  serVis(json_lda, out.dir = 'vis', open.browser = TRUE)
}

get_Most_Influenced_topics <- function(ldaOut){
  topic_probabilities<- as.data.frame(posterior(ldaOut)$topics)
  mean_topic_prob <- colMeans(topic_probabilities)
  topic_data <- data.frame(Topic = 1:length(mean_topic_prob),
                           topic_mean = mean_topic_prob)
  topic_data <- topic_data[order(topic_data$topic_mean, decreasing = TRUE), ]
  return(topic_data)
}


### Classify Positive and Negative Reviews

# read the data
full_data_hotels <- read.csv("HotelsData.csv")
# eliminate non-english words
english_reviews <- eliminate_nonEnglish_words(full_data_hotels,
                                              full_data_hotels$Text.1)
# get my sample
set.seed(449)
data_reviews <- sample_n(english_reviews, 2000)

# split the reviews
data_reviews_pos <- subset(data_reviews, Review.score == 5 | Review.score == 4)
data_reviews_neg <- subset(data_reviews, Review.score == 1 | Review.score == 2)




non_significant_words <- c("hotel", "room", "london", "rooms", "one", "just", "really", "get", "hotels", "stay")

# create corpus Positive reviews
corpus_reviews_pos <- create_corpus(data_reviews_pos$Text.1)
# clean Positive reviews
dtm_reviews_pos <- clean_reviews(corpus_reviews_pos, non_significant_words)
# create DTM with Positive frecuency
DTM_results_pos <- create_DTM_termFrequency(dtm_reviews_pos)
frequency_pos <- DTM_results_pos$frequency
dtm.new_pos <- DTM_results_pos$dtm.new
doc_length_pos <- DTM_results_pos$doc_length
# Positive Wordcloud
create_Wordcloud(frequency_pos)
mtext("Figure 7: Words most used fo Satisfactory Reviews (Positive)", side = 1, line = 4.3)


# create corpus reviews
corpus_reviews_neg <- create_corpus(data_reviews_neg$Text.1)
# clean reviews
dtm_reviews_neg <- clean_reviews(corpus_reviews_neg, non_significant_words)
# create DTM with frecuency
DTM_results_neg <- create_DTM_termFrequency(dtm_reviews_neg)
frequency_neg <- DTM_results_neg$frequency
dtm.new_neg <- DTM_results_neg$dtm.new
doc_length_neg <- DTM_results_neg$doc_length
# Wordcloud
create_Wordcloud(frequency_neg)
mtext("Figure 8: Words most used for Dissatisfaction Reviews (Negative)", side = 1, line = 4.3)


### Topic Modelling

# determing the number of topics
Determining_Number_Topics(dtm.new_pos, 5, 20)
Determining_Number_Topics(dtm.new_neg, 5, 20)

# Modelling LDA Positive
number_of_topics_pos=19
Modelling_results_pos <-topic_modelling_LDA(dtm_reviews_pos,number_of_topics_pos)
ldaOut_pos <- Modelling_results_pos$ldaOut
phi_pos <- Modelling_results_pos$phi
theta_pos<- Modelling_results_pos$theta

# generate a list of  topics Which highest alpha 'term' is part of which topics
ldaOut.terms_pos <- as.matrix(terms(ldaOut_pos, 15))

# make the grouping fot Positive
grouping_by_topics(data_reviews_pos,ldaOut_pos)

# get the most influenced topics in order Positive
topics_influenced_pos <- get_Most_Influenced_topics(ldaOut_pos)
write.csv(ldaOut.terms_pos, "ldaOut_terms_pos.csv", row.names = TRUE)
#print(ldaOut.terms_pos)

# Visualizing with Positive LDAVis
Visualising_LDA(phi_pos, theta_pos, doc_length_pos, frequency_pos)



### Top factors that affect the satisfaction and dissatisfaction of the customers

# Modelling LDA Negative
number_of_topics_neg=14
Modelling_results_neg <-topic_modelling_LDA(dtm_reviews_neg,number_of_topics_neg)
ldaOut_neg <- Modelling_results_neg$ldaOut
phi_neg <- Modelling_results_neg$phi
theta_neg<- Modelling_results_neg$theta

# generate a list of  topics Which highest alpha 'term' is part of which topics
ldaOut.terms_neg <- as.matrix(terms(ldaOut_neg, 15))

# make the grouping fot Negative
grouping_by_topics(data_reviews_neg,ldaOut_neg)

# get the most influenced topics in order Negative
topics_influenced_neg <- get_Most_Influenced_topics(ldaOut_neg)
write.csv(ldaOut.terms_neg, "ldaOut_terms_neg.csv", row.names = TRUE)
#print(ldaOut.terms_neg)

# Visualizing with Negative LDAVis
Visualising_LDA(phi_neg, theta_neg, doc_length_neg, frequency_neg)




