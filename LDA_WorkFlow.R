####-----------------------------------------------------------------------------------------------------------------
##For the first round of LDA with original stopword
#
#DTM:
##First, we will turn the documents into DTM:
DebateDTM<-corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords(language="es",source="snowball")) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA (e.g. with 7 topics)
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(see=1234, alpha=1/1:10))
#
##Beta Matrix the top 20 words and their probability of belonging to each topic:
DebateLDA7_TopicWordProp<-tidy(Debate_LDA7, matrix="beta")
LDA7_Top20Words<-DebateLDA7_TopicWordProp %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#
#plotting the top 20 words per topic:
LDA7_Top20Words %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=factor(topic)))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()
#
#
#
#INSPECT THE RESULTS AND DO THE FOLLOWS:
##1. what words should be added to the list of stopword
##2. for not clear topic, we need to read the original text
###--------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------
##To add stopword:
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ser", RegStopWords)
#Now, we need to remove the New Stop Words to the dfm_remove()
DebateDTM<-corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
#
###--------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------
##To review the document
#(e.g. if topic 2 is not clear)
#We first ask for gamma matrix:
Debate_DocTopicProp<-tidy(Debate_LDA7, matrix="gamma")
#
#Then we filter topic 2 out:
Topic2<-Debate_DocTopicProp %>%
  filter(topic==2) %>%
  arrange(-gamma)
#Find the top documents and read it:
head(Topic2)
#
#
###--------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------
#
#REPEAT THE ABOVE STEPS UNTIL WE FIND THE FINAL SOLUTION FOR THE TOPIC



#Final solution, to make the probability of topic for each document and merge with the original data:
#
#The below step needs to be repeated for all the topics:
#
#first filter out topic 1:
#
FinalSolution_topic1<-Debate_DocTopicProp %>%
  filter(topic==1)
#
#Make the doc_id that will match with the original data doc_id:
FinalSolution_topic1<-FinalSolution_topic1 %>%
  mutate(doc_id=str_remove_all(FinalSolution_topic1$document,"\\.\\d{1,}"))
FinalSolution_topic1$doc_id<-as.numeric(FinalSolution_topic1$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic1_aggregatedGamma<-FinalSolution_topic1 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma))
#
##We can merge this "Topic1_aggregatedGamma" data back with the original Debate_Candidate_Election Data
#And we will have a column with topic 1 probability for each document:
Debate_Candidate_Election<-merge(Debate_Candidate_Election, Topic1_aggregatedGamma,
                                 by.x = "doc_id",
                                 by.y = "doc_id")
