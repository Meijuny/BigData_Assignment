###-------------------------------------------------------------------------------------------------------
####Solution 1:
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa",
                RegStopWords)

##Run LDA with 7 topics:
##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(seed=123, alpha=1/1:10))
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





###-------------------------------------------------------------------------------------------------------
####Solution 2: 
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa","presidente","presidenta",
                "nacional","pais",
                RegStopWords)

##Run LDA with 7 topics:
##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(seed=123, alpha=1/1:10))
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


####-------------------------------------------------------------------------------------------------
###BEST SO FAR Solution 3: 
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa", "presidente", "presidenta", "pais", "nacional",
                "menos","forma","parlamentario","mismo","general","entoces","legisladores","projecto","presente","manera","decir","ahora","cada","tipo",
                "partido","ademas","tribuna","parte","uso","reforma","comision","hase","cada","tener","vamos",
                "diputados","legisladoras","puede","buenas","claro","diputadas",
                RegStopWords)

##Run LDA with 7 topics:
##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(seed=123, alpha=1/1:10))
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



####-------------------------------------------------------------------------------------------------
##Solution 4: 
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa", "presidente", "presidenta", "pais", "nacional",
                "menos","forma","parlamentario","mismo","general","entoces","legisladores","projecto","presente","manera","decir","ahora","cada","tipo",
                "partido","ademas","tribuna","parte","uso","reforma","comision","hase","cada","tener","vamos",
                "diputados","legisladoras","puede","buenas","claro","diputadas",
                RegStopWords)

##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA6<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=6, control = list(seed=1667, alpha=1/1:10))
#
##Beta Matrix the top 20 words and their probability of belonging to each topic:
DebateLDA6_TopicWordProp<-tidy(Debate_LDA6, matrix="beta")
LDA6_Top20Words<-DebateLDA6_TopicWordProp %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#
#plotting the top 20 words per topic:
LDA6_Top20Words %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=factor(topic)))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()


####------------------------------------------------------------------------------------------------
##Best Alex so far:
#Solution 5
##We re-run the above, and we need to add more stop words as below:
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa", "presidente", "presidenta", "pais", "nacional",
                "menos","forma","parlamentario","mismo","general","entoces","legisladores","projecto","presente","manera","decir","ahora","cada","tipo",
                "partido","ademas","tribuna","parte","uso","reforma","comision","hase","cada","tener","vamos",
                "diputados","legisladoras","puede","buenas","claro","diputadas",
                "mayor",  "permiso", "caso","hacer",            RegStopWords)

##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA6<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=6, control = list(seed=1534, alpha=1/1:10))
#
##Beta Matrix the top 20 words and their probability of belonging to each topic:
DebateLDA6_TopicWordProp<-tidy(Debate_LDA6, matrix="beta")
LDA6_Top20Words<-DebateLDA6_TopicWordProp %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#
#plotting the top 20 words per topic:
LDA6_Top20Words %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=factor(topic)))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()


####---------------------------------------------------------------------------------------------------------
##BEST BEST MEIJUN:
#Solution 6:
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa", "presidente", "presidenta", "pais", "nacional",
                "menos","forma","parlamentario","mismo","general","entoces","legisladores","projecto","presente","manera","decir","ahora","cada","tipo",
                "partido","ademas","tribuna","parte","uso","reforma","comision","hase","cada","tener","vamos",
                "diputados","legisladoras","puede","buenas","claro","diputadas",
                "mayor",  "permiso", "caso","hacer",  "hace", "tema", "sentido",
                "casos", "solamente", "materia", "entonces", "hecho","quieren",
                RegStopWords)

##Run LDA with 7 topics:
##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(seed=123, alpha=1/1:10))
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



####------------------------------------------------------------------------------------------------
##BEST BEST Alex
##Solution 7:
RegStopWords<-stopwords(language = "es", source = "snowball")
NewStopWords<-c("ley", "ano", "articulo","toda", "hoy",
                "fraccion","gracias","muchas","muchos","grupo",
                "mas", "cuanto", "favor", "sino","asi","ver",
                "solo", "estan", "aqui", "van", "ello", "quiero",
                "dictamen","si","dias","venia","debe","pues","ser",
                "companeros","companeras","diputada","diputado","tan",
                "camara","anos","dia",
                "traves","va","diputados","mexico","senor","gran","aun",
                "ustedes", "vez", "tambien","iniciativa", "presidente", "presidenta", "pais", "nacional",
                "menos","forma","parlamentario","mismo","general","entoces","legisladores","projecto","presente","manera","decir","ahora","cada","tipo",
                "partido","ademas","tribuna","parte","uso","reforma","comision","hase","cada","tener","vamos",
                "diputados","legisladoras","puede","buenas","claro","diputadas",
                "mayor",  "permiso", "caso","hacer", "hace","tema","sentido","casos","solamente","materia","entonces",
                "hecho","quieren",
                "leyes", "existe", "incluso", 
                "ahi", "marco",
                RegStopWords)

##First, we will turn the documents into DTM:
DebateDTM<- corpus(Debate_Candidate_Election) %>%
  corpus_reshape(to="paragraph") %>%
  tokens(remove_punct=TRUE) %>%
  dfm() %>%
  dfm_remove(NewStopWords) %>%
  dfm_trim(min_docfreq = 0.005, max_docfreq = 0.75, docfreq_type = "prop")
#
##Run the very first LDA
Debate_LDA7<-DebateDTM %>%
  convert(to="topicmodels") %>%
  LDA(k=7, control = list(seed=1694, alpha=1/1:10))
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




####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
##Solution 6: Meijun BEST BEST:
#
#First ask for the per document per prop matrix:
LDA7_DocTopicProp<-tidy(Debate_LDA7, matrix="gamma")
#
##first filter out topic 1:
#
Topic1<-LDA7_DocTopicProp %>%
  filter(topic==1)
#
#Make the doc_id that will match with the original data doc_id:
Topic1<-Topic1 %>%
  mutate(doc_id=str_remove_all(Topic1$document,"\\.\\d{1,}"))
Topic1$doc_id<-as.numeric(Topic1$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic1_aggregatedGamma<-Topic1 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 2:
#
Topic2<-LDA7_DocTopicProp %>%
  filter(topic==2)
#
#Make the doc_id that will match with the original data doc_id:
Topic2<-Topic2 %>%
  mutate(doc_id=str_remove_all(Topic2$document,"\\.\\d{1,}"))
Topic2$doc_id<-as.numeric(Topic2$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic2_aggregatedGamma<-Topic2 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 3:
#
Topic3<-LDA7_DocTopicProp %>%
  filter(topic==3)
#
#Make the doc_id that will match with the original data doc_id:
Topic3<-Topic3 %>%
  mutate(doc_id=str_remove_all(Topic3$document,"\\.\\d{1,}"))
Topic3$doc_id<-as.numeric(Topic3$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic3_aggregatedGamma<-Topic3 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 4:
#
Topic4<-LDA7_DocTopicProp %>%
  filter(topic==4)
#
#Make the doc_id that will match with the original data doc_id:
Topic4<-Topic4 %>%
  mutate(doc_id=str_remove_all(Topic4$document,"\\.\\d{1,}"))
Topic4$doc_id<-as.numeric(Topic4$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic4_aggregatedGamma<-Topic4 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 5:
#
Topic5<-LDA7_DocTopicProp %>%
  filter(topic==5)
#
#Make the doc_id that will match with the original data doc_id:
Topic5<-Topic5 %>%
  mutate(doc_id=str_remove_all(Topic5$document,"\\.\\d{1,}"))
Topic5$doc_id<-as.numeric(Topic5$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic5_aggregatedGamma<-Topic5 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic6<-LDA7_DocTopicProp %>%
  filter(topic==6)
#
#Make the doc_id that will match with the original data doc_id:
Topic6<-Topic6 %>%
  mutate(doc_id=str_remove_all(Topic6$document,"\\.\\d{1,}"))
Topic6$doc_id<-as.numeric(Topic6$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic6_aggregatedGamma<-Topic6 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic7<-LDA7_DocTopicProp %>%
  filter(topic==7)
#
#Make the doc_id that will match with the original data doc_id:
Topic7<-Topic7%>%
  mutate(doc_id=str_remove_all(Topic7$document,"\\.\\d{1,}"))
Topic7$doc_id<-as.numeric(Topic7$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic7_aggregatedGamma<-Topic7 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)


####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
##Solution 7: Alex BEST BEST:
#First ask for the per document per prop matrix:
LDA7_DocTopicProp<-tidy(Debate_LDA7, matrix="gamma")
#
##first filter out topic 1:
#
Topic1<-LDA7_DocTopicProp %>%
  filter(topic==1)
#
#Make the doc_id that will match with the original data doc_id:
Topic1<-Topic1 %>%
  mutate(doc_id=str_remove_all(Topic1$document,"\\.\\d{1,}"))
Topic1$doc_id<-as.numeric(Topic1$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic1_aggregatedGamma<-Topic1 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 2:
#
Topic2<-LDA7_DocTopicProp %>%
  filter(topic==2)
#
#Make the doc_id that will match with the original data doc_id:
Topic2<-Topic2 %>%
  mutate(doc_id=str_remove_all(Topic2$document,"\\.\\d{1,}"))
Topic2$doc_id<-as.numeric(Topic2$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic2_aggregatedGamma<-Topic2 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 3:
#
Topic3<-LDA7_DocTopicProp %>%
  filter(topic==3)
#
#Make the doc_id that will match with the original data doc_id:
Topic3<-Topic3 %>%
  mutate(doc_id=str_remove_all(Topic3$document,"\\.\\d{1,}"))
Topic3$doc_id<-as.numeric(Topic3$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic3_aggregatedGamma<-Topic3 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 4:
#
Topic4<-LDA7_DocTopicProp %>%
  filter(topic==4)
#
#Make the doc_id that will match with the original data doc_id:
Topic4<-Topic4 %>%
  mutate(doc_id=str_remove_all(Topic4$document,"\\.\\d{1,}"))
Topic4$doc_id<-as.numeric(Topic4$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic4_aggregatedGamma<-Topic4 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 5:
#
Topic5<-LDA7_DocTopicProp %>%
  filter(topic==5)
#
#Make the doc_id that will match with the original data doc_id:
Topic5<-Topic5 %>%
  mutate(doc_id=str_remove_all(Topic5$document,"\\.\\d{1,}"))
Topic5$doc_id<-as.numeric(Topic5$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic5_aggregatedGamma<-Topic5 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic6<-LDA7_DocTopicProp %>%
  filter(topic==6)
#
#Make the doc_id that will match with the original data doc_id:
Topic6<-Topic6 %>%
  mutate(doc_id=str_remove_all(Topic6$document,"\\.\\d{1,}"))
Topic6$doc_id<-as.numeric(Topic6$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic6_aggregatedGamma<-Topic6 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic7<-LDA7_DocTopicProp %>%
  filter(topic==7)
#
#Make the doc_id that will match with the original data doc_id:
Topic7<-Topic7%>%
  mutate(doc_id=str_remove_all(Topic7$document,"\\.\\d{1,}"))
Topic7$doc_id<-as.numeric(Topic7$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic7_aggregatedGamma<-Topic7 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)

####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------
##Solution 5: Alex BEST BEST:
#First ask for the per document per prop matrix:
LDA6_DocTopicProp<-tidy(Debate_LDA6, matrix="gamma")
#
##first filter out topic 1:
#
Topic1<-LDA6_DocTopicProp %>%
  filter(topic==1)
#
#Make the doc_id that will match with the original data doc_id:
Topic1<-Topic1 %>%
  mutate(doc_id=str_remove_all(Topic1$document,"\\.\\d{1,}"))
Topic1$doc_id<-as.numeric(Topic1$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic1_aggregatedGamma<-Topic1 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 2:
#
Topic2<-LDA6_DocTopicProp %>%
  filter(topic==2)
#
#Make the doc_id that will match with the original data doc_id:
Topic2<-Topic2 %>%
  mutate(doc_id=str_remove_all(Topic2$document,"\\.\\d{1,}"))
Topic2$doc_id<-as.numeric(Topic2$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic2_aggregatedGamma<-Topic2 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 3:
#
Topic3<-LDA6_DocTopicProp %>%
  filter(topic==3)
#
#Make the doc_id that will match with the original data doc_id:
Topic3<-Topic3 %>%
  mutate(doc_id=str_remove_all(Topic3$document,"\\.\\d{1,}"))
Topic3$doc_id<-as.numeric(Topic3$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic3_aggregatedGamma<-Topic3 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 4:
#
Topic4<-LDA6_DocTopicProp %>%
  filter(topic==4)
#
#Make the doc_id that will match with the original data doc_id:
Topic4<-Topic4 %>%
  mutate(doc_id=str_remove_all(Topic4$document,"\\.\\d{1,}"))
Topic4$doc_id<-as.numeric(Topic4$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic4_aggregatedGamma<-Topic4 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 5:
#
Topic5<-LDA7_DocTopicProp %>%
  filter(topic==5)
#
#Make the doc_id that will match with the original data doc_id:
Topic5<-Topic5 %>%
  mutate(doc_id=str_remove_all(Topic5$document,"\\.\\d{1,}"))
Topic5$doc_id<-as.numeric(Topic5$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic5_aggregatedGamma<-Topic5 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic6<-LDA7_DocTopicProp %>%
  filter(topic==6)
#
#Make the doc_id that will match with the original data doc_id:
Topic6<-Topic6 %>%
  mutate(doc_id=str_remove_all(Topic6$document,"\\.\\d{1,}"))
Topic6$doc_id<-as.numeric(Topic6$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic6_aggregatedGamma<-Topic6 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)
####----------------------------------------------------------------------------------------------
#
#Then filter out topic 6:
#
Topic7<-LDA7_DocTopicProp %>%
  filter(topic==7)
#
#Make the doc_id that will match with the original data doc_id:
Topic7<-Topic7%>%
  mutate(doc_id=str_remove_all(Topic7$document,"\\.\\d{1,}"))
Topic7$doc_id<-as.numeric(Topic7$doc_id)
#
#Group them by doc_id and calculate the meanGamma value:
Topic7_aggregatedGamma<-Topic7 %>%
  group_by(doc_id) %>%
  summarise(MeanProp=mean(gamma)) %>%
  arrange(-MeanProp)