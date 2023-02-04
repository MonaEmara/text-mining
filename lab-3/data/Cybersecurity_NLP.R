ts_cyber_data <- read_csv("Cybersecurity Survey_copy.csv", 
                          col_types = cols(Timestamp = col_character()
                          )
)
#{r tokenize-forums}
forums_tidy1 <- ts_cyber_data %>%
  unnest_tokens(output = word, input = challenges_NetsBlox) %>%
  anti_join(stop_words, by = "word")
forums_tidy1
#{r count-words}
forums_tidy1 %>%
  count(word, sort = TRUE)
#{r find-quotes, echo=FALSE}
forum_quotes1 <- ts_cyber_data %>%
  select(challenges_NetsBlox) %>% 
  filter(grepl('code', challenges_NetsBlox))

sample_n(forum_quotes1,7)
#{r cast-dtm}
forums_dtm1 <- forums_tidy1 %>%
  count(Timestamp, word) %>%
  cast_dtm(Timestamp, word, n)
#{r class-dtm, echo=FALSE}
class(forums_dtm1)

forums_dtm1
#{r textProcessor}
temp1 <- textProcessor(ts_cyber_data$challenges_NetsBlox, 
                      metadata = ts_cyber_data,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=NULL)
#{r stm-inputs}
meta1 <- temp1$meta
vocab1 <- temp1$vocab
docs1 <- temp1$documents
#{r wordStem}
stemmed_forums1 <- ts_cyber_data %>%
  unnest_tokens(output = word, input = challenges_NetsBlox) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_forums1
#{r stem-practice, eval=FALSE}
stemmed_dtm1 <- ts_cyber_data %>%
  unnest_tokens(output = word, input = challenges_NetsBlox) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  ______() %>%
  ______() %>%
  
  stemmed_dtm1
#{r stem-counts, echo=FALSE, message=FALSE}
stem_counts1 <- ts_cyber_data %>%
  unnest_tokens(output = word, input = challenges_NetsBlox) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)

stem_counts1
#{r n-distinct}
n_distinct(ts_cyber_data$grade)
#{r LDA}
n_distinct(ts_cyber_data$grade)

forums_lda1 <- LDA(forums_dtm1, 
                  k = 5, 
                  control = list(seed = 588)
)

forums_lda1
#{r stm-docs}
docs1 <- temp1$documents 
meta1 <- temp1$meta 
vocab1 <- temp1$vocab 
#{r stm}
forums_stm <- stm(documents=docs, 
                  data=meta,
                  vocab=vocab, 
                  prevalence =~ course_id + forum_id,
                  K=5,
                  max.em.its=25,
                  verbose = FALSE)

forums_stm
