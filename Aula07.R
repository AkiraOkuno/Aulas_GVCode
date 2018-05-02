library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(wordcloud)

austen_bybook <- austen_books() %>% group_by(book)
head(austen_bybook)
austen_lines <- mutate(austen_bybook, linenumber = row_number())
austen_chapters <- mutate(austen_lines, chapter = cumsum(str_detect(text,
                                                                    regex("^chapter [\\divxlc]", 
                                                                          ignore_case = TRUE))))

austen_ungrouped <- ungroup(austen_chapters)
tidy_books <- unnest_tokens(austen_ungrouped,word,text)
data(stop_words)
tidy_books <- anti_join(tidy_books,stop_words)
tidy_sort <- count(tidy_books, word, sort=TRUE)

filter(tidy_sort,n>540) -> tidy_filter
mutate(tidy_filter,word=reorder(word,n)) -> tidy_mutate

ggplot(tidy_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(tidy_mutate$word,tidy_mutate$n,
          max.words = 100,
          rot.per = FALSE,colors= c("#973232", "#1E5B58", "#6A8D2F", "#287928"))

library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230,159))
tidy_wells <- hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words)
wells_sort <- count(tidy_wells, word, sort=TRUE)
filter(wells_sort,n>170) -> wells_filter
mutate(wells_filter,word=reorder(word,n)) -> wells_mutate

ggplot(wells_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(wells_mutate$word,wells_mutate$n,
          max.words = 100,
          rot.per = FALSE,colors= c("#AB329C", "#195B5B", "#6D8D2F", "#287928"))
