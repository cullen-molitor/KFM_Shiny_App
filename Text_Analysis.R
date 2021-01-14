


library(tidyverse)
library(tidytext)
library(pdftools)
library(wordcloud)
library(topicmodels)
library(textdata)
library(igraph)
library(ggraph)

data(stop_words)

text_complete <- pdftools::pdf_text("App/www/Annual_Reports/1982-1989.pdf") %>%
  readr::read_lines() %>%
  dplyr::tibble() %>%
  dplyr::rename('text' = '.') %>%
  tidytext::unnest_tokens(word, text, token = "ngrams") %>% 
  dplyr::anti_join(stop_words) %>% 
  dplyr::filter(word != "x x x") %>%
  dplyr::mutate(word = base::gsub("\\b\\d+\\b", NA, word),
                word = base::gsub("x x", "", word),
                word = base::gsub("cases n", "", word),
                Year = "1982-1989") %>% 
  dplyr::filter(word != " s", word != "s s s", word != "s s a", word != "x s s", word != "max size mm", word != "min size mm") %>%
  tidyr::drop_na() %>%
  dplyr::group_by(Year) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::arrange(desc(n)) %>% 
  utils::head(500)


for (yr in 1990:2013) {
  text <- 
    pdftools::pdf_text(
      glue::glue("App/www/Annual_Reports/{yr}.pdf")) %>%
    readr::read_lines() %>%
    dplyr::tibble() %>%
    dplyr::rename('text' = '.') %>%
    tidytext::unnest_tokens(word, text, token = "ngrams") %>%
    dplyr::anti_join(stop_words) %>% 
    dplyr::filter(word != "x x x") %>%
    dplyr::mutate(word = base::gsub("\\b\\d+\\b", NA, word),
                  word = base::gsub("x x", "", word),
                  word = base::gsub("cases n", "", word),
                  Year = as.character(yr)) %>% 
    tidyr::drop_na() %>%
    dplyr::filter(word != " s", word != "s s s", word != "s s a", word != "x s s", word != "max size mm", word != "min size mm") %>%
    dplyr::group_by(Year) %>% 
    dplyr::count(word, sort = TRUE) %>% 
    dplyr::arrange(desc(n)) %>% 
    utils::head(500)
  text_complete <- rbind(text_complete, text)
}


text_all_summary <- text_complete %>%
  dplyr::group_by(word) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(Year = "All Years") %>% 
  base::rbind(text_complete) %>% 
  # dplyr::group_by(Year) %>% 
  # dplyr::mutate(n = round(scales::rescale(n, c(1, 5000))), 0) %>% 
  # dplyr::ungroup() %>% 
  readr::write_csv("App/Tidy_Data/Text.csv")

# text_all_sentiment <- text_complete %>%
#   inner_join(get_sentiments("afinn")) %>%
#     filter(n > 1)
# 
# ggplot(text_all_sentiment, aes(word, value, fill = Year)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~Year, ncol = 10, scales = "free_x")

# text_all_summary %>%
#   filter(n > 100) %>%
#   arrange(dplyr::desc(n)) %>%
#   dplyr::mutate(word = reorder(word, n)) %>%
#   ggplot(aes(n, word)) +
#   geom_col() +
#   labs(y = NULL) 

wordcloud::wordcloud(
  words = text_all_summary$word, 
  freq = text_all_summary$n, min.freq = 1,
  max.words = 250, random.order = FALSE, rot.per = 0, 
  colors = brewer.pal(8, "Dark2"))





# tidytext::cast_dfm()
# 
# ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))





