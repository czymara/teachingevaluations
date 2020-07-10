

### create worldcloude of open evaluation items

evaluations <- read.delim("C:/Users/czymara.local/PowerFolders/teaching/teachingevaluations/in/evaluations.txt", fileEncoding="UTF-8")

evaluations <- as.character(evaluations[,1])

library("quanteda", "tidytext", "dplyr")


toks <- tokens(corpus(evaluations), remove_punct = T,
               remove_numbers = T,
               remove_symbols = T,
               remove_separators = T,
               remove_hyphens = T)

toks <-  tokens_remove(toks, c(stopwords("german"),
                               "dass", "h", "wäre", "wären", "z.b", "mal",
                               "bzw", "pro", "eher", "h", "denen",
                               "dafür", "innen", "ja", "wurde"), case_insensitive = TRUE, padding = FALSE)
toks <-  tokens_remove(toks, stopwords(), case_insensitive = TRUE, padding = FALSE)

DFM <- dfm(toks)


### wordcloud

win.metafile("C:/Users/czymara.local/PowerFolders/teaching/teachingevaluations/out/eval_wordcloud.wmf")
# dev.copy(png,"C:/Users/czymara.local/PowerFolders/teaching/teachingevaluations/out/eval_wordcloud.png")
textplot_wordcloud(DFM,
                  # min_size = 1.5,
                 #  min_count = 3,
                   color = "black")
dev.off()


## sentiment analysis

# create dictionary (see https://www.inwt-statistics.de/blog-artikel-lesen/text-mining-part-3-sentiment-analyse.html)
SentiWS <- c(
  readLines("C:/Users/czymara.local/Google Drive/job/zonstiges/spielRei/twitter/SentiWS_v2.0_Positive.txt",
            encoding = "UTF-8"),
  readLines("C:/Users/czymara.local/Google Drive/job/zonstiges/spielRei/twitter/SentiWS_v2.0_Negative.txt",
            encoding = "UTF-8")
) %>% lapply(function(x) {
  # Extrahieren der einzelnen Spalten
  res <- strsplit(x, "\t", fixed = TRUE)[[1]]
  return(data.frame(word = res[1], value = res[2],
                    stringsAsFactors = FALSE))
}) %>%
  bind_rows %>%
  mutate(word = gsub("\\|.*", "", word) %>% tolower,
         value = as.numeric(value)) %>%
  # manche Wörter kommen doppelt vor, hier nehmen wir den mittleren Wert
  group_by(word) %>% summarise(value = mean(value)) %>% ungroup



DFM %>%
  inner_join(SentiWS) %>%
  count(word, sort = TRUE)

words <- NULL
words$word <- colnames(DFM)

# mean sentiment
sentTwt <- left_join(words,
                     SentiWS,
                     by = "word") %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

mean(sentTwt$value) # negative

