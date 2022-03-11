#with readxl library I can read 'myspar-lidlplus_reviewk.xlsx' what contains the data.
library(readxl)

app_reviews <- readxl::read_xlsx("myspar-lidlplus_reviewk.xlsx")
app_reviews

# There are columns which are unuseful for the analyse. 
app_reviews$reviewId<- NULL
app_reviews$userImage <- NULL
app_reviews$userName <- NULL
app_reviews$reviewCreatedVersion <- NULL
app_reviews$replyContent <- NULL
app_reviews$repliedAt <- NULL
app_reviews$sortOrder <- NULL
app_reviews$thumbsUpCount <- NULL

# 'at' column (date) contains the hh:mm:ss part of time, removing that:
app_reviews$at <- strtrim(app_reviews$at, 10)

# Score column containt charachter variables, change it to numeric
as.numeric(app_reviews$score)

#load ggplot2 and tidyverse
library(ggplot2)
library(tidyverse)

#this plot shows that Lidl plus application has reviews from 2019, and show the distribution of the scores by date.
app_reviews %>% dplyr::group_by(appId, at) %>% dplyr::summarise(t_m = mean(score)) %>% ggplot(aes(as.Date(at), t_m)) +  geom_point() + facet_wrap(~ appId)

# I'll work with STM
library(stm)


processed <- textProcessor(app_reviews$content, metadata = app_reviews, lowercase = TRUE,
                           removestopwords = TRUE,
                           removenumbers = TRUE,
                           removepunctuation = TRUE,
                           ucp = TRUE,
                           stem = FALSE,
                           wordLengths = c(3, Inf),
                           sparselevel = 1,
                           language = "hun",
                           verbose = TRUE,)
processed

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
out

topics_n_app <- searchK(out$documents, out$vocab, 3:15)

topics_n_app_df <- as.data.frame(topics_n_app$results)

#This plot shows that 6 topics maybe good for us
topics_n_app_df %>% ggplot(aes(as.numeric(semcoh), as.numeric(exclus), label = K)) + geom_point(size = 3, stroke = 0, shape = 16) + geom_text(aes(label=K),hjust=0, vjust=0, position = position_nudge(y = -0.07)) + xlab("Szemantikus koherencia") + ylab("Exkluzivitás") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#with lubridate package you can deal with dates
library(lubridate)

out$meta$at <- ymd(out$meta$at)
out$meta$at <- as.numeric(out$meta$at)

app_PrevFit <- stm(documents = out$documents, vocab = out$vocab,
                    K = 6, prevalence =~ score + s(at) + appId,
                    max.em.its = 75, data = out$meta,
                    init.type = "Spectral")

summary(app_PrevFit)

prep <- estimateEffect(1:6 ~ score + s(at) + appId, app_PrevFit,
                       meta = out$meta, uncertainty = "Global")

prep


#Here is some plot to check the outcomes. 


plot(kermark_PrevFit, type = "summary")

plot(prep, "at", model=app_PrevFit,
     method="continuous")

plot(prep, "score", model=app_PrevFit,
     method="continuous")

plot(prep, "appId", model=app_PrevFit,
     method="pointestimate")


