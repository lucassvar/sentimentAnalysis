# Load required libraries
library(tidytext)
library(janeaustenr)
library(dplyr)
library(textdata)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(wordcloud)

# Create tidy data from Jane Austen's books
tidy_data <- austen_books() %>%
	group_by(book) %>%
	mutate(linenumber = row_number(),
				 chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
	ungroup() %>%
	unnest_tokens(word, text)

# Extract positive sentiments using Bing lexicon
positive_senti <- get_sentiments("bing") %>%
	filter(sentiment == "positive")

# Analyze positive sentiments in the book "Emma"
tidy_data %>%
	filter(book == "Emma") %>%
	semi_join(positive_senti) %>%
	count(word, sort = TRUE)

# Get sentiment analysis for "Emma" and create a bar plot
bing <- get_sentiments("bing")

Emma_sentiment <- tidy_data %>%
	inner_join(bing, relationship = "many-to-many") %>%
	count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
	spread(sentiment, n, fill = 0) %>%
	mutate(sentiment = positive - negative)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
	geom_bar(stat = "identity", show.legend = TRUE) +
	facet_wrap(~book, ncol = 2, scales = "free_x")

# Analyze word count and sentiment score
counting_words <- tidy_data %>%
	inner_join(bing) %>%
	count(word, sentiment, sort = TRUE)
head(counting_words)

# Create a bar plot for words with count greater than 150
counting_words %>%
	filter(n > 150) %>%
	mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
	mutate(word = reorder(word, n)) %>%
	ggplot(aes(word, n, fill = sentiment)) +
	geom_col() +
	coord_flip() +
	labs(y = "Sentiment Score")

# Generate a word cloud for sentiment comparison
tidy_data %>%
	inner_join(bing) %>%
	count(word, sentiment, sort = TRUE) %>%
	acast(word ~ sentiment, value.var = "n", fill = 0) %>%
	comparison.cloud(colors = c("red", "dark green"),
									 max.words = 100)