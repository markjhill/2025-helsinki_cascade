library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(scales)

# Basic TSV loading
df <- read_tsv("C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv")

# # Alternative loading method
# df <- read_delim("C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv", 
#                  delim = "\t")

# Royal Society Corpus - Exploratory Data Analysis
# Simple, flexible code snippets for actual exploration

# ==========================================
# EXPLORATION TIPS
# ==========================================

# Always try:
# 1. Different time windows: df %>% filter(year >= 1850 & year <= 1950)
# 2. Different subsets: df %>% filter(type == 'article')
# 3. Different groupings: group_by(decade, type) vs group_by(year)
# 4. Different plot types: geom_point(), geom_bar(), geom_histogram(), geom_boxplot()
# 5. Different statistics: mean(), median(), sd(), quantile()

# Useful patterns:
# df %>% filter(year > 1900, pages < 20)  # Clean query syntax
# df %>% slice_max(tokens, n = 10)        # Longest documents
# df %>% slice_min(pages, n = 10)         # Shortest documents
# df %>% sample_n(100)                    # Random sample for quick checks

# ==========================================
# BASIC EXPLORATION - Start here
# ==========================================

# Quick overview
dim(df)
colnames(df)
str(df)
summary(df)

# What's actually in this data?
head(df, 3)
df %>% sample_n(5)  # Random sample often more revealing than head()

# ==========================================
# TEMPORAL PATTERNS - Play with different groupings
# ==========================================

# Publications by year - try different periods
df %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line() + 
  labs(title = "Publications by Year")

df %>% 
  count(decade) %>% 
  ggplot(aes(x = factor(decade), y = n)) + 
  geom_col() + 
  labs(title = "Publications by Decade", x = "Decade")

df %>% 
  count(century) %>% 
  ggplot(aes(x = factor(century), y = n)) + 
  geom_col() + 
  labs(title = "Publications by Century", x = "Century")

# Try different time windows
df %>% 
  count(period) %>% 
  ggplot(aes(x = factor(period), y = n)) + 
  geom_col() + 
  labs(title = "Publications by Period (50-year)")

df %>% 
  filter(year > 1800) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line() + 
  labs(title = "Publications by Year (Modern Period)")

# What types of documents over time?
df %>% 
  count(decade, type) %>% 
  ggplot(aes(x = factor(decade), y = n, fill = type)) + 
  geom_col() + 
  labs(title = "Document Types by Decade", x = "Decade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>% 
  count(decade, type) %>% 
  group_by(decade) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = factor(decade), y = prop, fill = type)) + 
  geom_col() + 
  labs(title = "Document Types by Decade (Proportions)", x = "Decade", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Document length over time - experiment with different metrics
df %>% 
  group_by(decade) %>% 
  summarise(avg_pages = mean(pages, na.rm = TRUE)) %>% 
  ggplot(aes(x = decade, y = avg_pages)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Average Pages by Decade")

df %>% 
  group_by(decade) %>% 
  summarise(median_tokens = median(tokens, na.rm = TRUE)) %>% 
  ggplot(aes(x = decade, y = median_tokens)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Median Tokens by Decade")

# Rolling average for sentences
df %>% 
  group_by(year) %>% 
  summarise(avg_sentences = mean(sentences, na.rm = TRUE)) %>% 
  arrange(year) %>% 
  mutate(rolling_avg = zoo::rollmean(avg_sentences, k = 10, fill = NA)) %>% 
  ggplot(aes(x = year, y = rolling_avg)) + 
  geom_line() + 
  labs(title = "Sentences (10-year rolling average)")

# ==========================================
# CONTENT & TOPICS - Flexible topic exploration
# ==========================================

# What topics exist?
df %>% count(primaryTopic, sort = TRUE)

df %>% 
  count(primaryTopic, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(x = reorder(primaryTopic, n), y = n)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "Top 10 Primary Topics", x = "Topic")

# Try different numbers of topics
df %>% count(primaryTopic, sort = TRUE) %>% tail(20)  # Rare topics
df %>% count(secondaryTopic, sort = TRUE) %>% head(15)

# Topic confidence - play with thresholds
summary(df$primaryTopicPercentage)

ggplot(df, aes(x = primaryTopicPercentage)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Distribution of Primary Topic Confidence")

df %>% 
  filter(primaryTopicPercentage > 80) %>% 
  count(primaryTopic, sort = TRUE)  # High confidence topics

df %>% 
  filter(primaryTopicPercentage < 30) %>% 
  count(primaryTopic, sort = TRUE)  # Low confidence - might be interesting!

# Topics over time - try different topics
topic <- 'Optics'  # Change this!
df %>% 
  filter(primaryTopic == topic) %>% 
  count(decade) %>% 
  ggplot(aes(x = decade, y = n)) + 
  geom_line() + 
  geom_point() + 
  labs(title = paste(topic, "over time"))

# Compare topics
topics_to_compare <- c('Optics', 'Geography', 'Electricity')  # Modify as needed
df %>% 
  filter(primaryTopic %in% topics_to_compare) %>% 
  count(decade, primaryTopic) %>% 
  ggplot(aes(x = decade, y = n, color = primaryTopic)) + 
  geom_line(alpha = 0.7) + 
  geom_point(alpha = 0.7) + 
  labs(title = "Topic Comparison Over Time")

# ==========================================
# DOCUMENT CHARACTERISTICS - Experiment with different views
# ==========================================

# Length distributions - try different cuts
ggplot(df, aes(x = pages)) + 
  geom_histogram(bins = 50) + 
  labs(title = "Distribution of Page Lengths")

df %>% 
  filter(pages < 50) %>% 
  ggplot(aes(x = pages)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Distribution of Page Lengths (< 50 pages)")

ggplot(df, aes(x = tokens)) + 
  geom_histogram(bins = 50, alpha = 0.7) + 
  labs(title = "Distribution of Token Counts")

# By document type
ggplot(df, aes(x = type, y = pages)) + 
  geom_boxplot() + 
  labs(title = "Page Distribution by Document Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = type, y = tokens)) + 
  geom_boxplot() + 
  labs(title = "Token Distribution by Document Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>% 
  group_by(type) %>% 
  summarise(across(pages, list(mean = ~mean(., na.rm = TRUE),
                               median = ~median(., na.rm = TRUE),
                               sd = ~sd(., na.rm = TRUE))))

# Relationships between variables - try different pairs
ggplot(df, aes(x = pages, y = tokens)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Pages vs Tokens")

ggplot(df, aes(x = pages, y = sentences)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Pages vs Sentences")

ggplot(df, aes(x = sentences, y = tokens)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Sentences vs Tokens")

# Correlations - which variables relate?
numeric_cols <- c('year', 'pages', 'sentences', 'tokens', 'primaryTopicPercentage')
correlation_matrix <- df %>% 
  select(all_of(numeric_cols)) %>% 
  cor(use = "complete.obs")

corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# ==========================================
# MISSING DATA PATTERNS - What's missing and why?
# ==========================================

# Simple missing data check
df %>% 
  summarise_all(~sum(is.na(.))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count")

df %>% 
  summarise_all(~sum(is.na(.)) / n() * 100) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "missing_percent")

# Missing data by time period - might reveal data collection issues
missing_by_decade <- df %>% 
  group_by(decade) %>% 
  summarise_all(~sum(is.na(.)))

missing_by_decade %>% 
  ggplot(aes(x = decade, y = author)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Missing Authors by Decade")

missing_by_decade %>% 
  ggplot(aes(x = decade, y = doi)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Missing DOIs by Decade")

# What's missing together?
df %>% 
  select(author, doi, jstorLink, language) %>% 
  summarise_all(~sum(is.na(.)))

# Documents with no author info - what are they?
no_author <- df %>% filter(is.na(author))
no_author %>% count(type, sort = TRUE)
no_author %>% count(decade, sort = TRUE)

# ==========================================
# AUTHORSHIP - Simple exploration of who wrote what
# ==========================================

# Basic author patterns
sum(!is.na(df$author))  # How many have author info?

# Simple author count (assumes | separator)
df_with_authors <- df %>% 
  filter(!is.na(author)) %>% 
  mutate(n_authors = str_count(author, "\\|") + 1)

df_with_authors %>% count(n_authors, sort = TRUE)

ggplot(df_with_authors, aes(x = n_authors)) + 
  geom_histogram(bins = 20) + 
  labs(title = "Distribution of Number of Authors")

# Collaboration over time
df_with_authors %>% 
  group_by(decade) %>% 
  summarise(avg_authors = mean(n_authors, na.rm = TRUE)) %>% 
  ggplot(aes(x = decade, y = avg_authors)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Average Authors per Paper by Decade")

df_with_authors %>% 
  group_by(decade) %>% 
  summarise(median_authors = median(n_authors, na.rm = TRUE)) %>% 
  ggplot(aes(x = decade, y = median_authors)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Median Authors per Paper by Decade")

# Single vs multi-author by topic
collab_topic <- df_with_authors %>% 
  group_by(primaryTopic) %>% 
  summarise(avg_authors = mean(n_authors, na.rm = TRUE)) %>% 
  arrange(desc(avg_authors)) %>% 
  slice_head(n = 10)

ggplot(collab_topic, aes(x = reorder(primaryTopic, avg_authors), y = avg_authors)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "Most Collaborative Topics", x = "Topic", y = "Average Authors")

# ==========================================
# JOURNALS & PUBLICATION PATTERNS
# ==========================================

# What journals are in here?
df %>% count(journal, sort = TRUE)
df %>% count(jrnl, sort = TRUE)  # Abbreviated journal names

# Journal patterns over time
main_journals <- df %>% count(journal, sort = TRUE) %>% slice_head(n = 5)
main_journal <- main_journals$journal[1]

df %>% 
  filter(journal == main_journal) %>% 
  count(decade) %>% 
  ggplot(aes(x = decade, y = n)) + 
  geom_line() + 
  geom_point() + 
  labs(title = paste(main_journal, "over time"))

# Volume numbers over time - shows publication frequency
ggplot(df, aes(x = year, y = volume)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Volume Numbers Over Time")

df %>% 
  group_by(year) %>% 
  summarise(max_volume = max(volume, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = max_volume)) + 
  geom_line() + 
  labs(title = "Max Volume per Year")

# ==========================================
# QUALITY & COMPLETENESS PATTERNS
# ==========================================

# DOI patterns - when did they start?
df %>% filter(!is.na(doi)) %>% summarise(min_year = min(year, na.rm = TRUE))  # First DOI year

df %>% 
  group_by(decade) %>% 
  summarise(doi_count = sum(!is.na(doi))) %>% 
  ggplot(aes(x = decade, y = doi_count)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "DOIs by Decade")

# JSTOR availability
df %>% 
  group_by(decade) %>% 
  summarise(jstor_count = sum(!is.na(jstorLink))) %>% 
  ggplot(aes(x = decade, y = jstor_count)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "JSTOR Links by Decade")

# Language info - when available?
df %>% 
  filter(!is.na(language)) %>% 
  count(decade) %>% 
  ggplot(aes(x = decade, y = n)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Language Info by Decade")

df %>% count(language, sort = TRUE)

# ==========================================
# SIMPLE COMPARATIVE ANALYSIS
# ==========================================

# Compare different time periods
early <- df %>% filter(year < 1800)
late <- df %>% filter(year >= 1800)

cat("Early period (pre-1800):\n")
cat("  Average pages:", round(mean(early$pages, na.rm = TRUE), 1), "\n")
cat("  Average tokens:", round(mean(early$tokens, na.rm = TRUE), 0), "\n")
early_mode <- early %>% count(primaryTopic, sort = TRUE) %>% slice_head(n = 1) %>% pull(primaryTopic)
cat("  Most common topic:", early_mode, "\n")

cat("\nLate period (1800+):\n")
cat("  Average pages:", round(mean(late$pages, na.rm = TRUE), 1), "\n")
cat("  Average tokens:", round(mean(late$tokens, na.rm = TRUE), 0), "\n")
late_mode <- late %>% count(primaryTopic, sort = TRUE) %>% slice_head(n = 1) %>% pull(primaryTopic)
cat("  Most common topic:", late_mode, "\n")

# Compare document types
doc_types <- unique(df$type)
for(doc_type in doc_types) {
  subset_data <- df %>% filter(type == doc_type)
  top_topics <- subset_data %>% 
    count(primaryTopic, sort = TRUE) %>% 
    slice_head(n = 3) %>% 
    pull(primaryTopic)
  
  cat("\n", doc_type, "s:\n", sep = "")
  cat("  Count:", nrow(subset_data), "\n")
  cat("  Avg length:", round(mean(subset_data$pages, na.rm = TRUE), 1), "pages\n")
  cat("  Common topics:", paste(top_topics, collapse = ", "), "\n")
}