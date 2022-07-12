library(rtweet)
library(dplyr)
library(readr)

N <- 30000 # 抽出するツイート数
# 20favorite以上のツイートを抽出
# スパム回避のための簡易的な対策
result <- search_tweets(
  "ウクライナ min_faves:20", n = N, lang = "ja", retryonratelimit = T
)

result.df <- data.frame(
  created_at = result$created_at,
  screen_name = result$screen_name,
  text = result$text,
  favorites = result$favorite_count,
  retweets = result$retweet_count
)

write.csv(
  result.df,
  file = paste(getwd(), "/data/result.csv", sep = ""),
  row.names = FALSE
)
