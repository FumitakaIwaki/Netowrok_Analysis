library(rtweet)
library(dplyr)
library(readr)
library(stringr)

# 理由はわからないがnotebook型だとうまく読み込めない
# ツイート情報
result.df <- read_csv("data/result.csv")

n_act <- 30 # 抽出するアカウント数
# resultデータからリツイート数Top n_actのアカウントを抽出
accounts <- unique(
  result.df[order(result.df$retweets, decreasing = T),]$screen_name
  )
accounts <- head(accounts, n_act)
accounts.df <- data.frame(accounts)
names(accounts.df) <- c("screen_name")

# 各アカウントのフォロワーを抽出
n_fol <- 10000 # 抽出するフォロワー数
# フォロワー抽出
followers <- get_followers(
  accounts.df$screen_name[1], n = n_fol, retryonratelimit = T
  )
# フォロワーのuser_idを取得
followers <- lookup_users(as_userid(followers$user_id))
# データフレームの1列目のデータとする
followers.df <- data.frame(
  user_id = followers$user_id
  )
# フォロワー数がn_folより小さい場合、0で補填
# データフレームにするため
if (nrow(followers) < n_fol) {
  zeros <- data.frame(matrix(0, nrow = n_fol - nrow(followers), ncol = 1))
  names(zeros) <- c("user_id")
  followers.df <- rbind(followers.df, zeros)
}

# 2列目以降も同様に
for (i in 2:length(accounts.df[,1])) {
  followers <- get_followers(
    accounts.df$screen_name[i], n = 5000, retryonratelimit = T
    )
  followers <- lookup_users(as_userid(followers$user_id))
  followers <- data.frame(
    user_id = followers$user_id
  )
  if (nrow(followers) < n_fol) {
  zeros <- data.frame(matrix(0, nrow = n_fol - nrow(followers), ncol = 1))
  names(zeros) <- c("user_id")
  followers <- rbind(followers, zeros)
  }
  # データフレームに列方向でappend
  followers.df <- cbind(followers.df, followers)
}

# 列名を各アカウント名に
names(followers.df) <- accounts.df$screen_name
# データ書き出し
write_csv(
  followers.df,
  file = paste(getwd(), "/data/follower_data.csv", sep = "")
)

write_csv(
  accounts.df,
  file = paste(getwd(), "/data/accounts.csv", sep = '')
)
