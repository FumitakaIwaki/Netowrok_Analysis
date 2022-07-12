library(dplyr)
library(readr)
library(stringr)

# データ読み込み&データフレーム生成
followers_user_id <- read_csv("data/follower_data.csv", col_names = T)
followers.df <- data.frame(followers_user_id)

# 辺リスト作成
C <- ncol(followers.df)
# appendするために1行目3列で生成
edge_list <- matrix(nrow = 1, ncol = 3)
for (i in 1:(C - 1)) {
  data1 <- followers.df[, i]
  # i列目のデータに0が含まれる場合
  if (length(data1[which(data1 %in% 0 == TRUE)]) != 0) {
    # 0の要素を削除
    data1 <- data1[-which(data1 == 0)]
  }
  for (j in (i + 1):C) {
    data2 <- followers.df[, j]
    # j列目のデータに0が含まれる場合
    if (length(data2[which(data2 %in% 0 == TRUE)]) != 0) {
      data2 <- data2[-which(data2 == 0)]
    }
    # i列目とj列目で重複している要素の数を重みに
    weight <- length(data2[which(data2 %in% data1 == TRUE)])
    # 重みが0でないなら
    if (weight != 0) {
      # 辺リストに追加
      edge_list <- rbind(edge_list, matrix(c(i, j, weight), nrow = 1, ncol = 3))
    }
  }
}
# 辺リストの1行目は不要
edge_list <- edge_list[-1, ]
edge_list.df <- data.frame(edge_list)

# データ書き出し
write_csv(
  edge_list.df,
  file = paste(getwd(), "/data/edge_list.csv", sep = ""),
  col_names = FALSE
)