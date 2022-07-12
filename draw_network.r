library(dplyr)
library(readr)
library(igraph)
library(tidyr)
library(tidyverse)
library(ggraph)

# データの読み込み
# 頂点になるアカウントの名前
accounts <- read_csv("data/accounts.csv", col_names = T)
accounts <- accounts$screen_name
# 辺リスト
edge_list <- read_csv(
  "data/edge_list.csv",
  col_names = c("from", "to", "weight")
)
# 頂点数
N <- max(edge_list[, 1:2]) - min(edge_list[, 1:2]) + 1

# MDMCの結果
belongingness <- read_csv("data/f_main_belongingness.csv", col_names = c("node", "cluster"))

# 隣接行列
adj <- data.frame(matrix(0, nrow = N, ncol = N))
# 重みの切り捨て基準
# この重み以下の辺は削除
weight_cut_off <- 0
# 隣接行列作成
for (i in 1:nrow(edge_list)) {
  if (edge_list$weight[i] > weight_cut_off) {
    adj[edge_list$from[i], edge_list$to[i]] <- edge_list$weight[i]
    adj[edge_list$to[i], edge_list$from[i]] <- edge_list$weight[i]
  } else {
    adj[edge_list$from[i], edge_list$to[i]] <- 0
    adj[edge_list$to[i], edge_list$from[i]] <- 0
  }
}
# 辺を持たない頂点を削除
k <- 0
for (i in 1:N) {
  i <- i - k
  if ((sum(adj[i,]) + sum(adj[,i])) == 0) {
    adj <- adj[-i,]
    adj <- adj[,-i]
    accounts <- accounts[-i]
    belongingness[-i, ]
    k <- k + 1
  }
}
N <- N - k # 頂点数調整

# グラフ作成
adj <- as.matrix(adj)
g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = T)
# 辺の重みで辺の太さを調整
E(g)$width <- sqrt(E(g)$weight) / 2
# 頂点のラベルにアカウント名付与
V(g)$name <- accounts

# コミュニティ抽出
# louvain法
louvain <- cluster_louvain(g)
# MDMC
clusters <- sort(unique(belongingness$cluster))
membership <- c()
for (i in 1:N) {
  membership <- c(membership, which(clusters == belongingness$cluster[i]))
}
mdmc <- make_clusters(g, membership)

# プロット
set.seed(1)
png("data/plot.png", width = 1000, height = 1000)
plot(mdmc, g,  vertex.size = 10, vertex.label.cex = .8)
dev.off()
