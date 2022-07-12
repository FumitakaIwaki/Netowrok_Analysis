# REPLで`@time main()
# gcc -04でコンパイルするとJuliaの２倍早い
using Random
using Printf
using DelimitedFiles

#これらは入力に合わせてより小さくした方がいい
const NN = 50 #総node数
const LL = 1000 #総link数
const KK = 50 #cluster数の最大値

# datファイルからネットワーク構築
function network(filename, directed, from_node, to_node, weight, strength, transition, weight_cut_off, rprob)
  f = open(filename)
  lines = readlines(f, keep=false)
  if rprob # "\r"が含まれているかどうか
    lines = split(lines[1], "\r")
  end

  l = 0
  for line in lines
    from, to, wght = parse.(Int, split(line, ","))
    # wght = 1.0 #重みを無視
    l += 1
    from_node[l]  = from
    to_node[l]    = to
    weight[l]     = wght 
    if !directed #無向グラフのとき
      # 無向の場合はlinkを逆向きにコピーする（i→jとj→iを作る）ので、link数lをカウントアップして使う
      l += 1
      from_node[l] = to
      to_node[l] = from
      weight[l] = wght
    end
  end

  L = l #link数
  # min(x, y)は2値、minimum(itr)はイテレータ(配列)が引数
  #from_nodeとto_nodeの最小値をそれぞれ求め、その2つのうち小さい方をn_minとする
  n_min = min(minimum(from_node[1:L]), minimum(to_node[1:L])) #nodeの最小値
  n_max = max(maximum(from_node[1:L]), maximum(to_node[1:L])) #nodeの最大値
  N = n_max - n_min + 1 #node数

  @printf("Total number of links: %d\n", L)
  @printf("Total number of nodes: %d\n", N)

  # 次は単なる全体初期化でいいなら、最初のzerosでOK
  # for n = n_min:n_max
  #   strength[n] = 0
  # end
  for l = 1:L
    #from_nodeが持つ全てのlinkの重みを足す
    strength[from_node[l]] += weight[l]
  end
  for l = 1:L
    # 遷移確率を決める
    transition[l] = weight[l] / (strength[from_node[l]] + weight_cut_off)
  end
  (L, n_min, n_max, N)
end

function data_points(n_min, n_max, N, L, weight_cut_off, input_PR, p_PR, p_PR_pre, p_PR_tilde, strength, transition, to_node, from_node, dt_PR, rho)
  #danglingJump = 0.0

  #まず各nodeのPageRankの値を求める
  for n = n_min:n_max
    # 各nodeのPageRankの値を1/Nに初期化
    p_PR[n] = 1.0 / N
  end
  for g = 1:10000 # 10000回実行
    for n = n_min:n_max
      # p_PR_preに現在のPageRankの値をコピー
      # p_PR_preは1つ前の状態のPageRankの値
      p_PR_pre[n] = p_PR[n]
    end
    danglingJump = 0.0
    for n = n_min:n_max
      # input_PRを初期化
      input_PR[n] = 0.0
      # danglingJumpの値を(各nodeの値*軽い調整)の総和に設定
      danglingJump += p_PR_pre[n] * (weight_cut_off / (strength[n] + weight_cut_off))
    end
    for l = 1:L
      # input_PR(流入量)を(遷移確率*from_nodeの値)で決定
      input_PR[to_node[l]] += transition[l] * p_PR_pre[from_node[l]]
    end
    for n = n_min:n_max
      # PageRankの値を決定
      p_PR[n] = p_PR_pre[n] + dt_PR * (0.0 - p_PR_pre[n] + (1.0 - rho) * input_PR[n] + (1.0 - rho) * danglingJump / N + rho / N)
    end
  end

  for l = 1:L
    # 遷移確率*PageRank
    p_PR_tilde[l] = transition[l] * p_PR[from_node[l]]
  end
  sum_p_PR_tilde = sum(p_PR_tilde)
  # @printf("sum_p_PR_tilde = %lf\n", sum_p_PR_tilde)
  for l = 1:L
    # 総和が1になるように調整
    p_PR_tilde[l] /= sum_p_PR_tilde
  end
  # @printf("sum of p_PR_tilde = %lf\n", sum(p_PR_tilde[1:L]))
end  

function initialization(p, pi_cluster, K, n_min, n_max)
  XXX::Float64 = 0.0

  # マルコフ連鎖
  for k = 1:K
    XXX = 0.0
    for n = n_min:n_max
      # ランダムにpの値を取得(初期値)
      p[n, k] = rand()
      # XXXをその総和に
      XXX += p[n, k]
    end
    for n = n_min:n_max
      # pを確率として規格化(総和XXXで各値を割る)
      # 初期値として設定完了
      p[n, k] /= XXX
    end
  end

  # Mステップのパラメータ
  # pi_clusterの値を同様に初期設定
  XXX = 0.0
  for k = 1:K
    pi_cluster[k] = rand()
    XXX += pi_cluster[k]
  end
  for k = 1:K
    pi_cluster[k] /= XXX
    # @printf("pi[%d]=%lf\n", k, pi_cluster[k])
  end
end

function EM_step(K, L, N, n_min, n_max, pi_cluster_pre, pi_cluster, p_pre, p, p_PR_tilde, input, from_n, to_n, from_n_without_pi, to_n_without_pi, weight_cut_off, strength, to_node, from_node, rho, transition, alpha)
  XXX = 0.0
  danglingJump = 0.0

  # pi_cluster
  # 1つ前の値として現在の値を格納
  for k = 1:K
    pi_cluster_pre[k] = pi_cluster[k]
    for n = n_min:n_max
      p_pre[n, k] = p[n, k]
    end
  end

  for k = 1:K
    danglingJump = 0.0
    for n = n_min:n_max
      input[n] = 0.0
      from_n[n] = 0.0
      to_n[n] = 0.0
      from_n_without_pi[n] = 0.0
      to_n_without_pi[n] = 0.0
      danglingJump = danglingJump + p_pre[n, k] * (weight_cut_off / (strength[n] + weight_cut_off))
    end
    pi_cluster[k] = 0.0
    for l = 1:L
      XXX = 0.0
      for kk = 1:K
        XXX = XXX + pi_cluster_pre[kk] * p_pre[to_node[l], kk] * p_pre[from_node[l], kk]
      end
      responsibility = pi_cluster_pre[k] * p_pre[to_node[l], k] * p_pre[from_node[l], k] / (XXX + 0.0000000001)
      pi_cluster[k] = pi_cluster[k] + p_PR_tilde[l] * responsibility
      responsibility_without_pi = p_pre[to_node[l], k] * p_pre[from_node[l], k] / (XXX + 0.0000000001)
      input[to_node[l]] = input[to_node[l]] + transition[l] * p_pre[from_node[l], k]
      from_n[from_node[l]] = from_n[from_node[l]] + p_PR_tilde[l] * responsibility
      to_n[to_node[l]] = to_n[to_node[l]] + p_PR_tilde[l] * responsibility
      from_n_without_pi[from_node[l]] = from_n_without_pi[from_node[l]] + p_PR_tilde[l] * responsibility_without_pi
      to_n_without_pi[to_node[l]] = to_n_without_pi[to_node[l]] + p_PR_tilde[l] * responsibility_without_pi
    end

    for n = n_min:n_max
      p[n, k] = (alpha / (alpha + pi_cluster[k])) * ((1.0 - rho) * input[n] + (1.0 - rho) * danglingJump / N + rho / N) + (1.0 / (alpha + pi_cluster[k])) * 0.5 * (from_n[n] + to_n[n])
    end
  end
end

function print_pi_cluster(K, pi_cluster)
  for k = 1:K
    @printf("pi_cluster[%d]=%lf\n", k, pi_cluster[k])
  end
end

function calculate_belongingness(n_min, n_max, K, pi_cluster, p, belongingness, maxAPosteriori, k_MAP)
  for n = n_min:n_max
    S_pi_pk = 0.0
    for k = 1:K
      S_pi_pk += pi_cluster[k] * p[n, k]
    end
    for k = 1:K
      belongingness[k, n] = pi_cluster[k] * p[n, k] / (S_pi_pk + 0.0000000001)
    end
  end

  for n = n_min:n_max
    k_MAP[n] = 1 - 100
    posterior = 0.0
    for k = 1:K
      xxx = belongingness[k, n]
      if xxx > posterior
        posterior = xxx
        k_MAP[n] = k
      end
    end
    for k = 1:K
      maxAPosteriori[k, n] = 0.0
    end
    maxAPosteriori[k_MAP[n], n] = 1.0
  end
end

function print_file(pi_cluster, K, n_min, n_max, k_MAP, p, k_MAP_series)
  writedlm("data/f_pi.csv", pi_cluster[1:K], ',')
  writedlm("data/f_p.csv", hcat(n_min:n_max, p[n_min:n_max, 1:K]), ',')
  writedlm("data/f_main_belongingness.csv", hcat(n_min:n_max, k_MAP[n_min:n_max]), ',')
  writedlm("data/f_pi_p.csv", hcat(n_min:n_max, p[n_min:n_max, 1:K] * pi_cluster[1:K]), ',')
  # writedlm("data/f_main_belongingness_series.csv", k_MAP_series, ',')
end

function main(;filename="data/edge_list.csv", dir=0, r_seed=0, alpha_ini=0.5, rprob=false)
  # filename=ネットワークデータ dir=drectedかどうか r_seed=疑似乱数用 alpha_ini=アルファの初期値
  # rprob=ファイルに"\r"が入っているかどうか
  PRE_GN = 0 # プレステップ(相転移考えるときに最初に回しておく)
  GN = 100 + PRE_GN # EMのステップ数
  PN = GN/2 # 画面・ファイル出力の間隔

  K = 10 # クラスターの(最大)値
  # クラスター数はKからスタート

  transition = zeros(Float64, LL) # 遷移確率行列(配列)
  from_node = zeros(Int, LL) #有向辺の出本のnode
  to_node = zeros(Int, LL) #有向辺の出先のnode
  weight = zeros(Float64, LL) #重み
  strength = zeros(Float64, NN) #from_nodeからのlinkのweihgtの総和

  # Input data: PageRank
  p_PR = zeros(Float64, NN)
  p_PR_pre = zeros(Float64, NN)
  p_PR_tilde = zeros(Float64, LL)
  input_PR = zeros(Float64, NN)

  # nodes
  # クラスターkにおける頂点iの確率p(i|k)
  p = zeros(Float64, NN, KK)
  p_pre = zeros(Float64, NN, KK)
  input = zeros(Float64, NN)
  from_n = zeros(Float64, NN)
  to_n = zeros(Float64, NN)
  from_n_without_pi = zeros(Float64, NN)
  to_n_without_pi = zeros(Float64, NN)

  # EMアルゴリズム
  #  Eステップ
  responsibility = 0.0
  responsibility_without_pi = 0.0
  #  Mステップ
  pi_cluster = zeros(Float64, KK)
  pi_cluster_pre = zeros(Float64, KK)
  pi_cluster_series = zeros(Float64, GN+1, KK)

  # 一般化されたモジュラリティ
  belongingness = zeros(Float64, KK, NN)
  maxAPosteriori = zeros(Float64, KK, NN)
  k_MAP = zeros(Int, NN)
  k_MAP_series = zeros(Int, GN+1, NN)
  dt = 1.0
  dt_PR = dt

  # alpha_ini = alpha_finだとdivision by zeroが起こるためずらしている
  alpha_fin = alpha_ini + 0.00000000001 #alphaの最終値
  chi_ini = 0.5
  chi_fin = 0.02 + 0.0000000001
  epsilon = 0.0000000001

  common_ratio_alpha = (alpha_fin / alpha_ini)^(1.0 / GN)
  common_ratio_chi = exp((log(chi_fin) - log(chi_ini)) / GN)

  # 有効か無向かの判定
  # rho=ジャンプの確率
  if dir == 1
    directed = true
    rho = 0.15
  elseif dir == 0
    directed = false
    rho = 0.0
  else
    println("error")
  end

  weight_cut_off = 10^-16 # linkの重みの二値化処理の閾値

  L, n_min, n_max, N = network(filename, directed, from_node, to_node, weight, strength, transition, weight_cut_off, rprob)

  data_points(n_min, n_max, N, L, weight_cut_off, input_PR, p_PR, p_PR_pre, p_PR_tilde, strength, transition, to_node, from_node, dt_PR, rho)

  Random.seed!(r_seed)

  initialization(p, pi_cluster, K, n_min, n_max) #初期値設定
  pi_cluster_series[1, :] = pi_cluster
  k_MAP_series[1, n_min:n_max] = Array(n_min:n_max)

  alpha = alpha_ini
  for g = 1:GN
    alpha = common_ratio_alpha * alpha
    if g <= PRE_GN
      alpha = alpha_ini
    end
    EM_step(K, L, N, n_min, n_max, pi_cluster_pre, pi_cluster, p_pre, p, p_PR_tilde, input, from_n, to_n, from_n_without_pi, to_n_without_pi, weight_cut_off, strength, to_node, from_node, rho, transition, alpha)
    calculate_belongingness(n_min, n_max, K, pi_cluster, p, belongingness, maxAPosteriori, k_MAP)
    pi_cluster_series[g+1, :] = pi_cluster
    k_MAP_series[g+1, :] = k_MAP
    if (g % PN) == 0
      println()
      @printf("g=%d\n", g)
      print_pi_cluster(K, pi_cluster)
      println()
    end
  end
  calculate_belongingness(n_min, n_max, K, pi_cluster, p, belongingness, maxAPosteriori, k_MAP)
  k_MAP_series = k_MAP_series'

  #ファイル出力
  print_file(pi_cluster, K, n_min, n_max, k_MAP, p, k_MAP_series)
  return pi_cluster_series[:, 1:K]
end