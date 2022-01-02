#Install igraph package
install.packages("igraph")

#Install Stringr
install.packages("stringr")

#Apply igraph library
library(igraph)

#Apply Stringr
library(stringr)

#Set work directory
setwd("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph")

#CSV file name.
csvFileName = "CFG_igraph_Edge_org.apache.catalina.manager.ManagerServlet_deploy.csv"
target_c = c()
target_c[1] = str_sub(csvFileName, start = 17, end = -5)

#Teaching Data : Modified 1 , No-changed 0
teaching_c = c()
teaching_c[1] = 1

#Read csv Edge-Edge data.
data = read.csv(csvFileName, header=T, sep="\t")
head(data, 2)

#Calculate the number of Jimple step.
length = length(data)
step_c = c()
step_c[1] = length

#Detect CRUD info.
tmp = data[,3]
data[,3]
idau = 0
while(length > 0){
  if(tmp[length] == 1){
    idau = idau+1
  }
  length = length-1
}
idau_c = c()
idau_c[1] = idau / length(tmp)
idau_c

#Delete Third CRUD boolean data.
data = data[,-3]
#Check by std-out.
head(data, 2)


#Convert the data-frame to graph data.
g <- graph.data.frame(data, directed = TRUE)
#Get ride of the multiple edges and self loops from whole of graph.
g <- simplify(g,remove.multiple=T,remove.loops=T)
#Visualize this graph for check.
plot(g, vertex.size=3, vertex.label.font=0.1, vertex.label.cex=0.1, edge.arrow.size=0.3, vertex.color="blue", layout=layout_with_kk)

#Results DataFrame
resultDf <- data.frame(matrix(rep(NA, 23), nrow=1))[numeric(0), ]
colnames(resultDf) = c("No", "edge_density", "graph_degree", "closeness_centrality", "eccentricity_centrality", 
                       "centralization", "page_rank", "bonapower", "betweenness", "community_edge_betweenness", "dendrogram",
                       "random_walk_trap_community", "frist_greedy_community", "eigenvector_community", "multiLevel_community",
                       "spinglass_community", "propagation_community", "information_community", "link_community", "ocg", "structure_fall", 
                       "code_step", "cyclomatic_complexity")

#Edge Density
edge_dencity_c = c()
edge_dencity_c[1] = c(edge_density(g))

#Graph Degree (variant)
graph_degree_c = c()
graph_degree_c[1] = var(degree(g))
graph_degree_c

#近接中心性:ある要件を満たすグラフの最短経路長の「合計」に着目したものもあります。
#あるノードから他のノードへの最短経路長の合計*4の逆数を取ったものを「近接中心性」
#(closeness centrality)と呼びます。
pdf(str_c(csvFileName, "_closeness_centrality.pdf")) #Open PDF
closeness_centrality_c = c()
g.cls<-closeness(g,mode = 'out')
plot(g,vertex.size=g.cls*20000, main='Closeness Centrality', edge.arrow.size=0.3, vertex.label.cex=0.1, vertex.color="blue", layout=layout_with_kk)
closeness_centrality_c[1] = var(g.cls)
closeness_centrality_c
dev.off() #Save PDF


#「離心中心性」(eccentricity centrality):グラフ全体を最短経路長を長さの単位とする
#円（球）*2とみなした上で、その「中心」からの距離*3の逆数を取ったもの。端的に
#言えば、グラフのど真ん中に近ければ近いほど大きくなる。
pdf(str_c(csvFileName, "_eccentricity_centrality.pdf")) #Open PDF
eccentricity_centrality_c = c()
eccentricity_centrality<-closeness(g,mode = "in") #in, all, out, total
plot(g, vertex.size=eccentricity_centrality*20000, main='Eccentricity Centrality', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
eccentricity_centrality_c[1] = var(eccentricity_centrality)
eccentricity_centrality_c
dev.off() #Save PDF

#Centralization
#固有ベクトル中心性:他の次数中心性の高いノード「とつながっている」ノードを評価するような指標
pdf(str_c(csvFileName, "_centralization.pdf")) #Open PDF
centralization_c = c()
g.evcent<-evcent(g,directed = T)
plot(g,vertex.size=g.evcent$vector*15, main='Centralization', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
centralization_c[1] = var(str_subset(unlist(g.evcent), "\\d+"))
centralization_c[1]
dev.off() #Save PDF

#PageRank:「リンク（流入）」を重視するようにした上で、さらに分離グラフや強連結でない
#有向グラフにも適用できるようにしたのが、あのSergey BrinとLawrence "Larry" Pageの2人
#が考案してGoogle検索の基になったPageRank
pdf(str_c(csvFileName, "_page_rank.pdf")) #Open PDF
page_rank_c = c()
gd.pr<-page.rank(g,directed=T)
plot(g,vertex.size=gd.pr$vector*300, main='Page Pank', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
page_rank_c[1] = var(str_subset(unlist(gd.pr), "\\d+"))
page_rank_c[1]
dev.off()

#Bonapower:中心性の高いノードとつながるノードの中心性が低くなる
pdf(str_c(csvFileName, "_bonpow.pdf"))
bonpow_c = c()
g.bp<-bonpow(g,exponent=0.2)
plot(g,vertex.size=g.bp*5,main='Bona Power',edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
bonpow_c[1] = var(g.bp)
bonpow_c[1]
dev.off()

#Betweenness:あるノードが他のノード間の最短経路上に位置する「程度」を中心性指標とし
#たものが媒介中心性(betweeness centrality)
pdf(str_c(csvFileName, "_betweenness.pdf"))
betweenness_c = c()
g.bw<-betweenness(g,directed = T)
plot(g,vertex.size=g.bw*0.0025,main='Betweeness',edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
betweenness_c[1] = var(g.bw)
betweenness_c
dev.off()

##########COMMUNITY#############
#Convert the data-frame to graph data.
gud <- graph.data.frame(data, directed = FALSE)
#Get ride of the multiple edges and self loops from whole of graph.
gud <- simplify(gud,remove.multiple=T,remove.loops=T)

#community edge betweenness
community_edge_betweenness_c = c()
g.com<-edge.betweenness.community(gud, weights = E(g)$value, directed = F)

community_edge_betweenness_c[1] = var(str_subset(unlist(g.com), "\\d+"))
community_edge_betweenness_c[1]
#dendrogram
g.dend<-as.dendrogram(g.com)
plot(g.dend, label.cex=0.1)
dendPlot(g.com, label.cex=0.1)
#graph
g.Q<-rep(0,77)
for (i in 1:77){
  memb<-cut_at(g.com,i)
  g.Q[i]<-modularity(gud, memb)
}
plot(g.Q,type='b')
which(g.Q==max(g.Q))
#display
g.mb13<-cut_at(g.com,13)
plot(gud, vertex.size=3,vertex.color=g.mb13,vertex.label.cex=0.1, layout=layout_with_kk)
plot(gud, vertex.size=3 ,vertex.color=g.com$membership,vertex.label.cex=0.1, layout=layout_with_kk)
g.mb13

#ランダムウォークに基づくコミュニティ:Random Walk trap community
gud.wt.com<-walktrap.community(gud,weights=E(gud)$value)
set.seed(25)
plot(gud, vertex.size=3, vertex.color=gud.wt.com$membership, vertex.label.cex=0.1, layout=layout_with_kk)

#貪欲アルゴリズムに基づくコミュニティ:Frist greedy community
gud2 <- simplify(gud,remove.multiple=T,remove.loops=T)
gud2.fg.com<-fastgreedy.community(gud2,weights=E(gud)$value)
plot(gud2,vertex.size=3, vertex.color=gud.wt.com$membership, vertex.label.cex=0.1, layout=layout_with_kk)

#固有ベクトルに基づくコミュニティ:Eigenvector community
gud.le.com<-leading.eigenvector.community(gud,weights=E(gud)$value)
set.seed(25)
plot(gud,vertex.size=3,vertex.color=gud.le.com$membership, vertex.label.cex=0.1, layout=layout_with_kk)

#多段階最適化に基づくコミュニティ検出:MultiLevel Community
gud.ml.com<-multilevel.community(gud,weights=E(gud)$value)
set.seed(25)
plot(gud, vertex.size=3,vertex.color=gud.ml.com$membership, vertex.label.cex=0.1, layout=layout_with_kk)

#スピングラス法に基づくコミュニティ検出:Spinglass community
gud.sg.com<-spinglass.community(gud,weights=E(gud)$value,spins=15)
set.seed(25)
plot(gud,vertex.size=3 ,vertex.color=gud.com$membership,vertex.label.cex=0.1, layout=layout_with_kk)

#ラベル伝播法に基づくコミュニティ:Propagation community
gud.lp.com<-label.propagation.community(gud,weights=E(gud)$value)
set.seed(25)
plot(gud, vertex.size=3,vertex.color=gud.ml.com$membership,vertex.label.cex=0.1, layout=layout_with_kk)

#Infomap法に基づくコミュニティ検出:Information Community
gud.im.com<-infomap.community(gud,e.weights=E(gud)$value)
set.seed(25)
plot(gud, vertex.size=3,vertex.color=gud.ml.com$membership, vertex.label.cex=0.1, layout=layout_with_kk)


#Install LinkCommunity library#
install.packages("linkcomm")
library(linkcomm)

#LinkCommunity
gl = read.csv("CFG_igraph_Edge_org.apache.catalina.manager.ManagerServlet_deploy.csv", header=T, sep="\t") #read data as dataframe
gl = gl[,-3]
head(gl)
gl.lc = getLinkCommunities(gl)
plot(gl.lc)
plot(gl.lc,type='graph',margin=-0.01)
gl.lc = getLinkCommunities(gl)
plot(gl.lc,type='graph',margin=-0.01)
plotLinkCommGraph(gl.lc)
plotLinkCommMembers(gl.lc)
getCommunityCentrality(gl.lc)

#OCG
gl.ocg<-getOCG.clusters(gl)
set.seed(25)
plot(gl.ocg,type='graph')
plot(gl.ocg,vertex.size=gl.ocg*100)
gud.le.com<-leading.eigenvector.community(gud,weights=E(gud)$value)
plot(gud.le.com,type='graph')

##ストラクチャーホール　拘束性（低いほどよい）
structure_fall_c = c()
structure_fall_c = var(constraint(g))
structure_fall_c


#Put each vector object data into the dataframe.
resultDf = data.frame(target_c, step_c, idau_c, edge_dencity_c, graph_degree_c, eccentricity_centrality_c, centralization_c, page_rank_c, bonpow_c, betweenness_c)
resultDf

