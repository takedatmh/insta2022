#---Import packages from CPAN---#

#Install igraph package
install.packages("igraph")

#Install Stringr
install.packages("stringr")

#Install bestglm for BIC
install.packages("bestglm")

#---Read Library---#
#Apply igraph library
library(igraph)

#Apply Stringr
library(stringr)

#Apply bestglm
library(bestglm)


#Set working directory
  ##In case of my local directory.
  wdPath = "/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv"
  setwd(wdPath)
  
  ##In case of on the my Google Drive when we execute this R script on the Google Colaborator.
  #TODO
  
  ##Vector object for each graph feature value like Ede_Dencity something like that.
  target_c = c()
  teaching_c = c()
  step_c = c()
  ccna_c = c()
  idau_c = c()
  edge_dencity_c = c()
  graph_degree_c = c()
  closeness_centrality_c = c()
  eccentricity_centrality_c = c()
  centralization_c = c()
  page_rank_c = c()
  bonpow_c = c()
  betweenness_c = c()
  structure_fall_c = c()
  targetFQCN_c = c()
  
  
#Loop : Define the following field objects:
  ##ListOfCsv : Stored all of csv file path information.
  ListOfCsv = c()
  ##NumOfCsv  : The value how many times this graph analysis process should be repeated.
  NumOfCsv = 0

  ListOfFile = c()
  NumOfFiles = 0
  fileName = ""
  data = data.frame("")
  resultDf = data.frame("")
  

#Loop : Count the number of the csv files.
  ##If the target edge csv file is found out, NumOfCsv is counted up and the csv file path is stored into the ListOfCsv.
  ListOfFile = list.files("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv")
  NumOfFiles = length(ListOfFile)
  NumOfFiles
  
#Loop : Calculate each graph specific values and generate graph visual data.
i = 1
for(i in 1:NumOfFiles){
  ##Read the edge's csv files under the designated working directory.
  fileName = str_trim(ListOfFile[i], side="both")
  data = read.csv(fileName, header=T, sep="\t")
  ##Store each calculated specific value into each value's vector object. 
  ##At the same time, the binary 0 or 1 data is put into this vector's the first column as the teaching data.

  #CSV file name which is appeared such FQCN_METHOD.
  target_c[i] = str_sub(ListOfFile[i], start = 17, end = -5)
  
  #Split target_c[i] by _ to detect java class FQCN.
  tmp_list = str_split(target_c[i], pattern="_")
  fqcn = tmp_list[[1]]
  targetFQCN_c[i] = fqcn[1]

  #Teaching Data : Modified 1 , No-changed 0  
  teaching_c[i] = 1
  #TODO: How to integrate github information?
  #Read the file of the number of commit for each java file.
  NumOfCommit = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/count/NumOfCommit4Java.csv", header=F, s="\t")
  
  
  #Calculate the number of Jimple step.
  length = data.frame(table(data[,3]))
  step_c[i] = length[1,2] + length[2,2]
  
  #Calculate CCNA = (The number of IF or GOTO jimple statement) - 1
  s = 1
  maxStep = step_c[i]
  count = 0
  if(!is.na(maxStep) && maxStep > 1) {
    for(s in 1:maxStep){
      subset = str_sub(data[s,1], start=1, end=2)
      if(subset == "if"){
        count = count + 1
      } else if( subset == "go") {
        count = count + 1
      }
    }
  } else if(!is.na(maxStep) && maxStep <= 1){
    count = 1
  } else if(is.na(maxStep)){
    count = 1
  }
  ccna_c[i] = count
  
  #Detect CRUD info.
  length[2,2]
  idau_c[i] = length[2,2] / step_c[i]
  
  #Delete Third CRUD boolean data.
  data = data[,-3]
  
  #Convert the data-frame to graph data.
  g <- graph.data.frame(data, directed = TRUE)
  #Get ride of the multiple edges and self loops from whole of graph.
  g <- simplify(g,remove.multiple=T,remove.loops=T)
  #Visualize this graph for check.
  plot(g, vertex.size=3, vertex.label.font=0.1, vertex.label.cex=0.1, edge.arrow.size=0.3, vertex.color="blue", layout=layout_with_kk)
  
  #Edge Density
  edge_dencity_c[i] = c(edge_density(g))
  
  #Graph Degree (variant)
  graph_degree_c[i] = var(degree(g))

  #近接中心性:ある要件を満たすグラフの最短経路長の「合計」に着目したものもあります。
  #あるノードから他のノードへの最短経路長の合計*4の逆数を取ったものを「近接中心性」
  #(closeness centrality)と呼びます。
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_closeness_centrality.pdf")) #Open PDF
  closeness_centrality_c = c()
  g.cls<-closeness(g,mode = 'out')
  plot(g,vertex.size=g.cls*20000, main='Closeness Centrality', edge.arrow.size=0.3, vertex.label.cex=0.1, vertex.color="blue", layout=layout_with_kk)
  closeness_centrality_c[i] = var(g.cls)
  closeness_centrality_c
  dev.off() #Save PDF
  
  #「離心中心性」(eccentricity centrality):グラフ全体を最短経路長を長さの単位とする
  #円（球）*2とみなした上で、その「中心」からの距離*3の逆数を取ったもの。端的に
  #言えば、グラフのど真ん中に近ければ近いほど大きくなる。
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_eccentricity_centrality.pdf")) #Open PDF
  eccentricity_centrality<-closeness(g,mode = "in") #in, all, out, total
  plot(g, vertex.size=eccentricity_centrality*20000, main='Eccentricity Centrality', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
  eccentricity_centrality_c[i] = var(eccentricity_centrality)
  eccentricity_centrality_c
  dev.off() #Save PDF
  
  #Centralization
  #固有ベクトル中心性:他の次数中心性の高いノード「とつながっている」ノードを評価するような指標
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_centralization.pdf")) #Open PDF
  g.evcent<-evcent(g,directed = T)
  plot(g,vertex.size=g.evcent$vector*15, main='Centralization', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
  centralization_c[i] = var(str_subset(unlist(g.evcent), "\\d+"))
  centralization_c
  dev.off() #Save PDF
  
  #PageRank:「リンク（流入）」を重視するようにした上で、さらに分離グラフや強連結でない
  #有向グラフにも適用できるようにしたのが、Sergey BrinとLawrence "Larry" Pageの2人
  #が考案してGoogle検索の基になったPageRank
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_page_rank.pdf")) #Open PDF
  gd.pr<-page.rank(g,directed=T)
  plot(g,vertex.size=gd.pr$vector*300, main='Page Pank', edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
  page_rank_c[i] = var(str_subset(unlist(gd.pr), "\\d+"))
  page_rank_c
  dev.off()
  
  #Bonapower:中心性の高いノードとつながるノードの中心性が低くなる
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_bonpow.pdf"))
  g.bp<-bonpow(g,exponent=0.2)
  plot(g,vertex.size=g.bp*5,main='Bona Power',edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
  bonpow_c[i] = var(g.bp)
  dev.off()
  
  #Betweenness:あるノードが他のノード間の最短経路上に位置する「程度」を中心性指標とし
  #たものが媒介中心性(betweeness centrality)
  pdf(str_c(wdPath, "/pdf/", target_c[i], "_betweenness.pdf"))
  g.bw<-betweenness(g,directed = T)
  plot(g,vertex.size=g.bw*0.0025,main='Betweeness',edge.arrow.size=0.3, vertex.color="blue",vertex.label.cex=0.1, layout=layout_with_kk)
  betweenness_c[i] = var(g.bw)
  betweenness_c
  dev.off()
}

#Store each vector data into the final DataFrame object.
resultDf = data.frame(target_c, targetFQCN_c,teaching_c, step_c, ccna_c, idau_c, edge_dencity_c, graph_degree_c, eccentricity_centrality_c, centralization_c, page_rank_c, bonpow_c, betweenness_c)
resultDf

#Merge between the resultDf and the NumOfCommit dataframe by FQCN.
length_resultDf = nrow(resultDf)
length_NumOfCommit = nrow(NumOfCommit)
x = 1
for(x in 1:length_resultDf){
  y = 1
  row_resultDf = resultDf[x,2]
  for(y in 1:length_NumOfCommit){
    row_NumOfCommit = NumofCommit[y,2]
    if(row_resultDf == row_NumOfCommit){
cat("true")
      resultDf[x,3] = NumOfCommit[y,1]
    }
  }
}

#Export resultDf as a csv file.
write.csv(x=resultDf, file = "/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf.csv")


####Analysis####
##AIC Liner Regression forward by RandamData
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf.csv", header=T, s=",")
result_tmp = result.d[1:85,4:14]
step(lm(teaching_c~1.,data=result_tmp), direction="forward",
     scope = ~step_c + idau_c + edge_dencity_c + graph_degree_c + eccentricity_centrality_c + centralization_c + page_rank_c + bonpow_c + betweenness_c)

##AIC Liner Regression backward by RandamData
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf.csv", header=T, s=",")
result_tmp = result.d[1:85,4:14]
step(lm(teaching_c~.,data=result_tmp), direction="backward")

##AIC Binomial Logistic Regression forward Analysis
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf_binominal.csv", header=T, s=",")
result_tmp = result.d[,4:14]
result_aic_forward = step(glm(teaching_c~1, data=result_tmp, family=binomial), direction="forward", 
     scope = ~step_c + idau_c + edge_dencity_c + graph_degree_c + eccentricity_centrality_c + centralization_c + page_rank_c + bonpow_c + betweenness_c)
plot(result_tmp[,1],xlim=c(1,118), ylim=c(0,1), ylab = "", col="red", pch = 8, main="")
par(new=T)
plot(fitted(result_aic_forward),xlim=c(1,118), ylim=c(0,1), ylab="T/F", col="blue", pch = 1, main="Prediction by Logistic Regression & AIC forkword")

##Used Logic for InSTA2022##
##AIC Binomial Logistic Regression backward Analysis, Tomcat AIC Backward 5 metrics model.
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf_binominal.csv", header=T, s=",")
result_tmp = result.d[,4:14]
result_aic_backward_tomcat = step(glm(teaching_c~. , data=result_tmp, family=binomial), direction="backward")
plot(result_tmp[,1],xlim=c(1,118), ylim=c(0,1), ylab = "", col="red", pch=8, main = "")
par(new=T)
plot(fitted(result_aic_backward_tomcat),xlim=c(1,118), ylim=c(0,1), ylab="T/F", col="blue", pch=1, main="Prediction by Logistic Regression & AIC Backword Tomcat")

##AIC Binomial Logistic Regression backward Analysis, Tomcat vs Traditional metrics model.
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf_binominal.csv", header=T, s=",")
result_tmp = result.d[,4:6]
result_traditional_backward = glm(teaching_c~. , data=result_tmp, family=binomial)
plot(result_tmp[,1],xlim=c(1,118), ylim=c(0,1), ylab = "", col="red", pch=8, main = "")
par(new=T)
plot(fitted(result_traditional_backward),xlim=c(1,118), ylim=c(0,1), ylab="T/F", col="blue", pch=1, main="Prediction by Logistic Regression & Traditional Metrics Tomcat")

##BestGLM
##AIC
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf_binominal_bic.csv", header=T, s=",")
result_aic = bestglm(result.d, family=binomial(link = "logit"), IC="AIC")
plot(fitted((result_aic)), xlim=c(1,118), ylim=c(0,1), ylab="T/F")

##BIC
result.d = read.csv("/Users/amazon/app/01.eclipseWorkspaces/luna_soot/soot/tmp/igraph/csv/result/resultDf_binominal_bic.csv", header=T, s=",")
result_bic = bestglm(result.d, family=binomial(link = "logit"), IC="BIC")




ROC = function(score, actual, add=FALSE,
               col="black", col.area="", type="l", pch=16) {
  o = order(score, decreasing=TRUE)
  fp = tp = fp_prev = tp_prev = 0
  nF = sum(actual == FALSE)
  nT = sum(actual == TRUE)
  score_prev = -Inf
  ber_min = Inf
  area = 0
  rx = ry = numeric(length(o))
  n = 0
  for (i in seq_along(o)) {
    j = o[i]
    if (score[j] != score_prev) {
      area = area + (fp - fp_prev) * (tp + tp_prev) / 2
      n = n + 1
      rx[n] = fp/nF
      ry[n] = tp/nT
      ber = (fp/nF + 1 - tp/nT)/2
      if (ber < ber_min) {
        ber_min = ber
        th = score_prev
        rx_best = fp/nF
        ry_best = tp/nT
      }
      score_prev = score[j]
      fp_prev = fp
      tp_prev = tp
    }
    if (actual[j] == TRUE) {
      tp = tp + 1
    } else {
      fp = fp + 1
    }
  }
  area = area + (fp - fp_prev) * (tp + tp_prev) / 2
  n = n + 1
  rx[n] = fp/nF  # = 1
  ry[n] = tp/nT  # = 1
  if (!add) {
    plot(NULL, xlim=c(0,1), ylim=c(0,1), asp=1,
         xlab="False Positive", ylab="True Positive",
         xaxs="i", yaxs="i")
    abline(h=(1:9)/10, v=(1:9)/10, col=gray(0.9))
    abline(0, 1, col=gray(0.4))
    abline(h=0:1, v=0:1)
  }
  t = (rx_best + ry_best)/2
  abline(ry_best-rx_best, 1, col=gray(0.8))
  lines(c(rx_best, t), c(ry_best, t), col=gray(0.8))
  if (col.area != "") {
    polygon(c(rx[1:n],1), c(ry[1:n],0), col=col.area)
  }
  lines(rx[1:n], ry[1:n], type=type, lwd=2, col=col, pch=pch, xpd=TRUE)
  cat("AUC =", area/(nF*nT), "th =", th, "\n")
  cat("BER =", (rx_best + (1-ry_best))/2,
      "OR =", (ry_best/(1-ry_best))/(rx_best/(1-rx_best)), "\n")
  print(table(score >= th, actual, dnn=c("Predicted","Actual")))
  invisible(list(rx=rx, ry=ry, AUC=area/(nF*nT), th=th))
}
