library(igraph) 
library(network) 
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(readr)
library(data.table)
library(dplyr)
library(stringr)
setwd("D:/Dropbox/crypto networks (1)/codes/v3")

################### Network Timeseries Part ###################

full_data = data.table(read_csv("Crypto_Matched_Entity_Result.csv"))
full_data$full_name = full_data$name
full_data$ticker = full_data$symbol
frequence_data = full_data %>%
  group_by(ticker) %>%
  summarise(n = n())
l = length(frequence_data$ticker)
frequence_data$stem_id = sample(1:l, l, replace=F)
full_data$stem_id = frequence_data$stem_id[match(full_data$ticker,frequence_data$ticker)]

#full_data$date = str_sub(full_data$id,1,8)
full_data$date = as.Date(full_data$date, format = "%m/%d/%Y")
full_data$week = as.Date(format(full_data$date,"%Y/%W/1"),"%Y/%W/%u")
full_data$month = format(full_data$date, "%Y-%m")
full_data$year = format(full_data$date, "%Y")

t1 = Sys.time()
links = data.frame(id = NA)
links$sid = NA
links$a_tic = NA
links$b_tic = NA
links$a_fullname = NA
links$b_fullname = NA
links$strength = NA
links$week = full_data$week[1]
links$month = full_data$month[1]
links$year = full_data$year[1]
id = unique(full_data$doc_id)
count = 1
pb = txtProgressBar(min = 0, max = length(id), initial = 0, style = 3, file = "")
for (i in id){
  temp = full_data[which(full_data$doc_id==i),]
  l = length(temp$sid)-1
  if (l>=1) {
    for (j in 1:l){
      for (m in (j+1):(l+1)){
        gap = temp$sid[m]-temp$sid[j]
        if (gap<=0&&(temp$ticker[m]!=temp$ticker[j])){
          if (temp$stem_id[m]>temp$stem_id[j]){
            Atic = temp$ticker[m]
            Btic = temp$ticker[j]
            Acomp = temp$full_name[m]
            Bcomp = temp$full_name[j]
            links[count,c("id","sid","a_tic","b_tic","a_fullname","b_fullname","strength")] =
              c(i,temp$sid[j],Atic,Btic,Acomp,Bcomp,1/(2^(gap)))
            links$week[count] = (temp$week[m])
            links$month[count] = (temp$month[m])
            links$year[count] = (temp$year[m])
            count = count+1
          }else{
            Atic = temp$ticker[j]
            Btic = temp$ticker[m]
            Acomp = temp$full_name[j]
            Bcomp = temp$full_name[m]
            links[count,c("id","sid","a_tic","b_tic","a_fullname","b_fullname","strength")] =
              c(i,temp$sid[j],Atic,Btic,Acomp,Bcomp,1/(2^(gap)))
            links$week[count] = (temp$week[m])
            links$month[count] = (temp$month[m])
            links$year[count] = (temp$year[m])
            count = count+1
          }
        }
      }
    }
  }
  setTxtProgressBar(pb, count)
}
t2 = Sys.time()
t2-t1
links$strength = as.numeric(links$strength)
links[,c('a_tic','b_tic','a_fullname','b_fullname')] = apply(links[,c('a_tic','b_tic','a_fullname','b_fullname')],2,
                                                              gsub,pattern = "_",replacement = "")
length(unique(c(links$a_fullname, links$b_fullname)))

test = filter(links, year == '2018')

write_csv(links,'Crypto_RandOrder_Link_v6(full).csv')

network = read_csv('Crypto_RandOrder_Link_v6(full).csv')
network$idsid = paste0(network$id,network$sid)
filter_sentence = network %>% group_by(idsid) %>% summarise(n = sum(strength))
filter_sentence = data.frame('idsid' = filter_sentence$idsid[filter_sentence$n>2])$idsid
network = filter(network, !(idsid %in% filter_sentence))
write_csv(network, "Crypto_RandOrder_Link_v6.csv")

### I filter out price quotes, see textual analysis ###
network = read_csv('Crypto_RandOrder_Link_v6.csv')
network$idsid = paste(network$id, network$sid, sep = '_')
labels = read_csv('textual analysis/all_sentences_labeled_2021_01_20.csv')
labels_c = filter(labels, Label == 'C')
labels_nc = filter(labels, Label == 'NC')
network_c = filter(network, idsid %in% labels_c$doc_id_sid)
network_nc = filter(network, idsid %in% labels_nc$doc_id_sid)
write_csv(network_c, "Crypto_RandOrder_Link_v6_competition.csv")
write_csv(network_nc, "Crypto_RandOrder_Link_v6_noncompetition.csv")

################### Adjacent Matrix Part ###################

nw = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
nw$date = str_sub(nw$id,1,8)
nw$date = as.Date(nw$date, format = "%Y%m%d")

#monthly#

months = unique(nw$month)
nw_temp = data.table(nw[,c("a_tic","b_tic","a_fullname","b_fullname","strength")])
#colnames(nw_temp) = c("company_a", "company_b", "strength")
w = data.table(nw_temp)[,sum(strength),by = c("a_tic","b_tic","a_fullname","b_fullname")]
w = w[order(w$V1,decreasing = TRUE),]
adj = data.frame(matrix(ncol = length(months)+4, nrow = dim(w)[1]))
x = c("company_a", "company_b","company_atic", "company_btic", as.character(months))
colnames(adj) = x
adj[,c("company_a", "company_b","company_atic", "company_btic")] = 
  w[,c("a_fullname","b_fullname","a_tic","b_tic")]

adj[is.na(adj)] = 0

combo_adj = paste0(adj$company_a,adj$company_b)
for (j in 1:length(months)){
  nw_temp = filter(nw,month==months[j])
  nw_temp$combo = paste0(nw_temp$a_fullname,nw_temp$b_fullname)
  for (i in 1:length(combo_adj)){
    if (combo_adj[i] %in% nw_temp$combo){
      adj[i,j+4] = sum(nw_temp$strength[which(nw_temp$combo==combo_adj[i])])
    }
  }
}

write_csv(adj,"Crypto_Adjacency_Unnormal_Monthly_v6_competition.csv")
copy = adj[,5:ncol(adj)]
copy[copy>0] = 1
adj[,5:ncol(adj)] = copy
write_csv(adj,"Crypto_Adjacency_Normal_Monthly_v6_competition.csv")

#weekly#
nw = filter(nw, date >= as.Date("2017-10-04", format = "%Y-%m-%d") &
              date <= as.Date("2020-11-30", format = "%Y-%m-%d"))

weeks = seq.Date(from=as.Date('2017-10-04'), to=as.Date('2020-12-01'), by='weeks')
for (i in 1:length(weeks)) {
  if (i < length(weeks)){
    nw$week[which(nw$date>=weeks[i] & nw$date < weeks[i+1])] = weeks[i]
  }else{
    nw$week[which(nw$date>=weeks[i])] = weeks[i]
  }
}

weeks = unique(nw$week)
nw_temp = data.table(nw[,c("a_tic","b_tic","a_fullname","b_fullname","strength")])
#colnames(nw_temp) = c("company_a", "company_b", "strength")
w = data.table(nw_temp)[,sum(strength),by = c("a_tic","b_tic","a_fullname","b_fullname")]
w = w[order(w$V1,decreasing = TRUE),]
adj = data.frame(matrix(ncol = length(weeks)+4, nrow = dim(w)[1]))
x = c("company_a", "company_b","company_atic", "company_btic", as.character(weeks))
colnames(adj) = x
adj[,c("company_a", "company_b","company_atic", "company_btic")] = 
  w[,c("a_fullname","b_fullname","a_tic","b_tic")]

adj[is.na(adj)] = 0

combo_adj = paste0(adj$company_a,adj$company_b)
for (j in 1:length(weeks)){
  nw_temp = filter(nw,week==weeks[j])
  nw_temp$combo = paste0(nw_temp$a_fullname,nw_temp$b_fullname)
  for (i in 1:length(combo_adj)){
    if (combo_adj[i] %in% nw_temp$combo){
      adj[i,j+4] = sum(nw_temp$strength[which(nw_temp$combo==combo_adj[i])])
    }
  }
}

write_csv(adj,"Crypto_Adjacency_Unnormal_Weekly_v6_competition.csv")
copy = adj[,5:ncol(adj)]
copy[copy>0] = 1
adj[,5:ncol(adj)] = copy
write_csv(adj,"Crypto_Adjacency_Normal_Weekly_v6_competition.csv")

# daily #
nw = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
nw$date = str_sub(nw$id,1,8)
nw$date = as.Date(nw$date, format = "%Y%m%d")
dates = unique(nw$date)
nw_temp = data.table(nw[,c("a_tic","b_tic","a_fullname","b_fullname","strength")])
w = data.table(nw_temp)[,sum(strength),by = c("a_tic","b_tic","a_fullname","b_fullname")]
w = w[order(w$V1,decreasing = TRUE),]
adj = data.frame(matrix(ncol = length(dates)+4, nrow = dim(w)[1]))
x = c("company_a", "company_b","company_atic", "company_btic", as.character(dates))
colnames(adj) = x
adj[,c("company_a", "company_b","company_atic", "company_btic")] = 
  w[,c("a_fullname","b_fullname","a_tic","b_tic")]

adj[is.na(adj)] = 0

combo_adj = paste0(adj$company_a,adj$company_b)
for (j in 1:length(dates)){
  nw_temp = filter(nw,date==dates[j])
  nw_temp$combo = paste0(nw_temp$a_fullname,nw_temp$b_fullname)
  for (i in 1:length(combo_adj)){
    if (combo_adj[i] %in% nw_temp$combo){
      adj[i,j+4] = sum(nw_temp$strength[which(nw_temp$combo==combo_adj[i])])
    }
  }
}

write_csv(adj,"Crypto_Adjacency_Unnormal_daily_v6_competition.csv")
copy = adj[,5:ncol(adj)]
copy[copy>0] = 1
adj[,5:ncol(adj)] = copy
write_csv(adj,"Crypto_Adjacency_Normal_daily_v6_competition.csv")



################### Network Plotting Part ###################
TopNodes1 = function(nw){
  nw = data.table(nw)
  temp = nw[,sum(strength),by = c("company_a","company_b")]
  temp = temp[order(temp$V1,decreasing = TRUE),]
  temp$type = 1
  temp = temp[,c("company_a","company_b","type","V1")]
  return(data.frame(temp))
}
TopNodes2 = function(nw,tops){
  nw = data.table(nw)
  temp = nw[,sum(strength),by = c("company_a","company_b")]
  temp = temp[order(temp$V1,decreasing = TRUE),]
  temp1 = temp[,c(1,3)]
  colnames(temp1) = c("company","V1")
  temp2 = temp[,c(2,3)]
  colnames(temp2) = c("company","V1")
  temp3 = rbind(temp1,temp2)
  temp3 = temp3[,sum(V1),by = company]
  temp3 = temp3[order(temp3$V1,decreasing = TRUE),]
  names = temp3[c(1:tops),1]
  temp$include = 0
  for (i in 1:length(temp$include)){
    if (temp$company_a[i] %in% names$company && temp$company_b[i] %in% names$company){
      temp$include[i]=1
    }
  }
  temp4 = data.table(filter(temp,include==1)[,c(1,2,3)])
  temp4$type = 1
  temp4 = temp4[,c("company_a","company_b","type","V1")]
  return(data.frame(temp4))
}
TopNodes_size = function(nw,V){
  temp1 = nw[,c(1,4)]
  colnames(temp1) = c("company","V1")
  temp2 = nw[,c(2,4)]
  colnames(temp2) = c("company","V1")
  temp3 = data.table(rbind(temp1,temp2))
  temp3 = temp3[,sum(V1),by = 'company']
  temp3 = temp3[order(temp3$V1,decreasing = TRUE),]
  size = c(1:length(V))
  for (i in 1:length(V)){
    size[i] = temp3$V1[which(temp3$company==V[i])]
  }
  return(size)
}
TopNodes_size_2 = function(mention,V){
  temp = mention[,'name']
  temp$count = 1
  temp = data.table(temp)
  temp = temp[,sum(count),by = 'name']
  size = c(1:length(V))
  for (i in 1:length(V)){
    size[i] = temp$V1[which(temp$name==V[i])]
  }
  return(size)
}
nw_full = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
nw_full$year = as.numeric(str_sub(nw_full$id,1,4))
nw_full$month = as.numeric(str_sub(nw_full$id,1,6))
mention = read_csv("Crypto_Matched_Entity_Result.csv")
#nw_full = filter(nw_full, a_tic %in% merged$X1 & b_tic %in% merged$X1)
## yearly
nw_full = nw_full[,c('a_tic','b_tic','strength','year')]
colnames(nw_full) = c('company_a','company_b','strength','year')
nw_2017 = filter(nw_full, year == 2017)
nw_2018 = filter(nw_full, year == 2018)
nw_2019 = filter(nw_full, year == 2019)

nw = nw_full
#temp = TopNodes2(nw,50)
temp = TopNodes1(nw)
net <- graph_from_data_frame(d=temp,directed=F)
V(net)$size = log(TopNodes_size(temp,V(net)$name)*2)/1.5
E(net)$width = log(temp$V1+1)/6
pdf('plots/competition/Competition_top100_2017-2020.pdf')
plot(net,layout = layout_nicely,vertex.label.cex= 0.5,vertex.frame.color="white")
#plot(net,vertex.label.cex=0.7,vertex.frame.color="white")
dev.off()
plot(net,vertex.label.cex=0.7)
plot(net,layout = layout_with_kk,vertex.label.cex= 0.7,vertex.frame.color="white")

# nw = nw_2017
# temp = TopNodes(nw)
# net <- graph_from_data_frame(d=temp,directed=F)
# V(net)$size = TopNodes_size(temp,V(net)$name)/10
# E(net)$width = log(temp$V1+1)
# plot(net,layout = layout_nicely,vertex.label.cex= 1,vertex.frame.color="white")
# plot(net,vertex.label.cex=1)
# plot(net,layout = layout_with_kk,vertex.label.cex=1,vertex.frame.color="white")
# 
# nw = nw_2018
# temp = TopNodes(nw)
# net <- graph_from_data_frame(d=temp,directed=F)
# V(net)$size = TopNodes_size(temp,V(net)$name)/60
# E(net)$width = log(temp$V1+1)/2
# plot(net,layout = layout_nicely,vertex.label.cex= 1,vertex.frame.color="white")
# plot(net,layout = layout_with_kk,vertex.label.cex=1,vertex.frame.color="white")
# 
# nw = nw_2019
# temp = TopNodes(nw)
# net <- graph_from_data_frame(d=temp,directed=F)
# V(net)$size = TopNodes_size(temp,V(net)$name)/200
# E(net)$width = log(temp$V1+1)/2
# plot(net,layout = layout_nicely,vertex.label.cex= 1,vertex.frame.color="white")
# plot(net,vertex.label.cex= 1,vertex.frame.color="white")
# plot(net,layout = layout_with_kk,vertex.label.cex=1,vertex.frame.color="white")

##### monthly #####
#merged = read_csv("all_merged_cryptos.txt", col_names = F)
#merged$X1 = tolower(merged$X1)
nw_full = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
nw_full$year = as.numeric(str_sub(nw_full$id,1,4))
nw_full$month = as.numeric(str_sub(nw_full$id,1,6))
#nw_full = filter(nw_full, a_tic %in% merged$X1 & b_tic %in% merged$X1)

nw_full = nw_full[,c('a_tic','b_tic','strength','month')]
colnames(nw_full) = c('company_a','company_b','strength','month')
months = unique(nw_full$month)
for (i in months){
  nw_temp = data.table(filter(nw_full, month == i))
  nw_temp$month = NULL
  nw_temp = nw_temp[,sum(strength),by = c("company_a","company_b")]
  nw_temp$type = 1
  nw_temp = nw_temp[,c("company_a","company_b","type","V1")]
  net = graph_from_data_frame(d=nw_temp,directed=F)
  V(net)$size = log(TopNodes_size(nw_temp,V(net)$name)+1)*2
  E(net)$width = log(nw_temp$V1+1)+2
  path = paste0("~/Dropbox/crypto networks (1)/codes/plots/monthly/",i,".png")
  png(filename=path,width = 1200, height = 1200)
  plot(net,layout = layout_nicely,vertex.label.cex=1,vertex.frame.color="white")
  dev.off()
}

##### quarterly #####
m2q = function(string){
  temp = str_sub(string,6,7)
  temp2 = str_sub(string,1,4)
  if (temp=='01'|temp=='02'|temp=='03'){
    string = paste0(temp2,'Q1')
  }else if (temp=='04'|temp=='05'|temp=='06'){
    string = paste0(temp2,'Q2')
  }else if (temp=='07'|temp=='08'|temp=='09'){
    string = paste0(temp2,'Q3')
  }else if (temp=='10'|temp=='11'|temp=='12'){
    string = paste0(temp2,'Q4')
  }
}
nw_full = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
nw_full = filter(nw_full, year>2017)
nw_full = nw_full[,c('a_tic','b_tic','strength','month')]
colnames(nw_full) = c('company_a','company_b','strength','quarterly')
nw_full$quarterly = sapply(nw_full$quarterly,m2q,USE.NAMES = FALSE)
quarterly = unique(nw_full$quarterly)
for (i in quarterly){
  nw_temp = data.table(filter(nw_full, quarterly == i))
  nw_temp$quarterly = NULL
  nw_temp = TopNodes2(nw_temp,50)
  net = graph_from_data_frame(d=nw_temp,directed=F)
  V(net)$size = log(TopNodes_size(nw_temp,V(net)$name)+1)*1.5
  E(net)$width = log(nw_temp$V1+1)/1.5
  path = paste0("plots/competition/",i,".pdf")
  pdf(path)
  plot(net,layout = layout_nicely,vertex.label.cex=0.7,vertex.frame.color="white")
  dev.off()
}

########## calculate characteristics time series ############
library(CINNA)

# monthly
adj = read_csv('Crypto_Adjacency_Unnormal_Monthly_v6_competition.csv')
mentions = read_csv("Crypto_Matched_Entity_Result.csv")
mentions$month = format(mentions$date, "%Y-%m")
name_table = data.frame(name = c(adj$company_a,adj$company_b))
name_table$tic = c(adj$company_atic, adj$company_btic)
name_table = data.table(name_table)
keycols = c('name','tic')
setkeyv(name_table, keycols)
name_table = unique(name_table)

skip = 4
months = colnames(adj)[(skip+1):ncol(adj)]

cha_mention = name_table
cha_mention[,months] = 0
cha_connections = cha_mention
cha_first = cha_mention
cha_second = cha_mention
cha_eigen = cha_mention
cha_harm = cha_mention
for (i in 1:length(months)){
  #mention
  temp = filter(mentions, month == months[i])['name']
  temp$count = 1
  temp = data.table(temp)
  temp = temp[,sum(count),by = "name"]
  #temp = filter(temp, naics %in% importance_zero$naics)
  p = match(temp$name, cha_mention$name)
  cha_mention[p,(i+2)] = temp$V1
  
  temp = adj[,c(1,2,i+skip)]
  colnames(temp)[3] = "strength"
  temp = filter(temp, strength!=0)
  tempw = as_adjacency_matrix(graph.data.frame(temp,directed=FALSE),
                              type="both",names=TRUE,sparse=FALSE,attr="strength")
  G = graph_from_adjacency_matrix(tempw, mode = c("undirected"))
  temp_imp = data.frame(name = colnames(tempw))
  
  links_here = rowSums(tempw)
  eigen_here = eigen_centrality(G, directed = FALSE, scale = TRUE)$vector
  harm_here = harmonic_centrality(G, vids = V(G))
  tempw = tempw/rowSums(tempw)
  first_here = colSums(tempw)
  second_here = c(colSums(tempw)%*%tempw)
  
  p = match(temp_imp$name, cha_connections$name)
  cha_connections[p,(i+2)] = links_here
  cha_first[p,(i+2)] = first_here
  cha_second[p,(i+2)] = second_here
  cha_eigen[p,(i+2)] = eigen_here
  cha_harm[p,(i+2)] = harm_here
}

write_csv(cha_mention, "Characteristic_mentions_monthly_v6_competition.csv")
write_csv(cha_connections, "Characteristic_connections_monthly_v6_competition.csv")
write_csv(cha_first, "Characteristic_scaledfirstorder_monthly_v6_competition.csv")
write_csv(cha_second, "Characteristic_scaledsecondorder_monthly_v6_competition.csv")
write_csv(cha_eigen, "Characteristic_eigen_monthly_v6_competition.csv")
write_csv(cha_harm, "Characteristic_harmonic_monthly_v6_competition.csv")

# weekly
adj = read_csv('Crypto_Adjacency_Unnormal_Weekly_v6_competition.csv')
mentions = read_csv("Crypto_Matched_Entity_Result.csv")
mentions = filter(mentions, date >= as.Date("2017-10-04", format = "%Y-%m-%d") &
              date <= as.Date("2020-11-30", format = "%Y-%m-%d"))
mentions$week = mentions$date
weeks = seq.Date(from=as.Date('2017-10-04'), to=as.Date('2020-12-01'), by='weeks')
for (i in 1:length(weeks)) {
  if (i < length(weeks)){
    mentions$week[which(mentions$date>=weeks[i] & mentions$date < weeks[i+1])] = weeks[i]
  }else{
    mentions$week[which(mentions$date>=weeks[i])] = weeks[i]
  }
}

name_table = data.frame(name = c(adj$company_a,adj$company_b))
name_table$tic = c(adj$company_atic, adj$company_btic)
name_table = data.table(name_table)
keycols = c('name','tic')
setkeyv(name_table, keycols)
name_table = unique(name_table)

skip = 4
weeks = colnames(adj)[(skip+1):ncol(adj)]

cha_mention = name_table
cha_mention[,weeks] = 0
cha_connections = cha_mention
cha_first = cha_mention
cha_second = cha_mention
cha_eigen = cha_mention
cha_harm = cha_mention
for (i in 1:length(weeks)){
  #mention
  temp = filter(mentions, week == weeks[i])['name']
  temp$count = 1
  temp = data.table(temp)
  temp = temp[,sum(count),by = "name"]
  #temp = filter(temp, naics %in% importance_zero$naics)
  p = match(temp$name, cha_mention$name)
  cha_mention[p,(i+2)] = temp$V1
  
  temp = adj[,c(1,2,i+skip)]
  colnames(temp)[3] = "strength"
  temp = filter(temp, strength!=0)
  tempw = as_adjacency_matrix(graph.data.frame(temp,directed=FALSE),
                              type="both",names=TRUE,sparse=FALSE,attr="strength")
  G = graph_from_adjacency_matrix(tempw, mode = c("undirected"))
  temp_imp = data.frame(name = colnames(tempw))
  
  links_here = rowSums(tempw)
  eigen_here = eigen_centrality(G, directed = FALSE, scale = TRUE)$vector
  harm_here = harmonic_centrality(G, vids = V(G))
  tempw = tempw/rowSums(tempw)
  first_here = colSums(tempw)
  second_here = c(colSums(tempw)%*%tempw)
  
  p = match(temp_imp$name, cha_connections$name)
  cha_connections[p,(i+2)] = links_here
  cha_first[p,(i+2)] = first_here
  cha_second[p,(i+2)] = second_here
  cha_eigen[p,(i+2)] = eigen_here
  cha_harm[p,(i+2)] = harm_here
}

write_csv(cha_mention, "Characteristic_mentions_weekly_v6_competition.csv")
write_csv(cha_connections, "Characteristic_connections_weekly_v6_competition.csv")
write_csv(cha_first, "Characteristic_scaledfirstorder_weekly_v6_competition.csv")
write_csv(cha_second, "Characteristic_scaledsecondorder_weekly_v6_competition.csv")
write_csv(cha_eigen, "Characteristic_eigen_weekly_v6_competition.csv")
write_csv(cha_harm, "Characteristic_harmonic_weekly_v6_competition.csv")

#whole period
adj = read_csv('Crypto_Adjacency_Unnormal_Monthly_v6_competition.csv')
mentions = read_csv("Crypto_Matched_Entity_Result.csv")
network = read_csv("Crypto_RandOrder_Link_v6_competition.csv")
chr = data.frame(name = mentions$name)
chr$tic = mentions$symbol
chr = data.table(chr)
keycols = c('name','tic')
setkeyv(chr, keycols)
chr = unique(chr)
chr$mentions = 0
chr$connections = 0
chr$first_order = 0
chr$second_order = 0
chr$eigen = 0
chr$harm = 0
#chr = data.frame(chr)

temp = mentions
temp$count = 1
temp = data.table(temp)
temp = temp[,sum(count),by = "name"]
p = match(temp$name, chr$name)
chr[p,c('mentions')] = temp$V1

temp = data.table(network[,c("a_fullname","b_fullname","strength")])
temp = temp[,sum(strength),by = c("a_fullname","b_fullname")]
tempw = as_adjacency_matrix(graph.data.frame(temp,directed=FALSE),
                            type="both",names=TRUE,sparse=FALSE,attr="V1")
G = graph_from_adjacency_matrix(tempw, mode = c("undirected"))
temp_imp = data.frame(name = colnames(tempw))

links_here = rowSums(tempw)
eigen_here = eigen_centrality(G, directed = FALSE, scale = TRUE)$vector
harm_here = harmonic_centrality(G, vids = V(G))
tempw = tempw/rowSums(tempw)
first_here = colSums(tempw)
second_here = c(colSums(tempw)%*%tempw)
p = match(temp_imp$name, chr$name)
chr$connections[p] = links_here
chr$first_order[p] = first_here
chr$second_order[p] = second_here
chr$eigen[p] = eigen_here
chr$harm[p] = harm_here

write_csv(chr, "Characteristic_whole_period_v6_competition.csv")

# daily
adj = read_csv('Crypto_Adjacency_Unnormal_Daily_v6.csv')
mentions = read_csv("Crypto_Matched_Entity_Result.csv")
mentions$date = format(mentions$date, "%Y-%m-%d")
name_table = data.frame(name = c(adj$company_a,adj$company_b))
name_table$tic = c(adj$company_atic, adj$company_btic)
name_table = data.table(name_table)
keycols = c('name','tic')
setkeyv(name_table, keycols)
name_table = unique(name_table)

skip = 4
dates = colnames(adj)[(skip+1):ncol(adj)]

cha_mention = data.frame(matrix(0,ncol = length(dates)+dim(name_table)[2], nrow = dim(name_table)[1]))
cha_mention[,c(1,2)] = name_table
colnames(cha_mention) = c(colnames(name_table),dates)
cha_mention = data.table(cha_mention)

for (i in 1:length(dates)){
  #mention
  temp = filter(mentions, date == dates[i])['name']
  temp$count = 1
  temp = data.table(temp)
  temp = temp[,sum(count),by = "name"]
  #temp = filter(temp, naics %in% importance_zero$naics)
  p = match(temp$name, cha_mention$name) #in data table, position 'NA' is ignored by default
  cha_mention[p,(i+2)] = temp$V1
  
  # temp = adj[,c(1,2,i+skip)]
  # colnames(temp)[3] = "strength"
  # temp = filter(temp, strength!=0)
  # tempw = as_adjacency_matrix(graph.data.frame(temp,directed=FALSE),
  #                             type="both",names=TRUE,sparse=FALSE,attr="strength")
  # G = graph_from_adjacency_matrix(tempw, mode = c("undirected"))
  # temp_imp = data.frame(name = colnames(tempw))
  # 
  # links_here = rowSums(tempw)
  # eigen_here = eigen_centrality(G, directed = FALSE, scale = TRUE)$vector
  # harm_here = harmonic_centrality(G, vids = V(G))
  # tempw = tempw/rowSums(tempw)
  # first_here = colSums(tempw)
  # second_here = c(colSums(tempw)%*%tempw)
  # 
  # p = match(temp_imp$name, cha_connections$name)
  # cha_connections[p,(i+2)] = links_here
  # cha_first[p,(i+2)] = first_here
  # cha_second[p,(i+2)] = second_here
  # cha_eigen[p,(i+2)] = eigen_here
  # cha_harm[p,(i+2)] = harm_here
}

write_csv(cha_mention, "Characteristic_mentions_daily_v6.csv")
