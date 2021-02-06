Sys.setenv(JAVA_HOME="C://Program Files/Java/jdk1.8.0_191/") #change this to where you install Java's jdk
options(java.parameters = "-Xmx13312m") #maximum memory you allow R to use for Java
options(java.parameters = "-Xms512m") #minimum memory you allow R to use for Java

library(rJava)
library(XML)
library(NLP)
library(RWeka)
library(readtext)
library(magrittr)
library(stringi)
library(coreNLP)
library(cleanNLP)
library(markdown)
library(dplyr)
library(stringr)
library(knitr)
library(readr)
library(data.table)

setwd("D://Dropbox/crypto networks (1)/codes/v3")
cnlp_init_corenlp(language = "en", anno_level = 0, mem = "4g")

# Load files
entity_full = read_csv("all_entities_v3.csv", progress = FALSE, col_types = cols())
entity_full$entity_normalized = NULL
org = read_csv("OrgList.csv")
keywords = read_csv("keywords.csv",col_names = FALSE)

# Start Cleaning
data = filter(entity_full) %>%
  group_by(entity) %>%
  summarise (weight = n())
data = data[order(data$weight,decreasing = TRUE),]
data = filter(data,weight>2)
data = data.table(data)

data[,entity_normalized := entity]
data$entity_normalized = tolower(data$entity_normalized)
data$entity_normalized = gsub("-", " ", data$entity_normalized)

#remove special symbol '.'
#data <- data[,entity_normalized := gsub('\\.com','',entity_normalized)]
data = data[!grepl('\\.com',entity_normalized),]
data <- data[,entity_normalized := gsub("\\.", " ", entity_normalized)] 
data <- data[,entity_normalized := gsub("[^[:alnum:][:blank:]&']",'',entity_normalized)]

#remove "& Co" "corp" "corporation" as suffixes
data <- data[,entity_normalized := paste0(entity_normalized,"_")]
data <- data[,entity_normalized := gsub(' & co_', '', entity_normalized)]
data <- data[,entity_normalized := gsub(' corporation_', '', entity_normalized)]
data <- data[,entity_normalized := gsub('_', '', entity_normalized)]

#tokenize entity names
data[, entity_id := c(1:length(entity))]
data_token = cnlp_annotate(data[,c("entity_id","entity_normalized")],as_string=TRUE,
                           doc_var = "entity_id",
                           text_var = "entity_normalized")$token
data_token$word = tolower(data_token$word)

#remove the tokens of business types
suffix = c("co", "co.", "inc", "inc.", "ag", "ag.", "ltd", "ltd.", "lp", "lp.", "llc", "llc.", "pllc", "pllc.", "llp", "llp.", "plc", "plc.", "ltd/plc", "ltd/plc.", "corp", "corp.", "ab", "ab.", "cos", "cos.", "cia", "cia.", "sa", "sa.", "based", "rrb", "the",
           "ico","icos","utc", "coin", "token", "coins", "tokens")
data_token = filter(data_token, !(word %in% suffix))

#remove other irrelevant noises
data_token = filter(data_token, upos != "." & upos !="")

#now paste the cleaned tokens as names accordingly
data_token <- data.table(data_token)
data_token_tmp <- data_token[, c("id","word")]
tmp <- data_token_tmp[, lapply(.SD, paste, collapse = "_"), by = id]
tmp[,word := paste0("_",word,"_") ]
setnames(tmp, "id","id_str")
setkey(tmp, "id_str")

data[, id_str := as.character(entity_id)]
setkey(data, "id_str")

data <- data[tmp]
data <- data[order(entity_id)]
data_token = c()
data_token_tmp = c()
tmp = c()

data[, entity_normalized := word]
data[, c("id_str", "word") := NULL]

#remove all the entities referring to non-firm organizations using keywords
for (i in 1:length(keywords$X1)){
  data = data[!grepl(keywords[i,1],entity_normalized),]
}

# MANUAL ADJUSTMENT #
data$entity_normalized = gsub("_&_", "&", data$entity_normalized)
data$entity_normalized = gsub("&", "_&_", data$entity_normalized)
data$entity_normalized = gsub("_'s_", "_", data$entity_normalized)

data <- data[,entity_normalized := gsub('_price_technical_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_price_watch_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_coin_technical_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_cash_price_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_technical_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_weekly_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_price_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_price_movement_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_tradingview_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_crypto_briefing_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_coinspeaker_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_ccn_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_bitcoin_news_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_charts_guides_&_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_newsbtc_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_bitcoin_magazine_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_us_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_federal_reserve_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_zero_knowledge_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_trump_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_chinese_yuan_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_ventures_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_venture_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_price_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_analysis_', '_', entity_normalized)]
data <- data[,entity_normalized := gsub('_wallet_', '_', entity_normalized)]
p = grep("_sec_", data$entity_normalized)
data = if (length(p) > 0) data[-p,] else data;
p = which(data$entity_normalized %in% c("_bank_", "_banks_"))
data = if (length(p) > 0) data[-p,] else data;

#data$entity_normalized[which(data$entity_normalized=="_facebook_libra_")] = "_libra_"
data$entity_normalized[which(data$entity_normalized=="_bitcoin_network_")] = "_bitcoin_"
data$entity_normalized = gsub("_calibra_", "_libra_", data$entity_normalized)
#data <- data[,entity_normalized := gsub('_facebook_libra_', '_libra_', entity_normalized)]
q = which(str_detect(data$entity_normalized, "_facebook_") | str_detect(data$entity_normalized, "_libra_"))
data$entity_normalized[q] = "_facebook_libra_"


# Remove empty names
p = which(data$entity_normalized == ""|data$entity_normalized == "_")
data = if (length(p) > 0) data[-p,] else data;

# Remove irrelevant entries
p = which(data$entity_normalized %in% c("_bank_", "_banks_", "_crypto_", "_cryptos_", "_cryptocurrency_", "_cryptocurrencies_", "_crypto_market_", "_crypto_markets_", "_group_",
                                        "_weekly_", "_office_", "_press_", "_research_", "_system_", "_treasuries_",
                                        "_agriculture_","_energy_","_bank_","_board_","_company_","_estimate_","_europe_","_globe_","_healthcare_","_real_estate_",
                                        "_transportation_","_united_states_","_usa_","_u_s_",'_blockchain_','_asset_',
                                        "_tech_","_fund_","_network_","_digital_"))
data = if (length(p) > 0) data[-p,] else data;

#merge entities that refer to the same company; again, we want to just deal with unique entity names
data_unique = data %>%
  group_by(entity_normalized) %>%
  summarise (n = sum(weight))
data_unique = data_unique[order(data_unique$n,decreasing = TRUE),]
data_unique = filter(data_unique, str_length(entity_normalized)>=4) #delete noises like '_x_'
data_unique$stem = NA

#when a entity shows more than 5 times and it is not a single gram, we believe it has potential to be the 'stem' name of a company,
#then we put it into 'stem candidate"
for (i in 1:length(data_unique$entity_normalized)){
  if (data_unique[i,]$n>=5){
    data_unique[i,]$stem = data_unique[i,]$entity_normalized
  }
}

non_na_id = which(!is.na(data_unique$stem) & str_count(data_unique$stem,'_')>2)
na_id = which(is.na(data_unique$stem) & str_count(data_unique$entity_normalized,'_')>2)

#merge within the 'stem candidate'; remember they're ranked by frequency
for (i in 1:(length(non_na_id)-1)){
  for (j in (i+1):length(non_na_id)){
    p = non_na_id[i]
    q = non_na_id[j]
    if((str_detect(data_unique$stem[p],data_unique$stem[q])|
        str_detect(data_unique$stem[q],data_unique$stem[p]))&(data_unique$stem[p]!=data_unique$stem[q])){
      data_unique$stem[q]=data_unique$stem[p]
    }
  }
}

#merge between the 'stem candidate' and non 'stem candidate'
data_unique = data.table(data_unique)
temp <- data_unique[, .(n=sum(n)), by=list(stem)]
temp <- temp[!is.na(stem)]
temp = temp[order(temp$n,decreasing = TRUE),]
temp_single = temp[which(str_count(temp$stem,'_')==2),]
temp_multi = temp[which(str_count(temp$stem,'_')>2),]

#first merge with multigram candidates, then merge with single gram candidates
for (i in na_id){
  for (j in 1:length(temp_multi$stem)){
    if(str_detect(data_unique$entity_normalized[i],temp_multi$stem[j])|
       str_detect(temp_multi$stem[j],data_unique$entity_normalized[i])){
      #print(c(data_unique$entity_normalized[i],temp$stem[j]))
      data_unique$stem[i] = temp_multi$stem[j]
      break
    }
  }
}

na_id = which(is.na(data_unique$stem) & str_count(data_unique$entity_normalized,'_')>2 &
                str_count(data_unique$entity_normalized,'_')<=3)

for (i in na_id){
  for (j in 1:length(temp_single$stem)){
    if(str_detect(data_unique$entity_normalized[i],temp_single$stem[j])|
       str_detect(temp_single$stem[j],data_unique$entity_normalized[i])){
      #print(c(data_unique$entity_normalized[i],temp$stem[j]))
      data_unique$stem[i] = temp_single$stem[j]
      break
    }
  }
}

#delete all unmerged non candidate
data_unique = data_unique[complete.cases(data_unique), ]
data_unique$n = NULL
data_unique$stem_ab = data_unique$stem

#unify abbreviations and full names
ab_transfer = function(string){
  temp = gsub("_new_york_stock_exchange_","_nyse_",string)
  temp = gsub("_federal_reserve_","_fed_",temp)
  temp = gsub("_us_securities_and_exchange_commission_","_sec_",temp)
  temp = gsub("_standard_&_poor_","_s_&_p_",temp)
  temp = gsub("_international_monetary_fund_","_imf_",temp)
  temp = gsub("_european_central_bank_","_ecb_",temp)
  temp = gsub("_european_union_","_eu_",temp)
  temp = gsub("_bank_of_japan_","_boj_",temp)
  temp = gsub("_societe_generale_","_socgen_",temp)
  temp = gsub("_societe_general_","_socgen_",temp)
  temp = gsub("_royal_bank_of_scotland_","_rbs_",temp)
  temp = gsub("_european_stability_mechanism_","_esm_",temp)
  temp = gsub("_federal_deposit_insurance_","_fdic_",temp)
  temp = gsub("_london_stock_exchange_","_lse_",temp)
  temp = gsub("_swiss_national_bank_","_snb_",temp)
  temp = gsub("_european_financial_stability_facility_","_efsf_",temp)
  temp = gsub("_european_financial_stability_fund_","_efsf_",temp)
  temp = gsub("_organization_petroleum_exporting_countries_","_opec_",temp)
  temp = gsub("_bank_of_america_","_bofa_",temp)
  temp = gsub("_federal_open_market_committee_","_fomc_",temp)
  temp = gsub("_royal_bank_of_canada_","_rbc_",temp)
  return(temp)
}
ad_transfer = function(string){
  temp = gsub("_s_&_p_","_standard_&_poor_",string)
  temp = gsub("_socgen_","_societe_generale_",temp)
  temp = gsub("_rbs_","_royal_bank_of_scotland_",temp)
  temp = gsub("_bofa_","_bank_of_america_",temp)
  temp = gsub("_rbc_","_royal_bank_of_canada_",temp)
  temp = gsub("_boj_","_bank_of_japan_",temp)
  return(temp)
}

#and filter out non-firms using organization list
data_unique = filter(data_unique, !(stem %in% org$stem_ab))
data_unique$stem_ab = ab_transfer(data_unique$stem)
data_unique = filter(data_unique, !(stem_ab %in% org$stem_ab))
data_unique$stem = ad_transfer(data_unique$stem_ab)

#now merge our unique entity clean results ('stem') into unique entity names we have before
data_unique$single = 1
data_unique$single[which(str_count(data_unique$stem, '_')>2)] = 0

data_unique = data.table(data_unique)
data = data.table(data)
setkey(data_unique,"entity_normalized")
setkey(data,"entity_normalized")
data <- data[data_unique]

setkey(data,"entity")
entity_full = data.table(entity_full)
setkey(entity_full,"entity")
final = entity_full[data]
final = final[order(final$row_order),]
final$row_order = NULL
final$entity_id = NULL
final$weight = NULL
write_csv(final,"Crypto_Full_Entity_Result.csv")


rm(list = ls())


####### Now match our identifications with list of cryptos #######

entity_full = read_csv("Crypto_Full_Entity_Result.csv")
sample = entity_full %>%
  group_by(stem,stem_ab) %>%
  summarise (weight = n())
sample = sample[order(sample$weight,decreasing = TRUE),]
sample$single = 1
sample$single[which(str_count(sample$stem, '_')>2)] = 0
base = read_csv('aggregated_crypto_data_nonna_v3-prelim.csv')[,c('ticker','coinname')]
colnames(base) = c('symbol', 'name')
base = data.table(base)
# setkey(base,"name")
# base = unique(base) #no repeat names, good!
sample$stem_ab = gsub("_","",sample$stem_ab)
base$name_normalized = tolower(base$name)
base$ticker_normalized = tolower(base$symbol)

base$name_normalized = gsub("-", " ", base$name_normalized)
#base <- base[,name_normalized := gsub('\\.com', "", name_normalized)] 
base <- base[,name_normalized := gsub("\\.", " ", name_normalized)] 
base <- base[,name_normalized := gsub("[^[:alnum:][:blank:]&']",'',name_normalized)]

base$name_normalized = gsub(" ", "_", base$name_normalized)
base[,name_normalized := paste0("_",name_normalized,"_") ]
base$name_normalized = gsub("_&_", "&", base$name_normalized)
base$name_normalized = gsub("&", "_&_", base$name_normalized)
base$name_normalized = gsub("_'s_", "_", base$name_normalized)
base$name_normalized = gsub("_coin_", "_", base$name_normalized)
base$name_normalized = gsub("_token_", "_", base$name_normalized)
base$name_normalized = gsub("_coins_", "_", base$name_normalized)
base$name_normalized = gsub("_tokens_", "_", base$name_normalized)

base$stem = NA
base$name_normalized_2 = gsub("_","",base$name_normalized)
sample$stem_normalized = gsub("_","",sample$stem)
sample[,'name'] = sample[,'symbol'] = NA
base_naid = which(is.na(base$stem))
sample = data.frame(sample)
base = data.frame(base)
base$matched = 0

library(RecordLinkage)

#We want to treat single gram and multi gram seperately, apply stricker rules on the latter
#First, merge the pasted stems that are equal to the pasted official names
sample_naid = which(is.na(sample$name))
for (i in sample_naid){
  for (j in base_naid){
    if (base$name_normalized_2[j]==sample$stem_normalized[i]){
      sample[i,c('symbol','name')] = base[j,c('symbol','name')]
      base$stem[j] = sample$stem[i]
      base$matched[j] = 1
      break
    }
  }
}

#From here we seperate rules on multi and single grams
#Merge the multi gram stems that are contained in the official names or vice versa; when multiple matched, choose the most similar one
sample_naid = which(is.na(sample$name) & sample$single == 0)
for (i in sample_naid){
  l1 = 0
  l2 = 0
  record = 0
  for (j in base_naid){
    if (str_detect(base$name_normalized[j],sample$stem[i])|str_detect(sample$stem[i],base$name_normalized[j]) & 
        str_length(base$name_normalized[j])>2){ #here we can control stricity
      l2 = levenshteinSim(base$name_normalized[j],sample$stem[i])
      if (l2>l1&l2>0.2){#here we can control stricity
        sample[i,c('symbol','name')] = base[j,c('symbol','name')]
        base$stem[j] = sample$stem[i]
        l1 = l2
        record = j
      }
    }
  }
  if (record>0){base$matched[record] = 1}
}

#Now deal with single gram
#Merge the single gram stems_ab that are equal to the ticker_normalized (>=4)
sample_naid = which(is.na(sample$name) & sample$single == 1)
for (i in sample_naid){
  for (j in base_naid){
    if (!is.na(base$ticker_normalized[j])){
      if (base$ticker_normalized[j]==sample$stem_ab[i] & str_length(base$ticker_normalized[j])>=4){
        sample[i,c('symbol','name')] = base[j,c('symbol','name')]
        base$stem[j] = sample$stem[i]
        break
      }
    }
  }
}

#Merge the single gram stems that are contained in the official names
sample_naid = which(is.na(sample$name) & sample$single == 1)
base_naid = which(!is.na(base$stem))
for (i in sample_naid){
  l1 = 0
  l2 = 0
  record = 0
  for (j in base_naid){
    if (str_detect(base$name_normalized[j],sample$stem[i]) & str_length(sample$stem[i])>=7){
      l2 = levenshteinSim(base$name_normalized[j],sample$stem[i])
      if (l2>l1){
        sample[i,c('symbol','name')] = base[j,c('symbol','name')]
        base$stem[j] = sample$stem[i]
        l1 = l2
        record = j
      }
    }
  }
  if (record>0){base$matched[record] = 1}
}

#Merge the single gram stems_ab that are equal to the ticker_normalized (<4) that are previously matched
sample_naid = which(is.na(sample$name) & sample$single == 1)
base_naid = seq(1:nrow(base))#which(!is.na(base$stem)) #control stricity
for (i in sample_naid){
  for (j in base_naid){
    if (!is.na(base$ticker_normalized[j])){
      if (base$ticker_normalized[j]==sample$stem_ab[i] & str_length(base$ticker_normalized[j])<4){
        sample[i,c('symbol','name')] = base[j,c('symbol','name')]
        base$stem[j] = sample$stem[i]
        break
      }
    }
  }
}





sample_naid = which(!is.na(sample$name))
base_naid = which(!is.na(base$stem))
sample$stem_normalized = NULL

noises = c('Credit', 'Online', 'Ultra', 'Fidelity House')
sample = filter(sample, !(name %in% noises))

write_csv(sample,"Crypto_MatchList.csv",na = "") #we can manually double check samples of the matching results in this step

entity_full$row = c(1:dim(entity_full)[1])
entity_full = data.table(entity_full)
sample = data.table(sample)
setkey(entity_full,"stem")
setkey(sample,"stem")
entity_full = sample[entity_full]
entity_full = entity_full[order(entity_full$row),]
entity_full$i.stem_ab = NULL
entity_full = filter(entity_full,!is.na(name))
# remove collateralized cryptos #
#noise = read_csv('collateralized_cryptos.csv')
#entity_full = filter(entity_full, !(name %in% noise$CoinName))

write_csv(entity_full,"Crypto_Matched_Entity_Result.csv")


  
  
  
