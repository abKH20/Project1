getwd()
setwd("C:/Users/Abhijit/Downloads")

rs_train=read.csv("housing_train.csv ", stringsAsFactors = FALSE)
rs_test=read.csv("housing_test.csv ", stringsAsFactors = FALSE)

#Combining train & test

rs_test$Price=NA
rs_train$data='train'
rs_test$data='test'
rs_all=rbind(rs_test,rs_train)

#data prep
library(dplyr)
glimpse(rs_all)

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

char_logical=sapply(rs_all,is.character)
cat_cols=names(rs_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','Price'))]
cat_cols

for(col in cat_cols){
  rs_all=CreateDummies(rs_all,col,50)
}
glimpse(rs_all)

rs_all=rs_all[!((is.na(rs_all$Price)) & rs_all$data=='train'), ]

for(col in names(rs_all)){
  if(sum(is.na(rs_all[,col]))>0 & !(col %in% c("data","price"))){
    rs_all[is.na(rs_all[,col]),col]=mean(rs_all[rs_all$data=='train',col],na.rm=T)
  }
}


rs_train=rs_all %>% filter(data=='train') %>% select(-data)
rs_test=rs_all %>% filter(data=='test') %>% select(-data,-Price)

set.seed(2)
s=sample(1:nrow(rs_train),0.8*nrow(rs_train))
train=rs_train[s,]
val=rs_train[-s,]

library(car)
fit=lm(Price~.,data=train)

summary(fit)
vif(fit)
sort(vif(fit),decreasing = T)

fit=lm(Price~.-CouncilArea_ ,data=train)
sort(vif(fit),decreasing = T)


fit=lm(Price~.-CouncilArea_-Postcode ,data=train)
sort(vif(fit),decreasing = T)

fit=lm(Price~.-CouncilArea_-Postcode-Distance ,data=train)
sort(vif(fit),decreasing = T)

fit=step(fit)

summary(fit)

val.pred=predict(fit,newdata = val)
errors=val$Price-val.pred

rmse=errors**2 %>% mean() %>% sqrt()
rmse
Score = 212467/rmse
Score

fit.final=lm(Price~.,data=rs_train)
fit.final=step(fit.final)

test.predictions=predict(fit.final,newdata=rs_test)
write.csv(test.predictions,'Abhijit_khairnar_P1_part2.csv',row.names = F)

