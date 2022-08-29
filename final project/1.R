library(dplyr)
ered1=read.csv('/Users/apple/Desktop/ered.csv',header=T,na.strings="NA")
summary(ered1)
ered1$basement_finished = factor(ered1$basement_finished,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$basement_finished=as.numeric(ered1$basement_finished)
ered1$has_fireplace = factor(ered1$has_fireplace,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$has_fireplace=as.numeric(ered1$has_fireplace)
ered1$has_garage = factor(ered1$has_garage,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$has_garage=as.numeric(ered1$has_garage)
ered1$fully_taxable = factor(ered1$fully_taxable,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$fully_taxable=as.numeric(ered1$fully_taxable)
ered1$walkout_basement = factor(ered1$walkout_basement,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$walkout_basement=as.numeric(ered1$walkout_basement)
ered1$air_conditioning = factor(ered1$air_conditioning,levels = c('NO', 'Yes'),labels = c(0, 1))
ered1$air_conditioning=as.numeric(ered1$air_conditioning)
ered1$display_type = factor(ered1$display_type,levels = c('NONRES', 'RES'),labels = c(0, 1))
ered1$display_type=as.numeric(ered1$display_type)
ered1$site_coverage=as.numeric(gsub("%","",ered1$site_coverage))
ered1$fully_complete=factor(ered1$fully_complete,levels = c('','NO','Yes'),labels = c(0,0,1))
ered1$fully_complete=as.numeric(ered1$fully_complete)
ered1=filter(ered1,assessed_value!='0')
ered1=filter(ered1,effective_build_year!='NA')
#ered1=filter(net_area!='0')
ered1=ered1[complete.cases(ered1[ , 29]),]
attach(ered1)
dim(ered1)

set.seed(1)
ered1=ered1[ered1$assessed_value<2e8,]
train=sample(62698,62698*0.9)

df=ered1[,c('taxroll_number','effective_build_year','net_area','basement_finished','has_garage','has_fireplace','house_number','fully_taxable','site_coverage','fully_complete','building_count','walkout_basement','air_conditioning','display_type','site_coverage','lon','lat')]
df=scale(df)
df1=data.frame(df,assessed_value)
lm.fit=lm(assessed_value~taxroll_number+effective_build_year+net_area+basement_finished+has_garage+has_fireplace+house_number+fully_taxable+site_coverage+fully_complete+building_count+walkout_basement+air_conditioning+display_type+site_coverage+lon+lat,data=df1[train,])
lm_pred=predict(lm.fit, df1[-train,])
mse_lm=mean((lm_pred-ered1$assessed_value[-train])^2)
mse_lm
r2=1-mse_lm/var(df1$assessed_value[-train])
r2


lm.fit=lm(assessed_value~taxroll_number+effective_build_year+net_area+basement_finished+has_garage+has_fireplace+house_number+fully_taxable+site_coverage+fully_complete+building_count+walkout_basement+air_conditioning+display_type+site_coverage+lon+lat,data=ered1[train,])
lm_pred=predict(lm.fit, ered1[-train,])
mse_lm=mean((lm_pred-ered1$assessed_value[-train])^2)
mse_lm
r2=1-mse_lm/var(ered1$assessed_value[-train])
r2


library(gbm)
set.seed(1)
boost.assessed_value=gbm(assessed_value~taxroll_number+effective_build_year+net_area+basement_finished+has_garage+has_fireplace+house_number+fully_taxable+site_coverage+fully_complete+building_count+walkout_basement+air_conditioning+display_type+site_coverage+lon+lat,data=df1[train,],distribution="gaussian",n.trees=500,interaction.depth=2,shrinkage=0.3,verbose=F)
summary(boost.assessed_value)
yhat.boost=predict(boost.assessed_value,newdata=df1[-train,],n.trees=500)
mean((yhat.boost-ered1$assessed_value[-train])^2)
1-mean((yhat.boost-ered1$assessed_value[-train])^2)/var(ered1$assessed_value[-train])
