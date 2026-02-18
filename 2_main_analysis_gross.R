
# read data ----------------
library(tidyverse)
library(data.table)
library(broom)

climatebound <- terra::vect("INPUT/climate+dem+lucc+npp+bound/bound.shp")
threshold_gross <- fread("datatable/ca_threshold_gross.csv")


threshold0 <- threshold_gross |> 
  rename_at(vars(contains("y_")),~str_replace(.,"y_","")) |> 
  melt(measure = 6:85,variable.name ="date")  

 
# the reason I place the function replace in there because it processes fewer data compared to the data after pivot_longer.


threshold0[,
  date:=as.numeric(date)][,
  date:=date+2020
  ]

threshold1 <- copy(threshold0)[,c("tenth","ninetieth"):=NULL] |> 
  dcast(model+scenario+x+y+date~var,value.var=c("value"))  |>
  na.omit() |> 
  melt(measure = 6:8,variable.name ="var")  


threshold1 <- merge(threshold1,copy(threshold0)[,value:=NULL] |> 
  distinct(model,scenario,var,tenth,ninetieth))

rm(threshold_gross)


## Normal IAV ###############

# Are there other simple ways I can achieve the same thing?
# remove NA values in certain model.


IAV <- copy(threshold1)[,c("tenth","ninetieth"):=NULL] |> 
  dcast(model+scenario+x+y+date~var,value.var=c("value")) 



# # test data----
# Conclusion: there is NA value in certain models in the area with water body
# 
# summary(threshold)
# summary(IAV)
# 
# ## There is NA.value in NPP but not climate
# ## I had already known this since the PPT made from 9.4!
# 
# na_test <- IAV[is.na(npp)] |> select(model,scenario,x,y) |> 
#   summarise(number=n(),.by=c(model,scenario,x,y)) |> 
#   distinct(x,y,number,model,scenario) |> 
#   mutate(x_y=paste0(x,y))
# 
# ggplot(na_test,aes(x=x_y,y=model,col=model)) +geom_point()
# 
# 
# # Spatial test 
# na_test2 <- IAV[is.na(npp)&date==2021][,
#   tas:=NULL
# ] 
# 
# test_raster <-  na_test2    |> 
#   select(-c(date,npp))    |> 
#   group_nest(scenario,model) |> 
#   mutate(data=map(data,~rast(.,crs=climatebound))) 
# 
# par(mfrow=c(4,4),mar=c(0,0,0,0),oma=c(1,1,1,1))
# walk(test_raster$data,~plot(.))
# 
# par(mfrow=c(2,2),mar=c(0,0,0,0),oma=c(1,1,1,1))
# 
# test_raster2 <-  test_raster |> 
#   filter(scenario=="SSP126")
# test_raster2 |> pull(model)
# 
# for (i in 1:nrow(test_raster2)) {
#   plot(climatebound)
#   test_raster2$data[[i]] |> plot(add=T)
# 
# }
# 




# where is the na from?
# There is also na value in attribution!


# Spatial correlation ----------

IAV_cor <- copy(IAV)[,
  .(pr_npp=cor(npp,pr,use = "na.or.complete"),
  tas_npp=cor(npp,tas,use = "na.or.complete")),
  by=.(model,scenario,x,y)]


IAV_cor_p <- copy(IAV)[,
  .(pr_npp_p=cor.test(npp,pr)$p.value,
  tas_npp_p=cor.test(npp,tas)$p.value),
  by=.(model,scenario,x,y)]

cor_out <- IAV_cor |> merge(IAV_cor_p) 

# Spatial sensitivity -----

#The below examples are from ten years ago!


sensitvity <- copy(IAV) [,
  as.list(coef(lm("npp ~ tas+pr",.SD))),
  by=.(model,scenario,x,y)] |> 
  select(-`(Intercept)`)|> 
  rename(tas_sen=tas,pr_sen=pr)


model_fit <- copy(IAV) [,
  .(rsquare=summary(lm("npp ~ tas+pr",.SD))$r.squared),
  by=.(model,scenario,x,y)]



model_fit_p <- copy(IAV)[,
         as.list(tidy(lm("npp ~ tas+pr",.SD))[2:3,c("p.value","term")]),
         by=.(model,scenario,x,y)] 


model_fit_p1 <- model_fit_p |> 
  dcast(model+scenario+x+y~term,value.var="p.value")


model_out <-  model_fit |> merge(sensitvity)


# IAV_TEST <- copy(IAV)[1:80]
# try <- lm("npp ~ tas+pr",IAV_TEST) |> broom::tidy()
# try <- lm("npp ~ tas+pr",IAV_TEST) |> summary()
# try |> select(term,p.value) |> 
#   dtplyr::lazy_dt() |> 
#   pivot_wider(names_from=term,values_from = p.value)
# IAV_TEST[,
#   as.list(tidy(lm("npp ~ tas+pr",.SD)) |> filter(term=="tas") |> select(p.value)),
#   by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(tidy(lm("npp ~ tas+pr",.SD))[]),
#          by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(tidy(lm("npp ~ tas+pr",.SD))[2:3,]),
#          by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(tidy(lm("npp ~ tas+pr",.SD))[term=="tas",]),
#          by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(tidy(lm("npp ~ tas+pr",.SD))[2:3,c("p.value","term")]),
#          by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(summary(lm("npp ~ tas+pr",.SD))$coefficients),
#          by=.(model,scenario,x,y)] 
# IAV_TEST[,
#          as.list(coef(lm("npp ~ tas+pr",.SD))),
#          by=.(model,scenario,x,y)] 
# try$fstatistic






# Spatial contribution ------------

## NPP contribution ----

npp_sum_nospace <-  copy(IAV)[,
  c("pr","tas"):=NULL][,
  npp_sum:=sum(npp),     # The sum of NPP across all region and date, we still save the npp value
  by=.(model,scenario,date)  # no space
  ][,
  abs:=abs(npp_sum)  # The absolute value of sum of NPP
  ][,  
  direction:= npp_sum/abs
  ]


npp_sum_up <- copy(npp_sum_nospace)[,   # no date
  npp:=npp*direction
  ][,
  .(npp=sum(npp)),
  by=.(model,scenario,x,y)]  # only space


npp_sum_down <- copy(IAV)[,
  c("pr","tas"):=NULL][,
  .(npp_sum=sum(npp)),  # sum across date and space
  by=.(model,scenario,date)
  ][,
  .(npp_sum=sum(abs(npp_sum))),
  by=.(model,scenario)
           ]

npp_contri <- merge(npp_sum_down,npp_sum_up)[,
  contri:=npp/npp_sum][,
  npp_sum:=NULL]


 npp_contri |> 
   select(-c(npp_sum,npp)) |>
   summarise(value=sum(contri,na.rm=T),.by = c(model,scenario))
 
 
 ## Climate contribution ----
 
climate  <- copy(IAV)[,npp:=NULL][
   sensitvity,
   on=.(scenario=scenario,x=x,y=y,model=model)][,
     c("tas","pr"):=.(tas*tas_sen,pr*pr_sen)
   ][,
     c("tas_sen","pr_sen"):=NULL] |> 
  melt(measure.vars = c("pr","tas"),variable.name = "var")
 

# Does not work
climate_up <- climate[npp_sum_nospace,
  on=.(scenario=scenario,x=x,y=y,model=model,date=date)][,
    value:=value*direction
#    c("tas","pr"):=.(tas*direction,pr*direction) Trying to melt data at end  
  ][,
#    .(tas=sum(tas),pr=sum(pr))
    .(value=sum(value)),
    by=.(model,scenario,x,y,var)] ##|> 
#  melt(measure.vars = c("pr","tas"),variable.name = "var")


cli_contri <- merge(npp_sum_down,climate_up)[,
  value:=value/npp_sum][,
  npp_sum:=NULL]


cli_contri |> 
  select(-c(value)) |>
  filter(scenario=="SSP585",var=="pr") |>
  summarise(value=sum(contri,na.rm=T),.by = c(var,model,scenario))

# IAV&Ext ----
## The IAV ----
#?[var=="npp"]

IAV_nospace <- copy(threshold1)[,
c("tenth","ninetieth"):=NULL][,
lapply(.SD,sum,na.rm=T),
.SDcols = "value",by=.(var,scenario,date,model)]

## ext ------

## pinpoint the extreme
ext <- copy(threshold1)[,
c("tenth","ninetieth"):=.(ifelse(value<tenth,value,NA),
ifelse(value>ninetieth,value,NA))]

ext_IAV_nospace  <- copy(ext)[,
  lapply(.SD,sum,na.rm=T),
  .SDcols=c("tenth","ninetieth","value"),
  by=.(date,scenario,model,var)]|>  
  mutate(ten_nine=tenth+ninetieth) 



## ext certain number ----------------

ext_number_ten_nospace <- copy(ext)[!is.na(tenth)][,
  rank := rank(tenth),
  by=.(model,scenario,date,var)][
  rank <= 50][,
  lapply(.SD,sum,na.rm=T),
  .SDcols = "tenth",
   by=.(var,scenario,date,model)]

ext_number_nine_nospace <-copy(ext)[!is.na(ninetieth)][,
  rank := rank(-ninetieth),
  by=.(model,scenario,date,var)][
  rank <= 50][,
  lapply(.SD,sum,na.rm=T),
  .SDcols = "ninetieth",
  by=.(var,scenario,date,model)]


ext_IAV_nospace_plot <- left_join(ext_number_ten_nospace,
                              ext_number_nine_nospace) |> 
  mutate(ten_nine=tenth+ninetieth)      |>
  left_join(ext_IAV_nospace,by=c("var", "scenario", "date", "model"),
  suffix=c("_50","_all")) 



# 



# Cor Extreme IAV&IAV -----

IAV_nospace1 <- IAV_nospace[var=="npp"][,var:=NULL]

ext_number_ten <- copy(ext)[!is.na(tenth)&var=="npp"][,
    c("ninetieth","value","var"):=NULL]
ext_number_nine <- copy(ext)[!is.na(ninetieth)&var=="npp"][,
    c("tenth","value","var"):=NULL]

A <- list()

for (i in 1:200) {
  
ext_number_ten_nospace <- ext_number_ten[,
  rank := rank(tenth),
  by=.(model,scenario,date)][
  rank <= i][,
  lapply(.SD,sum,na.rm=T),
  .SDcols = "tenth",
   by=.(scenario,date,model)]

ext_number_nine_nospace <-ext_number_nine[,
  rank := rank(-ninetieth),
  by=.(model,scenario,date)][
  rank <= i][,
  lapply(.SD,sum,na.rm=T),
  .SDcols = "ninetieth",
  by=.(scenario,date,model)]


A[[i]] <- merge(ext_number_ten_nospace,
  ext_number_nine_nospace)[,
  tennine:=tenth+ninetieth][IAV_nospace1,
  on=.(scenario=scenario,date=date,model=model)][,
  .(cor=cor(value,tennine,use = "na.or.complete")), # In many models, there is no data in certain year
  by=.(scenario,model)]}

  
for (i in 1:200) {
  A[[i]] <- mutate(A[[i]],number=i)
}

cor1_200_date_plot <- A |> bind_rows()




# Attribution ------

ext_long <- ext[,
  value:=NULL]                        |> 
  melt(measure=c("tenth","ninetieth"),
       variable.name = "magnitude")   |> 
  na.omit("value") 


cli_npp <- copy(ext_long)[
  !var=="npp",
  value:=fifelse(is.na(value),value,1)]|>  # Converting the climate value to 1 represents the frequency 
  dcast(model+scenario+x+y+date~var+magnitude,
  value.var = c("value"))

names(cli_npp)[8:11] <- c("dry","wet","cold","hot")

cli_npp[,
  dry_hot:=hot*dry   # Find the compound events # 1*na=na, 1*1=1 
  ][,npp_ninetieth:=NULL]           

# Below code is used to get the dry events which are not hot and hot events which are not dry  

cli_isolate <-  copy(cli_npp)[  # Find the extreme events separate to compound events
  is.na(dry_hot)] [,  # Remove the compound events  
  c("wet","cold","dry_hot","npp_tenth"):=NULL] |> 
  setnames(c("dry","hot"),c("dry_nothot","hot_notdry"))


cli_all <- merge(cli_npp,cli_isolate,all=T,by=names(cli_npp)[1:5])


att <- copy(cli_all)[!is.na(npp_tenth)][,       # 1*na=na, 1*value=value 
  (names(cli_all)[7:13]):=map(.SD,\(x)x*npp_tenth), 
  .SD=dry:hot_notdry]


att_long <- copy(att)[,npp_tenth:=NULL] |> 
  melt(measure.vars = 6:12,variable.name = "ext_type")|> 
  na.omit("value")


## spatial plot -----

att_nodate <- att_long[,
#  .(value=mean(value,na.rm = T),
  .(value=sum(value)/80,   # the sum value divided by the number of years
  quantity=.N),
  by=.(scenario,x,y,model,ext_type)]

att_nodatemodel <- att_nodate [,
  .(value=mean(value,na.rm = T),
  quantity=mean(quantity,na.rm=T),
  models=.N),
  by=.(scenario,x,y,ext_type)]


## zone_table -----

att_gross_nodate_zone <-  att_nodate              |> 
  group_nest(scenario,ext_type,model) |> 
  mutate(data=map(data,~rast(.,crs=climatebound))) |> 
  mutate(zone=map(data,~zonal(.,climatebound,sum,na.rm=T) |> 
  mutate(region=c("north","central","sw","se"))))  |> 
  mutate(global=map(data,~global(.,sum,na.rm=T)|> 
  rownames_to_column() |> 
  pivot_wider(names_from = rowname,values_from = sum))) |> 
  select(-data)                                       |> 
  unnest(cols = c(zone, global),names_sep = "_")  |>  # below is how to tidy the data
  pivot_wider(names_from = zone_region,
  values_from = c(zone_value,zone_quantity)) |> 
  pivot_longer(global_value:zone_quantity_se) |> 
  separate(name,into=c("name","index","region"),sep="_") |> 
  mutate(region=ifelse(is.na(region),"CA",region)) |> 
  select(-name)



# save data -----------------
pattern <- "plot$|zone$"
objects_to_save <- ls(pattern = pattern)

save(list=objects_to_save,file="rdata/ca_gross.RData")

objects_to_save <- c("cor_out", 
"model_out",
"npp_contri",
"cli_contri",
"model_fit_p1")


save(list=objects_to_save,file="rdata/ca_gross_space_relationship.RData")


objects_to_save <- c(#"cor_out", 
                     "model_out",
                     "npp_contri",
                     "cli_contri",
                     "model_fit_p1")


save(list=objects_to_save,file="rdata/ca_gross_space_relationship_extreme.RData")











