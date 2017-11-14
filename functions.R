###### Functions for the preparation of the education covariate and risk factor ######
###### By: Hunter York, hyork@uw.edu                                            ######
###### Adapted from code by Joe Friedman                                        ######
###### 11/14/2017                                                               ######

###################################################################################################################################################
load_edu_dta <- function(file) {

  data <- data.table(read_dta(file))
  print(file)
  return(data)

}

###################################################################################################################################################
import_prepped_data <- function() {

	files <- list.files(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/prepped_data"),full.names = T)
	data.list <- mclapply(files, load_edu_dta ,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))
	prepped.data <- rbindlist(data.list,fill=T)

}	

###################################################################################################################################################
collapse_surveys <- function() {

	for (c.level in c(8)) {  
  
		data <- copy(prepped.data)
		#change level of interest to standard variable name
		setnames(data,paste0("level_",c.level),"ihme_loc_id")
		#subset
		data <- data[age>14&ihme_loc_id!=""&sex%in%c(1,2)&!is.na(age)&!is.na(edu_min)&edu_min>=0 &age<140]
		data[,bins:=ifelse(edu_min==edu_max,0,1)]
		bins <- data[,.(bins=max(bins)),by=.(year,ihme_loc_id,type,nid)]
		data[,bins:=NULL]
		data <- data.table(merge(data,bins,by=c("year","ihme_loc_id","type","nid")))
		data <- data[bins==0]

		data[edu_min>18,edu_min:=18]
		data[,edu_min:=floor(edu_min)]
		#make age groups
		data[,age_start:=5*floor(age/5)]
		data[age_start>95,age_start:=95]
		data[age_start==95,age_end:=110]

		#swap out sample size for count for level 8

		if (c.level == 8) {
			data[,count:=num_persons]

		}


		#collapse to desired geography, age groups, sex, bins
		data <- data[,.(count=sum(count),sample_size=sum(num_persons)),by=.(age_start,sex,year,edu_min,nid,ihme_loc_id,type)]
		#get total sample size and totals for calculations
		total <- data[,.(total=sum(count),total_ss=sum(sample_size)),by=.(age_start,sex,year,nid,ihme_loc_id,type)]
		data <- data.table(merge(data,total,by=c("age_start","sex","year","nid","ihme_loc_id","type")))
		#calculate proportion in each single year
		data[,proportion:=count/total]
		setnames(data,"edu_min","edu_yrs") 
		#standard error of proportions
		data[,proportion_se:=sqrt(proportion * (1 - proportion)/total_ss)]
		#mean and standard error of mean
		means <- data[,.(mean=weighted.mean(x=edu_yrs,w=proportion),sd=wt.sd(x=edu_yrs,w=proportion)),by=.(age_start,sex,year,nid,type,ihme_loc_id)]
		data <- data.table(merge(data,means,by=c("age_start","sex","year","nid","ihme_loc_id","type")))
		data[,mean_se:=sd / sqrt(total_ss)]
		write.csv(data,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/single_year_",c.level,".csv"),row.names=F)

	}

}

###################################################################################################################################################
collapse_binned <- function() {
	
}

###################################################################################################################################################
import_tabulations <- function() {

	files <- list.files(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/input_data/tabulations/"),full.names = T)
	data.list <- mclapply(files, load_edu_dta ,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))
	edu.data <- rbindlist(data.list,fill=T)

}


###################################################################################################################################################
clean_tabulations <- function() {

	setnames(edu.data,"age end","age_end2")
	setnames(edu.data,"age-start","age_start2")
	#names(edu.data)[3] <- "age_end2"
	#names(edu.data)[29] <- "age_start2"
	#edu.data[is.na(year),year:=Year]
	edu.data[is.na(sex),sex:=SEX]
	edu.data[is.na(age_end),age_end:=age_end2]
	edu.data[is.na(count),count:=edu_count]
	edu.data[is.na(count),count:=cout]
	edu.data[is.na(age_start),age_start:=age_start2]
	edu.data[is.na(edu_start),edu_start:=edu_min]
	edu.data[is.na(edu_end),edu_end:=edu_max]
	edu.data[,year:=as.numeric(substr(year,1,4))]
	edu.data[,count:=gsub(x=count,pattern=",",replacement="")]
	edu.data[,count:=gsub(x=count,pattern=" ",replacement="")]
	edu.data[,count:=as.numeric(count)]
	edu.data <- edu.data[!is.na(sex) & !is.na(age_start) & !is.na(age_end) & !is.na(year) & !is.na(iso3) & !is.na(edu_start) & !is.na(edu_end) &!is.na(count)]
	edu.data <- edu.data[,.SD,.SDcols=c("iso3","year","age_start","age_end","sex","edu_start","edu_end","count","nid","type")]
	setnames(edu.data,"edu_start","edu_min")
	setnames(edu.data,"edu_end","edu_max")
	setnames(edu.data,"iso3","ihme_loc_id")

}	

###################################################################################################################################################
xwalk_dat <- function(c.id,all.data) {  
	print(c.id)
	data  <- all.data[iso_year_type==c.id]

	#identify training set
	cc.iso <- substr(unique(data$ihme_loc_id),1,3)
	target.r <- unique(locs[ihme_loc_id==cc.iso,region_name])
	target.sr <- unique(locs[ihme_loc_id==cc.iso,super_region_name])
	train <- copy(all.train)
	train[,spatial_distance:=1]
	train[ihme_loc_id %in% unique(locs[super_region_name==target.sr,ihme_loc_id]),spatial_distance:=.66]
	train[ihme_loc_id %in% unique(locs[region_name==target.r,ihme_loc_id]),spatial_distance:=.33]
	train[ihme_loc_id == cc.iso,spatial_distance:=0]
	c.year  <- unique(data$year)
	train[, temporal_distance := abs(year - c.year) / (max(year) - min(year))]
	train[, distance := (spatial_distance * space_weight) + (temporal_distance * time_weight)]
	rank <- train[,.(distance=mean(distance)),by=.(ihme_loc_id,type,year)]
	rank[,rank:=rank(distance,ties.method = "random")]
	train <- data.table(merge(train,rank,by=c("ihme_loc_id","type","year","distance")))
	train <- train[rank<=training_size]

	#Crosswalk in a bin-specific fashion
	data[,bin:=paste0(edu_min,"_",edu_max)]
	split.data <- list()
	bin.counter <- 1
	#loop over bins
	for (c.bin in unique(data$bin)) {
	  #subset training data and data to be crosswalked  
	  s.data <- data[bin==c.bin] 
	  s.train <- train[edu_yrs >= min(s.data[,edu_min]) & edu_yrs <= max(s.data[,edu_max])]
	  s.train <- s.train[,.SD,.SDcols=c("rank","edu_yrs","count","age_start","sex")]
	  s.train.tot <- s.train[,.(total=sum(count)),by=.(rank,age_start,sex)]  
	  s.train <- data.table(merge(s.train,s.train.tot,by=c("rank","age_start","sex")))  
	  s.train[,prop:=count/total]
	  s.train <- s.train[,.(prop=mean(prop)),by=.(edu_yrs,age_start,sex)]  
	  s.data <- data.table(merge(s.data,s.train,by=c("age_start","sex")))
	  s.rake <- s.data[,.(rake=sum(prop)),by=.(age_start,sex)]
	  s.data <- data.table(merge(s.data,s.rake,by=c("age_start","sex")))
	  s.data[,prop:=prop/rake]
	  s.data[,count:=count*prop]
	  s.data[,sample_size:=sample_size*prop]
	  split.data[[bin.counter]] <- s.data
	  bin.counter <- bin.counter + 1
	}
	data <- rbindlist(split.data)


	#get total sample size and totals for calculations
	total <- data[,.(total=sum(count),total_ss=sum(sample_size)),by=.(age_start,sex,year,nid,ihme_loc_id,type)]
	data <- data.table(merge(data,total,by=c("age_start","sex","year","nid","ihme_loc_id","type")))
	data[,proportion:=count/total]
	#standard error of proportions
	data[,orig_proportion_se:=sqrt(proportion * (1 - proportion)/total_ss)]
	data[,prop_se:=sqrt(prop * (1 - prop)/training_size)]
	data[,proportion_se:= sqrt((prop_se^2) + (orig_proportion_se)^2)]

	#mean and standard error of mean
	means <- data[,.(mean=weighted.mean(x=edu_yrs,w=proportion),sd=wt.sd(x=edu_yrs,w=proportion)),by=.(age_start,sex,year,nid,type,ihme_loc_id)]
	data <- data.table(merge(data,means,by=c("age_start","sex","year","nid","ihme_loc_id","type")))
	data[,mean_se:=sd / sqrt(total_ss)]

	if (is.data.table(data) == T) {
		return(data)
	}
}	


###################################################################################################################################################
age_split_tabulations <- function() {
	
	edu.data[age_start==16 & (age_end-age_start>0),age_start:=15]
	edu.data[,age_range:=age_end-age_start]
	#collapse single year ages to 5 years
	single <- edu.data[age_range==0]
	single[,age_start:=5*floor(age_start/4)]
	single[,age_end:=age_start+4]
	single <- single[,.(count=sum(count)),by=.(age_start,age_end,sex,year,ihme_loc_id,edu_min,edu_max,type)]
	edu.data <- rbind(edu.data[age_range>0],single,fill=T)
	edu.data[,age_range:=age_end-age_start]

	#data usable as-is
	orig_5 <- edu.data[age_range %in% c(4,15)]
	#data to be split
	orig_10 <- edu.data[age_range==9]
	temp10_1 <- copy(orig_10)
	temp10_2 <- copy(orig_10)
	temp10_1[,age_end:=age_end-5]
	temp10_2[,age_start:=age_start+5]
	new_10 <- rbind(temp10_1,temp10_2)
	new_10[,age_range:=age_end-age_start]

	orig_15 <- edu.data[age_range==14]
	temp15_1 <- copy(orig_15)
	temp15_2 <- copy(orig_15)
	temp15_3 <- copy(orig_15)
	temp15_1[,age_end:=age_end-10]
	temp15_2[,age_start:=age_start+5]
	temp15_2[,age_end:=age_end-5]
	temp15_3[,age_start:=age_start+10]
	new_15 <- rbind(temp15_1,temp15_2,temp15_3)
	new_15[,age_range:=age_end-age_start]

	all.data <- rbind(orig_5,new_10,new_15)
	all.data <- all.data[,.(count=sum(count)),by=.(ihme_loc_id,year,age_start,age_end,sex,edu_min,edu_max,nid,type)]

	all.data[is.na(type),type:="Census"]
	all.data[age_start>95,age_start:=95]
	all.data[age_start==95,age_end:=110]

	all.data <- all.data[count!=0]



	all.train <- fread(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/single_year_3.csv"))
	locs <- fread(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/ref/GBD_2016_locs_20161114.csv"))

	#loop over country-year
	all.data[,iso_year_type:=paste0(ihme_loc_id,"_",year,"_",type)]
	all.data[,sample_size:=count]

	all.split.data <- mclapply(unique(all.data$iso_year_type), xwalk_dat ,all.data=all.data,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))
	all.split.data.table <- rbindlist(all.split.data)


	all.split.data.table[total_ss<20,total_ss:=10850]
	 
	write.csv(all.split.data.table,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/tabulations.csv"),row.names=F)

}

###################################################################################################################################################
load_all_data <- function() {

	data.files <- c(list.files(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data"),full.names = T,pattern=".csv"),
                list.files(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/binned"),full.names = T))
	data.files <- data.files[!(data.files %in% c(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/single_year_7.csv"),
                paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/single_year_8.csv")))]

	data.list <- mclapply(data.files,fread,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))

}

###################################################################################################################################################
compile_education_distributions <- function() {
	edu.dists <- rbindlist(data.list,fill=T)
	#Subset to correct years
	edu.dists <- edu.dists[year <= end_year & year >= start_year]
	edu.dists[,iso3:=ihme_loc_id]
	#Just keep means
	edu.dists[,sample_size:=total_ss]
	write.csv(edu.dists,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/output_data/education_distributions.csv"),row.names=F)

}

###################################################################################################################################################
compile_education_means <- function() {
	
	edu.means <- edu.dists[,.(mean=mean(mean),mean_se=mean(mean_se)),by=.(age_start,sex,year,iso3,sample_size,type, nid)]
	#drop if less than 20 people
	edu.means <- edu.means[sample_size>14]
	#load outliers list, drop outliers
	outliers <- fread(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/ref/outliers.csv"))
	edu.means <- data.table(merge(edu.means,outliers,all=T,by=c("iso3","year","type")))
	#custom outliers
	edu.means[type=="Census" & substr(iso3,1,3) == "IND",is_outlier:=1]
	edu.means[type=="UKGHS" & age_start==35,is_outlier:=1]
	edu.means <- edu.means[is.na(is_outlier)]


	#Drop all of LSMS since the extraction is very low quality currently
	edu.means <- edu.means[type!= "LSMS"]
	edu.means <- edu.means[type!= "NHIS"]


	#tabulate country-survey-years
	locs <- fread(paste0(jpath,"/WORK/01_covariates/02_inputs/education/update_2017/ref/GBD_2016_locs_20161020.csv"))
	temp <- edu.means[nchar(iso3)==3,.(mean=mean(mean)),by=.(iso3, year,type)]
	temp$obs <- 1
	temp2 <- temp[,.(Number=sum(obs)),by=.(type)]
	dir.create(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/output_data/",model_version),showWarnings = F)
	          
	write.csv(temp2[order(-Number)],
	paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/output_data/",model_version,"/sources_record.csv"),row.names=F)

}

###################################################################################################################################################
back_cast <- function(dt,time) {

  timecast.dt <- dt[age_start>=25+time]
  timecast.dt <- timecast.dt[,age_start:= age_start - time]
  timecast.dt <- timecast.dt[,year:=year - time]
  timecast.dt[,point_type:= -1*time]
  timecast.dt[,mean_se:=mean_se * ((time/5)+1)]
  return(timecast.dt)

}


###################################################################################################################################################
for_cast <- function(dt,time) {
  
  timecast.dt <- dt[age_start<=95-time & age_start>=25]
  timecast.dt <- timecast.dt[,age_start:= age_start + time]
  timecast.dt <- timecast.dt[,year:=year + time]
  timecast.dt[,point_type:=time]
  timecast.dt[,mean_se:=mean_se * ((time/5)+1)]  
  return(timecast.dt)

}

###################################################################################################################################################
create_back_and_fore_casts<- function() {

	back.casts.list <- mclapply(X=seq(5,55,5),FUN=back_cast,dt=edu.means,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))
	for.casts.list <- mclapply(X=seq(5,70,5),FUN=for_cast,dt=edu.means,mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))
	back.casts <- rbindlist(back.casts.list)
	for.casts <- rbindlist(for.casts.list)

	#Create one dataset
	orig.summs <- edu.means
	orig.summs[,point_type:=0]
	all.means <- rbind(edu.means,for.casts,back.casts)

}

###################################################################################################################################################
fit_mixed_effects_model<- function() {

	locs <- fread(paste0(jpath,"/WORK/01_covariates/02_inputs/education/update_2017/ref/GBD_2016_locs_20161020.csv"))
	template <- expand.grid(year=seq(start_year,end_year,1),iso3=unique(locs$ihme_loc_id),age_start=unique(all.means$age_start),sex=c(1,2))
	#merge on regional info
	locs[,iso3:=ihme_loc_id]
	locs<- locs[,.SD,.SDcols=c("iso3","region_name","super_region_name")]
	template <- data.table(merge(template,locs,by='iso3'))
	template <- template[,iso3:= as.character(iso3)]
	#merge on data
	all.data <- data.table(merge(all.means,template,by=c('iso3','year','age_start','sex'),all.y=T))
	all.data <- all.data[year <= end_year & year >= start_year]
	#make region-sex var
	all.data[,reg_sex:=paste0(region_name,sex)]
	all.data[,iso3_age:=paste0(iso3,age_start)]
	#rescale and take logit of data
	all.data[mean < .05,mean:=.05]
	all.data[mean > 17.95,mean:=17.95]
	all.data[,logit_mean:=logit(mean/18)]
	all.data[age_start>=40,age_type:="old"]
	all.data[age_start<40,age_type:="young"]


	#Fit linear model for mean
	for (c.reg_sex in unique(all.data$reg_sex)){
	print(c.reg_sex)
	#linear.prior.mean <- lmer(logit_mean~(1|iso3_age) + ns(year,df=all.data[reg_sex==c.reg_sex],knots=c(seq(1970,2000,10))):age_start
	                        #+ns(age_start,df=all.data[reg_sex==c.reg_sex],knots=c(25,35,45,55,65,80)),data=all.data[reg_sex==c.reg_sex])
	#linear.prior.mean <- lmer(logit_mean~(1|iso3_age) + ns(year,
	                                      #knots=c(1990,2000)):age_type,data=all.data[reg_sex==c.reg_sex])

	#linear.prior.mean <- lmer(logit_mean~(1|iso3_age) + bs(year,knots=c(1980),degree=1):age_type,data=all.data[reg_sex==c.reg_sex])
	linear.prior.mean <- lmer(logit_mean~ year + (1|iso3_age) +ns(age_start,df=all.data[reg_sex==c.reg_sex],knots=c(35,50,65)),data=all.data[reg_sex==c.reg_sex])
	  

	#summary(linear.prior.mean)
	all.data[reg_sex==c.reg_sex,logit_prior_mean:= predict(linear.prior.mean,newdata=all.data[reg_sex==c.reg_sex],allow.new.levels=T)]
	all.data[reg_sex==c.reg_sex,prior_mean:=inv.logit(logit_prior_mean) * 18]
	}


###################################################################################################################################################
add_non_sampling_error<- function() {

	nse <- all.data
	nse[,abs.dev := abs(mean - prior_mean)]
	nse = nse[,.(nse=median(abs.dev,na.rm=T))]

	nse_add <- as.numeric(nse[,nse[1]])
	all.data[,mean_se:=mean_se + nse_add]

}

###################################################################################################################################################
delta_transform_variance<- function() {

	all.data[,mean_variance:=mean_se^2]
	all.data[,logit_mean_variance:=mean_variance/(324) * (1/((mean/18)*(1-(mean/18))))^2]
	all.data[mean < .5,logit_mean_variance:=mean_variance/(324) * (1/((.5/18)*(1-(.5/18))))^2]

}

###################################################################################################################################################
calculate_regional_MAD<- function()  {

	reg_mad <- all.data
	reg_mad[,abs.dev := abs(logit_mean - logit_prior_mean)]
	reg_mad = reg_mad[,.(mad=median(abs.dev,na.rm=T)),by=.(region_name)]

	all.data <- data.table(merge(all.data,reg_mad,by=c("region_name")))

}

###################################################################################################################################################
save_mixed_effects_model() <- function{
	###standardize panel variables
	setnames(all.data,old="sex",new="sex_id")
	setnames(all.data,old="iso3",new="ihme_loc_id")
	setnames(all.data,old="year",new="year_id")
	all.data[age_start < 80,age_group_id:=((age_start - 15) /5 ) + 8]
	all.data[age_start > 75,age_group_id:=((age_start - 15) /5 ) + 17]
	all.data[age_start ==95,age_group_id:=235]


	locs <- fread(paste0(jpath,"/WORK/01_covariates/02_inputs/education/update_2017/ref/GBD_2016_locs_20161020.csv"))
	locs <- locs[,c("location_id","location_name","ihme_loc_id"),with=F]
	save.data <- data.table(merge(all.data,locs,by=c('ihme_loc_id')))

	var95th <- quantile(save.data$logit_mean_variance,probs=.95,na.rm=T)
	save.data[logit_mean_variance < .001,logit_mean_variance:= var95th ]

	##save preds
	dir.create(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/output_data/",model_version),showWarnings = F)
	write.csv(save.data,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2017/data/output_data/",model_version,"/linear_prior.csv"),row.names=F)

}

###################################################################################################################################################

#GPR

###################################################################################################################################################

#SubmitGPR

###################################################################################################################################################
load_GPR_draws <- function() {

	files <- list.files(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/gpr_draws/"),full.names=T)
	estimates <- rbindlist(data.list,fill=T)

}


###################################################################################################################################################

collapse_GPR_means <- function(x){
	x <- x[,.(gpr_mean=mean(gpr_mean)),by=.(location_id,year_id,age_group_id,sex_id)]
}

###################################################################################################################################################

merge_pop_locs <- function(){

	#load locs
	locs <- fread(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/ref/GBD_2017_locs_20171020.csv"))[level >= 3]
	locs <- locs[,.SD,.SDcols=c("location_id","ihme_loc_id","location_name","parent_id","level")]

	#load pops
	pops <- readRDS(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/ref/GBD_2017_pops_20171020.Rds"))

	#Extend pops for 2017-2040
	pops <- pops[age_group_id %in% unique(estimates$age_group_id)]
	pops.extend <- pops[year_id==2017]
	for (y in 2018:2040) {
	  new_pop <- pops.extend
	  new_pop[,year_id:=y]
	  pops <- rbind(pops,new_pop)
	}

	#merge pops and locs onto data
	pops <- pops[location_id %in% unique(estimates$location_id) & sex_id < 3]

	est.pop <- data.table(merge(pops,estimates,by=c("location_id","year_id","age_group_id","sex_id"),all=T))
	est.pop.loc <- data.table(merge(est.pop,locs,by=c("location_id"),all.x=T))
}
###################################################################################################################################################


rake_estimates <- function() {

	est.pop.loc[,gpr_raked:=gpr_mean]

	assert_ids(est.pop.loc, id_vars = list(location_id = locs$location_id, age_group_id = c(8:20, 30:32, 235), sex_id = 1:2, year_id = 1950:2040), assert_dups = T)

	for (c.lvl in 4:6) {
	  #make aggs
	  aggs <- est.pop.loc[level==c.lvl]
	  aggs <- aggs[,.(agg_mean=weighted.mean(x=gpr_raked,w=population)),by=.(parent_id,year_id,age_group_id,sex_id)]
	  setnames(aggs,"parent_id","location_id")
	  rake <- data.table(merge(est.pop.loc,aggs,by=c("location_id","year_id","age_group_id","sex_id")))
	  #calculate raking factor
	  rake[,rake.factor:= gpr_raked / agg_mean]
	  rake <- rake[,.SD,.SDcols=c("location_id","year_id","age_group_id","sex_id","rake.factor")]
	  setnames(rake,"location_id","parent_id")
	  #merge onto estimates
	  est.pop.loc <- data.table(merge(est.pop.loc,rake,by=c("parent_id","year_id","age_group_id","sex_id"),all=T))
	  #rake
	  est.pop.loc[level== c.lvl, gpr_raked:= gpr_raked * rake.factor ]
	  setnames(est.pop.loc,"rake.factor",paste0("rake.factor",c.lvl))
	}
	#copy level estimates into raked var
	est.pop.loc[level==3,gpr_raked:=gpr_mean]


	#Test that it worked
	test <- est.pop.loc[level==4]
	test <- test[,.(test_mean=weighted.mean(x=gpr_raked,w=population)),by=.(parent_id,year_id,age_group_id,sex_id)] 

	#Save estimates for vizualization and graphing 
	raked.data <- est.pop.loc[,.SD,.SDcols=c("location_id","year_id","age_group_id","sex_id","gpr_raked")]
	estimates <- rbindlist(data.list,fill=T)
	estimates <- data.table(merge(estimates, raked.data , by=c("location_id","year_id","age_group_id","sex_id"),all=T))

	#Save loc specific for viz
	dir.create(paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/raked/"),showWarnings = F)

	for (iso in unique(estimates$ihme_loc_id)) {
	  write.csv(estimates[ihme_loc_id==iso],paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/raked/",iso,".csv"))  
	  print(iso) 
	}
	write.csv(estimates,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/raked.csv"))  
}



###################################################################################################################################################

make_education_covariate <- function() {

	esties17 <- estimates[,.(mean17=mean(gpr_raked)),by=.(location_id,year_id,age_group_id,sex_id)]
	esties17 <- data.table(merge(esties17,aw,by='age_group_id'))
	upload.ests <- esties17[year_id <= 2017, .(location_id, year_id, age_group_id, sex_id, mean17)]
	#use maternal for less than 15
	childages <- expand.grid(age_group_id = 2:7, sex_id = 1:2, location_id = unique(upload.ests$location_id), year_id = unique(upload.ests$year_id)) %>% as.data.table
	upload.ests <- rbind(upload.ests, childages, use.names = T, fill = T)
	upload.ests <- data.table(merge(upload.ests,mat,by=c('location_id','year_id'),all.x=T))
	upload.ests[age_group_id<8,mean17:=mat17]
	upload.ests[,mean_value:= mean17]
	upload.ests <- upload.ests[order(location_id,sex_id,age_group_id,year_id)]
	# COVARIATE INFO
	upload.ests[, c("covariate_id", "covariate_name_short") := .(33, 'education_yrs_pc')]
	#subset
	upload.ests <- upload.ests[,.SD,.SDcols=c("location_id","year_id","age_group_id","sex_id","covariate_id","covariate_name_short","mean_value")]
	upload.ests[,upper_value:=mean_value]
	upload.ests[,lower_value:=mean_value]
	#save
	assert_ids(upload.ests, id_vars = list(location_id = locs$location_id, year_id = 1950:2017, age_group_id = c(2:20, 30:32, 235), sex_id = 1:2))

	write.csv(upload.ests,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/education_upload.csv"),row.names = F)

}

###################################################################################################################################################



make_maternal_estimates <- function() {
	upload.maternal <- upload.ests[age_group_id==2 & sex_id==2]
	upload.maternal[,age_group_id:=22]
	upload.maternal[,sex_id:=3]
	upload.maternal[,covariate_id:=463]
	upload.maternal[,covariate_name_short:="maternal_educ_yrs_pc"]
	upload.maternal[,upper_value:=mean_value]
	upload.maternal[,lower_value:=mean_value]

	assert_ids(upload.maternal, id_vars = list(location_id = locs$location_id, year_id = 1950:2017, age_group_id = c(22), sex_id = 3))
	write.csv(upload.maternal,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/maternal_education_upload.csv"),row.names=F)

}
###################################################################################################################################################

make_agest_estimates <- function() {

	aw <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2018/ref/GBD_2017_ageweights_20171024.csv") # VS: right now same as GBD 2016, will need to update

	esties17 <- estimates[,.(mean17=mean(gpr_raked)),by=.(location_id,year_id,age_group_id,sex_id)]
	esties17 <- data.table(merge(esties17,aw,by='age_group_id'))
	upload.agestd <- esties17[,.(mean17=weighted.mean(x=mean17,w=age_group_weight_value)),by=.(location_id,year_id,sex_id)]
	setnames(upload.agestd,"mean17","mean_value")
	upload.agestd[,age_group_id:=22]
	upload.agestd[,covariate_id:=845]
	upload.agestd[,covariate_name_short:="educ_yrs_age_std_pc"]
	upload.agestd[,upper_value:=mean_value]
	upload.agestd[,lower_value:=mean_value]
	upload.agestd <- upload.agestd[year_id<=2017]
	assert_ids(upload.agestd, id_vars = list(location_id = locs$location_id, year_id = 1950:2017, age_group_id = c(22), sex_id = 1:2))
	write.csv(upload.agestd,paste0(jpath,"WORK/01_covariates/02_inputs/education/update_2018/data/output_data/",model_version,"/agestd_education_upload.csv"),row.names=F)
}

###################################################################################################################################################

