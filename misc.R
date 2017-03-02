ACLI_Average_Spread = fread(file.path(my_folder,"Input\\Average Spread\\Average_spread_ACLI.csv"))

first_orig = min(loan_detail$first_origdate)
last_month = max(loan_detail$period_month)


seq_date = months_diff_Date(date_from = first_orig,date_to = last_month)
dates = as.data.table(as.Date(sapply(seq(0:seq_date), FUN = function(x) {add_months_Date(date = first_orig, add_month = x )})))
names(dates) = "Month"
dates[,Qrt := ifelse(substr(dates$Month,6,7)%in%c("01","02","03"),"Q1",
                     ifelse(substr(dates$Month,6,7)%in% c("04","05","06"),"Q2",
                            ifelse(substr(dates$Month,6,7)%in% c("07","08","09"),"Q3","Q4")))]
dates[,Year_Qrt:=paste0(substr(Month,1,4),Qrt)]
ACLI_average_spread_interp = merge(dates,ACLI_Average_Spread,by.x="Year_Qrt",by.y="DATE_",all.x=T)
ACLI_average_spread_interp$Average_Spread_ACLI_nextQ=c(ACLI_average_spread_interp$Average_Spread_ACLI[-c(1:3)],NA,NA,NA)
ACLI_average_spread_interp[,diff:=Average_Spread_ACLI_nextQ-Average_Spread_ACLI]


ACLI_average_spread_interp$num=rep(c(0,1,2),nrow(ACLI_average_spread_interp)/3)
ACLI_average_spread_interp[,Average_Spread_Interp_ACLI:=Average_Spread_ACLI+diff/3*num]
ACLI_average_spread_interp[,Average_Spread_Interp_ACLI:=na.locf(Average_Spread_Interp_ACLI)]

ACLI_average_spread_interp_Trepp = merge(ACLI_average_spread_interp,average_spread_historical_Trepp,by.x="Month",by.y="Date",all.x=T)

ACLI_average_spread_interp_Trepp[,diff_ACLI_Trepp:=Average_Spread_Interp_ACLI-average_spread_RT_interp]

ACLI_average_spread_interp_Trepp[,average_spread_RT_L1_interp_ACLI:=average_spread_RT_L1_interp+diff_ACLI_Trepp]

average_spread_historical = ACLI_average_spread_interp_Trepp[,c("Month","Average_Spread_Interp_ACLI","average_spread_RT_L1_interp_ACLI"),with=F]
colnames(average_spread_historical) = colnames(average_spread_historical_Trepp)
