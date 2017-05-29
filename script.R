


res_tab<-data.table(aa$res_tab)
cols<-colnames(res_tab)[!colnames(res_tab) %in% c("gene","type")]
res_tab[,lapply(.SD, function(x) formatC(as.numeric(x),digits =5,format = "g",width = 5)),.SDcols=cols]
res_tab[,(cols):=lapply(.SD, function(x) formatC(as.numeric(x),digits =5,format = "g",width = 5)),.SDcols=cols]








