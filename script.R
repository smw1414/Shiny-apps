



cols<-colnames(res_tab)[!colnames(res_tab) %in% c("gene","type")]
res_tab[,(cols):=lapply(.SD, function(x) format(as.numeric(x),digits = 2,scientific = F)),.SDcols=cols]








