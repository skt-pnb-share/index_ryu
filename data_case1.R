# setwd("~/Desktop/index_tmp/visualization/timeuse-simulation")
setwd("/Users/1112055/Desktop/index_tmp/visualization/index-main_new")


# Terminal
cd Desktop/index_tmp/visualization/index-main_new
python -m SimpleHTTPServer 3141
#

library(magrittr)
library(data.table)


## read table

dat_all = fread('./files/dat_all_ryu.csv')
fread('./files/data') %>% head()

# column name
names(dat_all) = c('pid', 'grp', 'duration')

# pid + 1
dat_all[, pid := pid + 1]


# rowid -> -99

dat_all[, grp := paste0('d_', grp)]
dat_all[, rowid := rowid(pid)]
dat_all = dat_all[order(-rowid)]
dat_all[, rowid2 := rowid(pid)]
dat_all[rowid2 == 1, duration := -99]
dat_all = dat_all[order(pid, rowid)]

fwrite(dat_all[, 1:3], './files/data')
######

color_list = as.data.table(brewer.pal.info, keep.rownames = T) 
color_tb = NULL
for(i in 1:nrow(color_list)){
  
  color_tb = c(color_tb, brewer.pal(color_list[i, maxcolors], color_list[i, rn])) %>% unique()
}

color_tb_sido = sample(color_tb, 30)


brewer.pal

library(RColorBrewer)
brewer.pal(9, 'YlOrRd')
display.brewer.pal(9, 'YlOrRd')
display.brewer.all()

color_tb_sido = c(brewer.pal(9, 'YlOrRd'), '#661016', '#FFFFCC', '#FFFFCC')

##
sido_idx = 1
for( i in 3:7){
  for(j in 3:7){
    cat(  sprintf("'d_%s': { x: %s*width/10, y: %s*height/10, color: '%s', cnt: 0, fullname: '%s' },",
                  sido_idx-1,i,j, color_tb_sido[sido_idx],paste0('HOUR-', sido_idx-1)) %>% gsub("'", '"', .), '\n')
    
    sido_idx = sido_idx + 1
  }
  
}

########



## origin data 

dat_result = fread("./files/data")

## STEP 2 - SAMPLE DATA 

pid_list = 1:1000
grp_list = paste0('d_', c(1:17, 99))

dat_result = CJ(pid_list, grp_list) %>% setnames(c('pid', 'grp'))
dat_result[, duration := sample(3:10, nrow(dat_result), replace = T)]
dat_result = dat_result[sample(1:nrow(dat_result))]
dat_result[, rowid := rowid(pid)]
dat_result[rowid == 1, duration := -99]
dat_result = dat_result[order(-rowid, pid)]
dat_result[, rowid := NULL]

fwrite(dat_result, './files/data')

## STEP 3 -JJIN DATA

dat_result_2 = copy(dat_result)
sum(sido_tbl$min_cnt)

dat_result_2_default = NULL
for(i in 1:nrow(sido_tbl)){
  tmp = rep(sido_tbl[i, sido_num], sido_tbl[i, min_cnt])
  dat_result_2_default = c(dat_result_2_default, tmp)
}

dat_result_2_default = data.table(pid = 1:length(dat_result_2_default),
                                  grp = dat_result_2_default,
                                  duration = -99)

## TYPE 설정
## 왜냐면 뒤에서 움직이는 애들만 뽑아서 괜찮은 케이스 만들어야함
dat_result_2_backup = rbind(dat_result_2[pid > nrow(dat_result_2_default), 
                                         list(pid, grp, duration, type = 'move')],
                     dat_result_2_default[, list(pid, grp, duration, type = 'stop')])


dat_result_2 = copy(dat_result_2_backup[, list(pid, grp, duration)])

fwrite(dat_result_2, './files/data')


## STEP 4 - ALL JJIN DATA
dat_all
dat_min

# dat_result_2_backup

sub_pid_list = (nrow(dat_result_2_default)+1):1000
yw_list = unique(dat_all$yw) %>% sort

dat_result_3 = CJ(sub_pid_list, yw_list) %>% setnames(c('pid', 'yw'))
dat_result_3[, tmp := 'd_']
dat_result_3 = dcast(dat_result_3, pid ~ yw, value.var = 'tmp')

dat_result_3[, names(dat_result_3)[-1] := 
               lapply(names(dat_result_3)[-1], function(x){
                 
                 dat_all_tmp = dat_all[yw == x]
                 dat_all_tmp[, cnt := bubble_cnt - min_cnt]
                 
                 tmp_all = NULL
                 for(i in 1:nrow(dat_all_tmp)){
                   
                   tmp = rep(dat_all_tmp[i, sido_num], dat_all_tmp[i, cnt])
                   tmp_all = c(tmp_all, tmp)
                 }
                 #sort(tmp_all)
                 tmp_all
               })]



dat_result_3_t = t(as.matrix(dat_result_3, rownames = T)) %>% as.data.table(keep.rownames = T) %>% setnames('rn', 'yw')
dat_result_3_t_t = melt(dat_result_3_t, id.vars = 1, measure.vars = 2:ncol(dat_result_3_t))
dat_result_3_t_t = dat_result_3_t_t[order(variable, yw)]

# dat_result_3_t_t[, rowid := rowid(variable, value)]

dat_result_3_t_t[, value_lead1 := shift(value, n = -1), by = variable]
dat_result_3_t_t[, same_yn := ifelse(value == value_lead1, 1, 0)]
dat_result_3_t_t[, rownum := rowid(variable)]
# dat_result_3_t_t[, same_yn2 := ifelse(same_yn == 0, -rownum+1, same_yn)]
# dat_result_3_t_t[, duration := cumsum(same_yn2), by = variable]

dat_result_3_t_t_result = copy(dat_result_3_t_t[same_yn==0])
dat_result_3_t_t_result[, rownum_lag1 := ifelse(is.na(shift(rownum, n = 1)), 0, shift(rownum, n = 1)), by = variable]
dat_result_3_t_t_result[, duration := rownum - rownum_lag1]

dat_result_3 = dat_result_3_t_t_result[, list(pid = variable, 
                                                      grp = value,
                                                      duration)]

dat_result_3_dupl = NULL
for(i in 1:nrow(dat_result_3)){
  tmp = dat_result_3[i]
  
  dat_result_3_dupl = rbind(dat_result_3_dupl, tmp,tmp, tmp)
  
  
}

## -> 9676 row


# CEHCK
test_pid = sample(dat_result_3$pid, 1)

dat_result_3[pid == test_pid]
dat_result_3_t_t[variable == test_pid] %>% as.data.frame()


### ROW BIND



dat_result_3 = rbind(dat_result_3,
                     dat_result_2_default[, list(pid, grp, duration)])


fwrite(dat_result_3, './files/data')

fwrite(dat_result_3_dupl, './files/data')




## STEP 5 - bubble의 최소한의 이동


#### 위에서 정상적으로 만든것에서 최소한의 이동으로 변경하기 위해
#### 보통 버블이 몇개의 unique한 이동이 필요한지 확인..

result_3_sum = dat_result_3[, list(row_cnt = .N,
                                   grp_cnt = length(unique(grp))), by = pid]



###

sub_pid_list = (nrow(dat_result_2_default)+1):1000
yw_list = unique(dat_all$yw) %>% sort


test_all = NULL
for(i in 1:100){
  print(i)
  
  seednum = sample(1:10000, 1)
  
  
  dat_result_3 = CJ(sub_pid_list, yw_list) %>% setnames(c('pid', 'yw'))
  dat_result_3[, tmp := 'd_']
  dat_result_3 = dcast(dat_result_3, pid ~ yw, value.var = 'tmp')
  
  
  dat_result_3[, names(dat_result_3)[-1] := 
                 lapply(names(dat_result_3)[-1], function(x){
                   
                   dat_all_tmp = dat_all[yw == x]
                   dat_all_tmp[, cnt := bubble_cnt - min_cnt]
                   
                   tmp_all = NULL
                   for(i in 1:nrow(dat_all_tmp)){
                     
                     tmp = rep(dat_all_tmp[i, sido_num], dat_all_tmp[i, cnt])
                     tmp_all = c(tmp_all, tmp)
                   }
                   sort(tmp_all)
                   # sample(tmp_all, length(tmp_all))
                   
                 })]
  
  
  
  dat_result_3_t = t(as.matrix(dat_result_3, rownames = T)) %>% as.data.table(keep.rownames = T) %>% setnames('rn', 'yw')
  dat_result_3_t_t = melt(dat_result_3_t, id.vars = 1, measure.vars = 2:ncol(dat_result_3_t))
  dat_result_3_t_t = dat_result_3_t_t[order(variable, yw)]
  
  # dat_result_3_t_t[, rowid := rowid(variable, value)]
  
  dat_result_3_t_t[, value_lead1 := shift(value, n = -1), by = variable]
  dat_result_3_t_t[, same_yn := ifelse(value == value_lead1, 1, 0)]
  dat_result_3_t_t[, rownum := rowid(variable)]
  # dat_result_3_t_t[, same_yn2 := ifelse(same_yn == 0, -rownum+1, same_yn)]
  # dat_result_3_t_t[, duration := cumsum(same_yn2), by = variable]
  
  dat_result_3_t_t_result = copy(dat_result_3_t_t[same_yn==0])
  dat_result_3_t_t_result[, rownum_lag1 := ifelse(is.na(shift(rownum, n = 1)), 0, shift(rownum, n = 1)), by = variable]
  dat_result_3_t_t_result[, duration := rownum - rownum_lag1]
  
  dat_result_3 = dat_result_3_t_t_result[, list(pid = variable, 
                                                grp = value,
                                                duration)]
  
  test_tmp = data.table(seednum = seednum, nrow(dat_result_3))
  test_all = rbind(test_all, test_tmp)
  
  
}






########################################################################################################################
########################################################################################################################


"d_6": { x: width/3, y: height/3, color: "#79BACE", cnt: 0, fullname: "d_3" },




##
for( i in 3:7){
  for(j in 3:7){
    cat(  sprintf("'d_%s%s': { x: %s*width/10, y: %s*height/10, color: '#79BACE', cnt: 0, fullname: 'd_%s%s' },",
                  i-2,j-2,i,j,i-2,j-2) %>% gsub("'", '"', .), '\n')
    
  }
  
}




library(data.table)
##
ff = fread("./mb_jh_case1.csv")
ff[, rowid := rowid(pid)]
ff[, dur2 := sample(1:100, nrow(ff), replace = T)]
ff[rowid == 24, dur2 := -99]



write.table(ff[, list(pid, grp, duration = dur2)], file='da7eb4dcbbf0e87009d6a374c01b282e4310d6d53eddd6915fc04b41980a36d4667a4035773773e9b9e966c28477d566b732b21f0a28bfdb9bf420b21c166e87', quote=FALSE, sep='\t', col.names = NA)
fwrite(ff[, list(pid, grp, duration = dur2)], '6a533c858b154cac22f3ef89daae5a0fee936e7f7b4c0283136916247cafb2a2e0d6fae116f57963840d2ddec95077356f1d73f69b207eaa61f421d2605a9caf')



21, 41 52, 54, 25, 45, 12, 14
tmp = data.table(i = c(2, 4, 5, 5, 2, 4, 1, 1),
                 y = c(1, 1, 2, 4, 5, 5, 2, 4),
                 grp = c('0-3', '4-5', '6-7', '8-9', '10-11', '12-13', '14-15', '16-24'))
library(RColorBrewer)

for( n in 1:nrow(tmp)){
  i = tmp[n, i]
  y = tmp[n, y]
  grp = tmp[n, grp]
  
  cat(  sprintf("'%s': { x: %s*width/10, y: %s*height/10, color: '#79BACE', cnt: 0, fullname: '%s' },",
                grp, i, y, paste0('HH:', grp)) %>% gsub("'", '"', .), '\n')
  
}




