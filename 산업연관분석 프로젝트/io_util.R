#### 사용자 정의 함수
# 부문추출
value_take <- function(income,title){
  income<-as.data.frame(t(income))
  colnames(income) <- sec_names
  rownames(income) <-  paste(title)
  print(income)
}

# 승수도출
multiflier_tot <- function(income,
                           inp_mat,
                           tot_inp,
                           sec_names,
                           title0,# 소득계수
                           title1,# 소득승수
                           title3 # 소득승수행렬
                           
){
  income_coef=income/tot_inp
  rownames(income_coef) <-  paste(title0)
  income_coef
  income_mat<-diag(as.numeric(income_coef))%*%inp_mat # 억원 환산
  colnames(income_mat) <- sec_names
  income_mat_coef<-colSums(income_mat)
  income_mat_coef<-as.data.frame(income_mat_coef)
  colnames(income_mat_coef) <-  paste(title1)
  income_mat_coef=t(income_mat_coef)
  #rownames(income_mat_coef) <- sec_names
  value_income_list<-list(income_coef,income_mat,income_mat_coef)
  names(value_income_list) <- c(paste(title0),paste(title3),paste(title1))
  value_income_list
  
}
# 파급효과 도출
impact_tot <- function(income,
                       inp_mat,
                       tot_inp,
                       impact,
                       sec_names,
                       title4,
                       title5
){
  income_coef=income/tot_inp
  income_coef
  income_mat<-diag(as.numeric(income_coef))%*%inp_mat # 억원 환산
  colnames(income_mat) <- sec_names
  income_mat_coef<-colSums(income_mat)
  income_mat_coef<-as.data.frame(income_mat_coef)
  income_mat_coef=t(income_mat_coef)
  income_effect_sum1<-income_mat_coef%*%diag(as.numeric(impact))
  colnames(income_effect_sum1) <- sec_names
  income_effect_decom<-diag(as.numeric(impact))%*%income_mat # 억원 환산
  colnames(income_effect_decom) <- sec_names
  income_effect_sum2<-as.data.frame(t(colSums(income_effect_decom)))
  rownames(income_effect_sum2) <-  paste(title5)
  value_income<-list(income_effect_decom,income_effect_sum2)
  names(value_income) <- c(paste(title4),paste(title5))
  value_income
}

# 행렬통합 함수
merge_mat <- function(ori_mat,group){
  r_sum <- rowsum(ori_mat, group) #기준에 따라 가로합
  c_sum <- rowsum(t(r_sum), group) #기준에 따라 세로합
  merged_mat <- t(c_sum) # 전치를 해야 원래 행렬이 찾아짐
}

merge_col <- function(ori_mat,group){
  c_sum <- rowsum(t(ori_mat), group) #기준에 따라 가로합
  #c_sum <- rowsum(t(r_sum), group) #기준에 따라 세로합
  merged_mat <- t(c_sum) # 전치를 해야 원래 행렬이 찾아짐
}