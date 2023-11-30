if (!require(pacman)) {install.packages("pacman")
  library(pacman)}
pacman::p_load("openxlsx","DT","ggplot2","ggrepel")

source("C:\\Users\\donghyun\\Desktop\\r 관련 파일\\io_util.R")

# 산업분류_기본부문
# grp3=c(rep(1,25),rep(2,271),rep(3,85))
# length(grp3)
grp50=c(rep(1,25), rep(2,9), rep(3,26), rep(4,19), rep(5,15),
        rep(6,12), rep(7,28), rep(8,16), rep(9,22), rep(10,14),
        rep(11,22), rep(12,13), rep(13,21), rep(14,14), rep(15,10),
        rep(16,2), rep(17,7), rep(18,6), rep(19,15), rep(20,1),
        rep(21,1), rep(22,1), 25, rep(23,2), rep(24,1), rep(26,1), rep(25,7),
        rep(27,2), rep(28,1), rep(29,1), rep(30,1),
        rep(31,1), rep(32,1), rep(33,11), rep(34,1), rep(35,6), rep(36,4), rep(37,11), rep(38,1), rep(39,3), rep(40,1),
        rep(41,1), 42, rep(43,3), rep(44,5), rep(45,3), 46, rep(47,1), rep(48,1), rep(49,8), rep(50,1))
length(grp50)
# 산업분류_165_고용자수
# grp3_labor=c(rep(1,8),rep(2,109),rep(3,48))
# length(grp3_labor)
grp50_labor=c(rep(1,8), rep(2,4), rep(3,11), rep(4,6), rep(5,6),
              rep(6,3), rep(7,13), rep(8,5), rep(9,7), rep(10,4),
              rep(11,9), rep(12,6), rep(13,11), rep(14,7), rep(15,2),
              rep(16,1), rep(17,3), rep(18,4), rep(19,7), rep(20,1),        
              rep(21,1), rep(22,1), 25,  rep(23,1), rep(24,1), 26, rep(25,7), rep(27,2), 28, 29, 30, 31, 32,
              rep(33,7), rep(34,1), rep(35,3), rep(36,3), rep(37,5), rep(38,1), rep(39,3), rep(40,1), rep(41,1), 
              42, 43, rep(44,2), 45, 46, 47, 48, rep(49,3), 50)
length(grp50_labor)


# file.choose()
inter = read.xlsx("C:\\Users\\donghyun\\Desktop\\김동현\\산업연관분석 프로젝트\\투입산출표_생산자가격_기본부문 by Song.xlsx")
labor = read.xlsx("C:\\Users\\donghyun\\Desktop\\김동현\\산업연관분석 프로젝트\\내국인 고용표.xlsx")

# 내생시작점
inter[6,3]
labor[5,11]

# setting
n_sec_inter=381; n_sec_labor=175
n_vadded=6
start_inter=c(6,3); start_inter[1]; start_inter[2]
start_labor=c(5,11); start_labor[1]; start_labor[2]

# 내생부문 추출
inner=inter[start_inter[1]:(start_inter[1]+n_sec_inter-1),start_inter[2]:(start_inter[2]+n_sec_inter-1)]
inner=as.numeric(unlist(inner))
#class(inner)#; class(inner[,1])
inner=as.matrix(inner)
inner=as.numeric(inner)
inner=matrix(inner,ncol=n_sec_inter)
##View(inner)

# 고용자수 추출
#labor_num=labor[start_labor[1]:(start_labor[1]+n_sec_labor-1),start_labor[2]:(start_labor[2]+n_sec_labor-1)]
labor_num=labor[start_labor[1]:(start_labor[1]+n_sec_labor-1),start_labor[2]]
labor_num=as.numeric(unlist(labor_num))
#class(labor_num)#; class(labor_num[,1])
labor_num=as.matrix(labor_num)
labor_num=as.numeric(labor_num)
labor_num=matrix(labor_num,ncol=n_sec_labor)
##View(labor_num)

##내생부문 이름
#sec_names <- c("농업","제조업","서비스업")
sec_names_inter = inter[start_inter[1]:(start_inter[1]+n_sec_inter-1),2] # 산업의 이름들
rownames(inner) <- sec_names_inter
colnames(inner) <- sec_names_inter
##View(inner)

##고용자수 이름
#sec_names <- c("농업","제조업","서비스업")
sec_names_labor = labor[start_labor[1]:(start_labor[1]+n_sec_labor-1),2] # 산업의 이름들
#rownames(labor_num) <- sec_names_inter
colnames(labor_num) <- sec_names_labor
#View(labor_num)

# merge_mat: 행열 동시 통합
inner_merge=merge_mat(inner,grp50)
sec_names_inter_merge = sec_names <- c("농림수산품","광산품","음식료품","섬유_가죽제품","목재_종이_인쇄",
                                       "석탄_석유제품","화학제품","비금속광물제품","1차_금속제품","금속가공제품","컴퓨터_전자_광학기기",
                                       "전기장비","기계_장비","운송장비","기타_제조업_제품","제조임가공_산업용_장비_수리","전력_가스_증기","수도_폐기물처리_재활용서비스",
                                       "건설","도소매_및_상품중개서비스", "철도서비스","도로운송서비스", "수상운송서비스","항공운송서비스","기타화물운송및운송보조서비스","육상운송보조서비스","일반음식점서비스", "주점", 
                                       "비알콜음식점","숙박서비스","정보통신및방송서비스","무선및위성통신서비스","정보통신및방송서비스","은행서비스","금융및보험서비스","부동산서비스","전문과학및기술서비스",
                                       "장비및용품대여","사업지원서비스","공공행정(중앙정부)","공공행정(지방정부)","사회보장서비스","교육서비스","의료_서비스_및_사회복지_서비스","문화_및_예술서비스",
                                       "여행사서비스","스포츠서비스","오락서비스","기타서비스","기타")

rownames(inner_merge) <- sec_names_inter_merge
colnames(inner_merge) <- sec_names_inter_merge
#cat("기본부문을 33부분으로 통합한 내생부문")
#View(inner_merge)

# merge_col: 열만통합_노동자수
labor_merge <- merge_col(labor_num, grp50_labor)
rownames(labor_merge) <- c("노동자수")
colnames(labor_merge) <- sec_names_inter_merge
#cat("기본부문을 3부분으로 통합한 노동자수")
#paste("기본부문을 3부분으로 통합한 노동자수")
#View(labor_merge)

# 부가가치부문 도출
inter[388,3]
start_val=c(388,3)
value=inter[start_val[1]:(start_val[1]+n_vadded-1),start_inter[2]:(start_inter[2]+n_sec_inter-1)]
value=as.numeric(unlist(value))
#class(value)#; class(value[,1])
value=as.matrix(value)
value=as.numeric(value)
value=matrix(value,ncol=n_sec_inter);nrow(value)
value <- as.data.frame(value)
colnames(value) <- sec_names_inter
rownames(value) <- c("피용자보수","영업잉여","고정자본소모",
                     "생산세_보조금공제","부가가치계","총투입계")
#View(value)

# merge_col: 열만통합_부가가치
value_merge <- merge_col(value, grp50)
rownames(value_merge) <- c("피용자보수","영업잉여","고정자본소모",
                           "생산세_보조금공제","부가가치계","총투입계")
colnames(value_merge) <- sec_names_inter_merge
#cat("기본부문을 3부분으로 통합한 부가가치")
#paste("기본부문을 3부분으로 통합한 노동자수")
#View(value_merge)


# 총투입계 도출
value_merge[6,1]
start_total=c(6,1)

tot_inp=value_merge[6,]
##View(tot_inp)
# inner_value = rbind(inner,value[nrow(value),])
# tot_inp=as.numeric(unlist(tot_inp))
# tot_inp=as.matrix(tot_inp)
# tot_inp=as.numeric(tot_inp)
tot_inp <- as.data.frame(t(tot_inp))
# tot_inp=matrix(tot_inp,ncol=n_sec_inter)
#tot_inp=as.matrix(tot_inp)
rownames(tot_inp) <-  c("총투입계")
colnames(tot_inp) <- sec_names
print(dim(tot_inp))
print(tot_inp)
dim(tot_inp)
#View(tot_inp)

# A행렬: 기술계수행렬
tot_inp=as.numeric(tot_inp)
A_mat <- sweep(inner_merge,2, tot_inp, '/')
#View(A_mat)

I=diag(nrow(A_mat)) # 단위행렬
IminusA_reverse = solve(I-A_mat)
dim(IminusA_reverse)
inp_mat<-IminusA_reverse
#View(inp_mat)

# 생산승수: 생산승수유발행렬의 열합
inp_coef=as.matrix(t(colSums(inp_mat)))
rownames(inp_coef) <-  c("생산승수")
print(dim(inp_coef))
#View(inp_coef)

#View(sec_names_inter_merge)
#20: 도소매(쇼핑)
#21: 운송(관광교통)
#22: 음식점숙박(식음료_숙박)
#31: 예술스포츠여가(문화오락)

# 외부효과
#impact<-matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1510,1050,2230,618,0,0,0,0,288,0), nrow=30) # 고용효과를 쉽게 고려하기 위해 백만원 단위로 입력
#print(impact)
impact=c(rep(0,19),16210340,2318664,336750,76152,2544492,0,822060,7544071,358994,1210950,3123289,0,208917,0,0,141137,0,0,473844,0,1385900,2701026,0,0,0,338132,365512,387956,806505,0,0); length(impact)
length(impact)
#View(cbind(sec_names_inter_merge,t(impact)))
impact <- as.data.frame(impact)
datatable(impact)

impact<-as.data.frame(t(impact)); dim(impact)
rownames(impact) <- c("외부효과")
colnames(impact) <- sec_names
print(impact)
dim(impact)
datatable(impact)
# 생산파급효과
output_decom<-inp_mat%*%diag(as.numeric(impact)) # 억원 환산
colnames(output_decom) <- sec_names
#output_decom
output_tot<-colSums(output_decom)
output_tot<-as.data.frame(output_tot)
output_tot=t(output_tot)
rownames(output_tot) <-  c("생파효과_종합")
colnames(output_tot) <- sec_names
datatable(output_tot)
#생파효과_종합
output_coef_list<-list(inp_mat,inp_coef)
names(output_coef_list) <- c("생산유발승수행렬","생산승수")
#View(output_coef_list)
inp_coef=t(inp_coef)
datatable(inp_coef)

# 생산파급효과
output_decom<-inp_mat%*%diag(as.numeric(impact)) # 억원 환산
colnames(output_decom) <- sec_names
#output_decom
output_tot<-colSums(output_decom)
output_tot<-as.data.frame(output_tot)
output_tot=t(output_tot)
rownames(output_tot) <-  c("생파효과_종합")
colnames(output_tot) <- sec_names
datatable(output_tot)

# 소득계수 도출
# 소득추출
income = value_take(value_merge[1,],"피용자보수"); class(income)
# 소득승수도출
income_multiflier_tot=multiflier_tot(income,inp_mat,tot_inp,sec_names,
                                     "소득계수","소득승수","소득승수행렬")
# 소득파급효과도출
income_impact_tot=impact_tot(income,inp_mat,tot_inp,impact,sec_names,
                             "소득파급효과_분해","소득파급효과_종합")
income_multiflier_tot
income_impact_tot
# View(income_impact_tot)
# income_impact_tot$소득파급효과_종합 <- as.data.frame(income_impact_tot$소득파급효과_종합)
# colnames(income_impact_tot$소득파급효과_종합) <- sec_names

#이름이 적용이 안되면(소득승수)
income_1=colSums(income_multiflier_tot$소득승수)
income_1=t(income_1)
rownames(income_1) <- c("소득승수")
colnames(income_1) <- sec_names
income_1=t(income_1)
datatable(income_1)

r1 <- income_impact_tot$소득파급효과_종합
r1=t(r1)
datatable(r1)

# 간접세계수 도출
# 간접세추출
tax = value_take(value_merge[4,],"간접세"); class(tax)
# 간접세승수도출
tax_multiflier_tot=multiflier_tot(tax,inp_mat,tot_inp,sec_names,
                                  "간접세계수","간접세승수","간접세승수행렬")
# 간접세파급효과도출
tax_impact_tot=impact_tot(tax,inp_mat,tot_inp,impact,sec_names,
                          "간접세파급효과_분해","간접세파급효과_종합")
tax_multiflier_tot
tax_impact_tot

r2 <- tax_impact_tot$간접세파급효과_종합
r2=t(r2)
datatable(r2)

#이름이 적용이 안되면(간접세승수)
tax1=colSums(tax_multiflier_tot$간접세승수)
tax1=t(tax1)
rownames(tax1) <- c("간접세승수")
colnames(tax1) <- sec_names
tax1=t(tax1)
datatable(tax1)


# 부가가치계계수 도출
# 부가가치계추출
v_tot = value_take(value_merge[5,],"부가가치계"); class(v_tot)
# 부가가치계승수도출
v_tot_multiflier_tot=multiflier_tot(v_tot,inp_mat,tot_inp,sec_names,
                                    "부가가치계계수","부가가치계승수","부가가치계승수행렬")
# 부가가치계파급효과도출
v_tot_impact_tot=impact_tot(v_tot,inp_mat,tot_inp,impact,sec_names,
                            "부가가치계파급효과_분해","부가가치계파급효과_종합")
v_tot_multiflier_tot
v_tot_impact_tot

#이름이 적용이 안되면(부가가치승수)
v_tot1=colSums(v_tot_multiflier_tot$부가가치계승수)
v_tot1=t(v_tot1)
rownames(v_tot1) <- c("부가가치승수")
colnames(v_tot1) <- sec_names
v_tot1=t(v_tot1)
datatable(v_tot1)


r3 <- v_tot_impact_tot$부가가치계파급효과_종합
r3=t(r3)
datatable(r3)
# 고용계수 도출
# 고용추출
labor_merge
rownames(labor_merge) <- c("고용자수")
# 고용승수도출
labor_multiflier_tot=multiflier_tot(labor_merge,inp_mat,tot_inp,sec_names,
                                    "고용계수","고용승수","고용승수행렬")
# 고용파급효과도출
labor_impact_tot=impact_tot(labor_merge,inp_mat,tot_inp,impact,sec_names,
                            "고용파급효과_분해","고용파급효과_종합")
labor_multiflier_tot
labor_impact_tot
View(labor_impact_tot)


#이름이 적용이 안되면(부가가치승수)
labor1=colSums(labor_multiflier_tot$고용승수)
labor1=t(labor1)
rownames(labor1) <- c("고용승수")
colnames(labor1) <- sec_names
labor1=t(labor1)
datatable(labor1)

r4 <- labor_impact_tot$고용파급효과_종합
r4=t(r4)
datatable(r4)

