# Mini-Data-Analysis-project-
폭염 시 보행자 열노출 취약지역 분석 및 그늘막 우선 설치 후보지 선정

getwd()
#
if (!require(showtext)) install.packages("showtext")
library(showtext)
showtext_auto() 
knitr::opts_chunk$set(dev = "png")

#####1. 패키지 로드 
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(leaflet)
library(ggspatial)
library(httr)
library(jsonlite)
library(prettymapr)

####2. 데이터 로드
shade_gawn <- read.csv("다랩/서울특별시_관악구_그늘막쉼터_20250807.csv")
shade_young <- read.csv("다랩/서울특별시_영등포구_그늘막쉼터_20250801.csv")
dong_kor <- read_csv("다랩/행정동 단위 서울 생활인구(내국인)/행정동 단위 서울 생활인구(내국인).csv")
dong_fordan <- read_csv("다랩/행정동 단위 서울 생활인구(단기체류 외국인)/행정동 단위 서울 생활인구(단기체류 외국인).csv")
dong_forjang <- read_csv("다랩/행정동 단위 서울 생활인구(장기체류 외국인)/행정동 단위 서울 생활인구(장기체류 외국인).csv")
crosswalk_df <- read_csv("다랩/서울특별시_자치구별 횡단보도 위치 및 현황_20230530.csv", 
                         locale = locale(encoding = "CP949"),
                         show_col_types = FALSE)
str(shade_gawn)
str(shade_young)
str(dong_kor)
str(dong_fordan)
str(dong_forjang)
table(dong_fordan$시간대구분)
str(crosswalk_df)

####3. 전처리 
# 행정동 코드에 따라 새로운 df 생성 영등포구 - 11560~ / 관악구 - 11620 ~

cl_dong_kor <- dong_kor %>% filter(grepl("11560", as.character(dong_kor$행정동코드))|grepl("11620", as.character(dong_kor$행정동코드)))
cl_dong_fordan <- dong_fordan %>% filter(grepl("11560", as.character(dong_fordan$행정동코드))|grepl("11620", as.character(dong_fordan$행정동코드)))
cl_dong_forjang <- dong_forjang %>% filter(grepl("11560", as.character(dong_forjang$행정동코드))|grepl("11620", as.character(dong_forjang$행정동코드)))

str(cl_dong_kor)
str(cl_dong_forjang)
str(cl_dong_fordan)

# chr -> num 변수로 변환 

clean_and_numeric <- function(x) {
  cleaned <- str_remove_all(x, '[\\",]')
  as.numeric(cleaned)
}

# 1. 내국인 데이터 정제

cl_dong_kor <- cl_dong_kor %>%
  mutate(여자70세이상생활인구수 = clean_and_numeric(여자70세이상생활인구수)) %>%
  rename(총생활인구_내국인 = 총생활인구수)

# 2. 장기체류 외국인 데이터 정제

cl_dong_forjang <- cl_dong_forjang %>%
  mutate(중국외외국인체류인구수 = clean_and_numeric(중국외외국인체류인구수)) %>%
  rename(
    총생활인구_장기 = 총생활인구수,
    중국인_장기     = 중국인체류인구수,
    중국외_장기     = 중국외외국인체류인구수
  )

# 3. 단기체류 외국인 데이터 정제

cl_dong_fordan <- cl_dong_fordan %>%
  mutate(중국외외국인체류인구수 = clean_and_numeric(중국외외국인체류인구수)) %>%
  rename(
    총생활인구_단기 = 총생활인구수,
    중국인_단기     = 중국인체류인구수,
    중국외_단기     = 중국외외국인체류인구수
  )

# 행정동 단위 생활인구 데이터 통합

join_keys <- c("기준일ID", "시간대구분", "행정동코드")

combin_df <- cl_dong_kor %>%
  full_join(cl_dong_forjang, by = join_keys) %>%
  full_join(cl_dong_fordan, by = join_keys)

final_df <- combin_df %>%
  mutate(
    총생활인구_장기 = coalesce(총생활인구_장기, 0),
    총생활인구_단기 = coalesce(총생활인구_단기, 0),
    
    총생활인구_외국인 = 총생활인구_장기 + 총생활인구_단기,
    전체_총생활인구   = 총생활인구_내국인 + 총생활인구_외국인
  )

glimpse(final_df)

# 폭염 취약인구 정의 (어린아이 + 노인 / 외국인)

final_df <- final_df %>%
  mutate(
    폭염취약_내국인 = 남자0세부터9세생활인구수 + 여자0세부터9세생활인구수 + 
      남자65세부터69세생활인구수 + 남자70세이상생활인구수 + 
      여자65세부터69세생활인구수 + 여자70세이상생활인구수,
    
    폭염취약_외국인 = 총생활인구_외국인,
    
    총_폭염취약인구 = 폭염취약_내국인 + 폭염취약_외국인
  )

# 낮 시간 heatwave time으로 정의

heatwave_summary <- final_df %>%
  filter(시간대구분 %in% c("11", "12", "13", "14", "15", "16", "17")) %>%
  group_by(행정동코드) %>%
  summarise(
    평균_총생활인구 = mean(전체_총생활인구, na.rm = TRUE),
    평균_취약인구   = mean(총_폭염취약인구, na.rm = TRUE)
  )

# 그늘막 데이터 통합

shade_total <- bind_rows(shade_gawn, shade_young)
str(shade_total)



# 4. EDA 


# 관악구 그늘막  - 인터넷 지도

leaflet(shade_gawn) %>%
  addTiles() %>%  
  addMarkers(
    lng = ~경도,   # 경도 컬럼
    lat = ~위도,   # 위도 컬럼
    popup = ~그늘막유형 
  )

# 관악구 그늘막 좌표계에 표시 
geo_sf <- st_as_sf(shade_gawn, coords = c("경도", "위도"))

ggplot() +
  geom_sf(data = geo_sf, aes(color = 그늘막유형), size = 3) +
  theme_minimal() +
  labs(title = "관악구 그늘막 위경도 좌표", x = "경도", y = "위도")


# 영등포 그늘막 - 인터넷 지도 
leaflet(shade_young) %>%
  addTiles() %>%  
  addMarkers(
    lng = ~경도,   # 경도 컬럼 
    lat = ~위도,   # 위도 컬럼 
    popup = ~그늘막유형 
  )

# 영등포 그늘막 - 좌표계 
geo_sf2 <- st_as_sf(shade_young, coords = c("경도", "위도"))

ggplot() +
  geom_sf(data = geo_sf2, aes(color = 그늘막유형), size = 3) +
  theme_minimal() +
  labs(title = "영등포구 그늘막 위경도 좌표", x = "경도", y = "위도")

# 행정구 통합 EDA

leaflet(shade_total) %>%
  addTiles() %>%  
  addMarkers(
    lng = ~경도,   
    lat = ~위도,   
    popup = ~paste0("[", 시군구명, "] ", 설치장소명, " (", 그늘막유형, ")") 
  )

geo_total_sf <- st_as_sf(shade_total, coords = c("경도", "위도"), crs = 4326)

ggplot() +
  geom_sf(data = geo_total_sf, aes(color = 시군구명), size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "관악구·영등포구 통합 그늘막 위치 분포",
    x = "경도", y = "위도",
    color = "행정구"
  )

# 5. 그늘막 위치 효과 통계적 검정 

######## 공간데이터로 변환 후 좌표계 변환 
shade_sf <- st_as_sf(shade_total, coords = c("경도", "위도"), crs = 4326)

######### api key
my_vworld_key <- "My_api_key"
my_domain <-  "My_domain"
######## 3. 위경도 좌표를 넣으면 행정동명을 찾아주는 함수 정의
get_administrative_dong <- function(lng, lat) {
  url <- paste0("https://api.vworld.kr/req/address?service=address&request=getAddress&version=2.0&crs=epsg:4326&type=both",
                "&point=", lng, ",", lat, 
                "&key=", my_vworld_key,
                "&domain=", my_domain)
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      result_json <- fromJSON(rawToChar(response$content), simplifyVector = TRUE)
      
      # API 상태가 성공(OK)일 때만 작동
      if (result_json$response$status == "OK") {
        structure_df <- result_json$response$result$structure
        
        # 1순위: 행정동명(level4A) 컬럼이 존재하고 값이 있다면 추출
        if ("level4A" %in% colnames(structure_df)) {
          dong_vec <- structure_df$level4A[!is.na(structure_df$level4A) & structure_df$level4A != ""]
          if (length(dong_vec) > 0) return(dong_vec[1])
        }
        
        # 2순위: 만약 행정동(level4A)이 누락되었다면 법정동(level3, 예: 봉천동)이라도 차선책으로 반환
        if ("level3" %in% colnames(structure_df)) {
          dong_vec <- structure_df$level3[!is.na(structure_df$level3) & structure_df$level3 != ""]
          if (length(dong_vec) > 0) return(dong_vec[1])
        }
      }
    }
    return(NA) # 위 조건에 안 맞으면 NA 반환
  }, error = function(e) {
    return(NA) 
  })
}


shade_with_dong <- shade_total %>%
  rowwise() %>%
  mutate(행정동명 = get_administrative_dong(경도, 위도)) %>%
  ungroup()

####### API 불러온 결과 확인
str(shade_with_dong)
head(shade_with_dong %>% select(설치장소명, 행정동명))


#[unnamed-chunk-1-2.pdf](https://github.com/user-attachments/files/29750310/unnamed-chunk-1-2.pdf)
 행정동별 그늘막 개수 집계 
shade_counts <- shade_with_dong %>%
  filter(!is.na(행정동명)) %>% 
  group_by(행정동명) %>%
  summarise(그늘막개수 = n())

dong_mapping <- data.frame(
  행정동코드 = c(
    # 관악구 (21개 동)
    11620525, 11620545, 11620565, 11620575, 11620585, 11620595, 11620605, 
    11620615, 11620625, 11620630, 11620645, 11620655, 11620665, 11620675, 
    11620685, 11620695, 11620715, 11620725, 11620735, 11620745, 11620775,
    # 영등포구 (18개 동)
    11560515, 11560535, 11560540, 11560550, 11560560, 11560585, 11560605, 
    11560610, 11560620, 11560630, 11560650, 11560670, 11560680, 11560690, 
    11560700, 11560710, 11560720, 11560730
  ),
  행정동명 = c(
    # 관악구 행정동명
    "보라매동", "청룡동", "행운동", "낙성대동", "인헌동", "남현동", "서원동", 
    "신원동", "서림동", "신사동", "신림동", "난향동", "조원동", "대학동", 
    "은천동", "성현동", "청림동", "중앙동", "삼성동", "미성동", "난곡동",
    # 영등포구 행정동명
    "영등포본동", "영등포동", "여의동", "당산제1동", "당산제2동", "도림동", "문래동", 
    "양평제1동", "양평제2동", "신길제1동", "신길제3동", "신길제4동", "신길제5동", "신길제6동", 
    "신길제7동", "대림제1동", "대림제2동", "대림제3동"
  ),
  stringsAsFactors = FALSE
)

# 2. 생활인구 요약 데이터와 정밀 결합

heatwave_summary_named <- heatwave_summary %>%
  left_join(dong_mapping, by = "행정동코드")
head(heatwave_summary_named)

analysis_df <- heatwave_summary_named %>%
  left_join(shade_counts, by = "행정동명") %>%
  mutate(그늘막개수 = coalesce(그늘막개수, 0))

# 상관관계 분석 
cor_result <- cor.test(analysis_df$평균_총생활인구, analysis_df$그늘막개수)
print(cor_result)

# 낮시간 때 생활인구와 그늘막 개수 사이에는 양의 상관관계를 보인다. 
# p-value = 0.005 이므로 reject H0 통계적으로 유의하다.  

# 취약인구 선형회귀분석 

model <- lm(그늘막개수 ~ 평균_취약인구, data = analysis_df)
analysis_df <- analysis_df %>%
  mutate(
    예측값 = predict(model),
    잔차 = residuals(model)
  )

# 취약인구 유동 대비 그늘막이 가장 부족한 취약 행정동 Top 5 도출

vulnerable_dongs <- analysis_df %>%
  filter(잔차 < 0) %>%
  arrange(잔차) %>% 
  select(행정동코드, 행정동명, 평균_취약인구, 그늘막개수, 잔차)

print(head(vulnerable_dongs, 5))

'
1위: 영등포구 문래동 

2위: 관악구 남현동 

3위: 관악구 난곡동 

4위: 관악구 삼성동 

5위: 영등포구 대림제1동 
'
# 전체유동인구 선형회귀분석 

model <- lm(그늘막개수 ~ 평균_총생활인구, data = analysis_df)
analysis_df <- analysis_df %>%
  mutate(
    예측값 = predict(model),
    잔차 = residuals(model)
  )

# 전체인구 유동 대비 그늘막이 가장 부족한 취약 행정동 Top 5 도출

vulnerable_dongs2 <- analysis_df %>%
  filter(잔차 < 0) %>%
  arrange(잔차) %>% 
  select(행정동코드, 행정동명, 평균_총생활인구, 그늘막개수, 잔차)

print(head(vulnerable_dongs2, 5))

'
1위: 영등포구 문래동

2위: 관악구 삼성동 

3위: 관악구 남현동 

4위: 관악구 인헌동 

5위: 관악구 난곡동 
'


# 5. 신규 후보지 선정 후 제안 
cw_filtered <- crosswalk_df %>%
  filter(자치구 == "관악구" | (자치구 == "영등포구" & str_detect(주소, "문래동")))

# 횡단보도 데이터를 고유 투영좌표계로 변경 
cw_sf <- st_as_sf(cw_filtered, coords = c("X좌표", "Y좌표"), crs = 5186)


# 기존 그늘막 100m 권역 버퍼 생성 및 소외 지역 추출
#좌표변환 
shade_sf_5186 <- st_as_sf(shade_with_dong, coords = c("경도", "위도"), crs = 4326) %>%
  st_transform(crs = 5186)

#기존 그늘막들 주변으로 반경 100m 원(버퍼) 그리기
shade_buffer <- st_buffer(shade_sf_5186, dist = 100)
shade_union <- st_union(shade_buffer)

#[소외 지역 추출]
uncovered_idx <- st_disjoint(cw_sf, shade_union, sparse = FALSE)
uncovered_cw <- cw_sf[uncovered_idx[, 1], ]

uncovered_cw_4326 <- st_transform(uncovered_cw, crs = 4326)
uncovered_cw_4326 <- uncovered_cw_4326 %>%
  mutate(
    경도_4326 = st_coordinates(.)[,1],
    위도_4326 = st_coordinates(.)[,2]
  )

candidate_crosswalks <- uncovered_cw_4326 %>%
  st_drop_geometry() %>% # 빠른 연산을 위해 공간 정보 잠시 해제
  rowwise() %>%
  mutate(행정동명 = get_administrative_dong(경도_4326, 위도_4326)) %>%
  ungroup()

#텍스트 처리 
candidate_crosswalks <- candidate_crosswalks %>%
  mutate(행정동명 = str_remove_all(행정동명, "제"))

# 최종 후보지 도출 

arget_dongs <- c("문래동", "삼성동", "남현동", "인헌동", "난곡동")

final_candidates <- candidate_crosswalks %>%
  filter(행정동명 %in% arget_dongs) %>%
  select(자치구, 행정동명, 주소, 교차로명, 횡단보도종류, 경도_4326, 위도_4326)

print(final_candidates)

# 최종 후보지 시각화. 
shade_spatial <- st_as_sf(shade_with_dong, coords = c("경도", "위도"), crs = 4326)
candidates_spatial <- st_as_sf(final_candidates, coords = c("경도_4326", "위도_4326"), crs = 4326)

# 좌표계에 점찍기 
ggplot() +
  #기존 그늘막 분포
  geom_sf(data = shade_spatial, aes(color = "기존 그늘막"), size = 1.5, alpha = 0.5) +
  
  #최종 우선 설치 추천 후보 횡단보도 
  geom_sf(data = candidates_spatial, aes(color = "우선 설치 후보지(횡단보도)"), size = 2.5, alpha = 0.8) +
  
  
  scale_color_manual(values = c("기존 그늘막" = "#4A90E2", "우선 설치 후보지(횡단보도)" = "#D0021B")) +
  theme_minimal(base_family = "NanumGothic") + # 한글 깨짐 방지를 위해 컴퓨터 내 한글 폰트 지정
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "#E2E2E2", size = 0.2)
  ) +
  labs(
    title = "관악구·영등포구 유동인구 취약지역 내 그늘막 미설치 사각지대",
    x = "경도", y = "위도"
  )

# 좌표계 + 지도 

ggplot() +
  #줌 수치 조절 
  annotation_map_tile(type = "osm", zoom = 14, alpha = 0.8) + 
  
  #기존 그늘막 점 찍기 (파란색)
  geom_sf(data = shade_spatial, aes(color = "기존 그늘막"), size = 1.5, alpha = 0.4) +
  
  #최종 우선 설치 후보 횡단보도 점 찍기 (빨간색)
  geom_sf(data = candidates_spatial, aes(color = "우선 설치 후보지"), size = 3, alpha = 0.9) +

  scale_color_manual(values = c("기존 그늘막" = "#3182bd", "우선 설치 후보지" = "#e34a33")) +
  theme_minimal(base_family = "NanumGothic") + # 한글 폰트 설정
  labs(
    title = "관악구·영등포구 그늘막 서비스 사각지대 및 우선 설치 후보지",
    x = "경도", y = "위도"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank()
  )


# interactive 지도 

candidates_popup <- paste0(
  "<div style='font-family: Arial; padding: 5px;'>",
  "<h4 style='margin: 0 0 5px 0; color: #D0021B;'>★ 최우선 설치 후보지</h4>",
  "<b>위치:</b> ", final_candidates$자치구, " ", final_candidates$행정동명, "<br>",
  "<b>주소:</b> ", final_candidates$주소, "<br>",
  "<b>교차로명:</b> ", ifelse(final_candidates$교차로명 == "-", "일반 교차로", final_candidates$교차로명), "<br>",
  "<b>유형:</b> ", final_candidates$횡단보도종류,
  "</div>"
)


leaflet() %>%
  addTiles() %>% 
  # 기존 그늘막
  addCircleMarkers(
    data = final_candidates, # 또는 shade_with_dong
    lng = ~경도_4326, lat = ~위도_4326,
    radius = 4, color = "#4A90E2", fillColor = "#4A90E2",
    fillOpacity = 0.6, weight = 1,
    group = "기존 그늘막"
  ) %>%
  
  # 최종 후보 횡단보도 지점 
  addMarkers(
    data = final_candidates,
    lng = ~경도_4326, lat = ~위도_4326,
    popup = candidates_popup,
    group = "우선 설치 후보지"
  ) %>%

  addLayersControl(
    overlayGroups = c("기존 그늘막", "우선 설치 후보지"),
    options = layersControlOptions(collapsed = FALSE)
  )





