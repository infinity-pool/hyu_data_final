library(httr)
library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)

url = 'https://www.dhlottery.co.kr/gameResult.do?method=byWin'

# Find the recent draw no.
get_result = GET(url)
recent_draw_no = read_html(get_result) %>%
  html_nodes('.win_result') %>%
  html_nodes('h4') %>%
  html_text() %>%
  str_extract_all('\\d+') %>%
  unlist() %>%
  as.numeric()

all_lotto_nums = c()

for (drw_no in (recent_draw_no-9):recent_draw_no) {
  print(drw_no)
  
  data_lotto = POST(
    url, 
    body = list(
      drwNo = as.character(drw_no),
      dwrNoList = as.character(drw_no)
    )
  )
  
  data_lotto_html = data_lotto %>% read_html()
  
  lotto_nums = data_lotto_html %>%
    html_nodes('.num.win') %>%
    html_text() %>%
    str_extract_all('\\d+') %>%
    unlist()
  
  all_lotto_nums = c(all_lotto_nums, lotto_nums)
  
  Sys.sleep(2)
}

# 번호별 당첨 횟수 계산
lotto_num_counts = as.data.frame(table(all_lotto_nums))
colnames(lotto_num_counts) = c("number", "count")

# 번호를 숫자로 변환
#lotto_num_counts$number = as.numeric(lotto_num_counts$number)

# 모든 가능한 번호와 count를 포함하는 데이터프레임 생성
all_numbers = data.frame(number = factor(1:45), count = 0)

# 실제 데이터와 모든 가능한 번호 데이터프레임 결합 후 count 합산
combined_counts <- left_join(all_numbers, lotto_num_counts, by = "number", suffix = c(".all", ".lotto")) %>%
  mutate(count = count.all + coalesce(count.lotto, 0)) %>%
  select(number, count)

# 막대그래프 시각화
ggplot(combined_counts, aes(x = number, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Lotto Number Frequency", x = "Lotto Number", y = "Frequency") +
  theme_minimal()
