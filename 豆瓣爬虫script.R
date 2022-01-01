if (!require("stringr")) 
  install.packages("stringr")
if (!require("rvest")) 
  install.packages("rvest") 

df_douban_250 = data.frame()
for(page_number in 0:9){
  content = read_html(str_c(
    "https://movie.douban.com/top250?start=", 25*page_number, "&filter="))

  middle_text = content %>% html_nodes(
    xpath="//div[@class='info']//div[@class='bd']//p[@class][1]") %>% html_text()
  middle_text_line_1 = sapply(str_split(str_trim(middle_text), '\n'), "[[", 1)  #[1] "导演: 弗兰克·德拉邦特 Frank Darabont   主演: 蒂姆·罗宾斯 Tim Robbins /..."
  middle_text_line_2 = sapply(str_split(str_trim(middle_text), '\n'), "[[", 2)  #[1] "1994 / 美国 / 犯罪 剧情"
  
  m_director = sapply(str_split(middle_text_line_1, "[\u00A0]{3}"), "[[", 1)  # 注意:  &nbsp;字符需要用unicode去匹配
  m_actor = str_split(middle_text_line_1, "[\u00A0]{3}")
  for(i in 1:length(m_actor)){
    if(length(m_actor[[i]])<2){m_actor[[i]][2]=''}  # 补齐操作
  }
  m_actor = sapply(m_actor, "[[", 2)
  
  list_year = str_split(middle_text_line_2, "/")
  for(i in 1:length(list_year)){
    if(length(list_year[[i]])>3){  # 处理No.054的异常数据
      list_year[[i]][3] = list_year[[i]][length(list_year[[i]])]
      list_year[[i]][2] = list_year[[i]][length(list_year[[i]])-1]
    }
  }
  m_year = str_trim(sapply(list_year, "[[", 1))
  m_country = str_trim(sapply(list_year, "[[", 2))
  m_type = str_trim(sapply(list_year, "[[", 3))
  
  m_name = content %>% html_nodes(
    xpath="//span[@class='title'][1]") %>% html_text()
  m_score = content %>% html_nodes(
    xpath="//div[@class='info']//div[@class='bd']//div//span[@class='rating_num']") %>% html_text()
  m_commit_count = content %>% html_nodes(
    xpath="//div[@class='info']//div[@class='bd']//div//span[last()]") %>% html_text()
  m_commit_count = str_sub(m_commit_count, end=-4)
  m_detail_url = content %>% html_nodes(
    xpath="//div[@class='info']//div[@class='hd']//a[1]") %>% html_attr('href')
  
  df_one_page = data.frame(
    m_name,  # 电影名称
    m_director,  # 导演
    m_actor,  # 主演
    m_year,  # 上映年份
    m_country,  # 国家
    m_type,  # 所属分类
    m_score,  # 评分
    m_commit_count,  # 评论人数
    m_detail_url  # 详细页面
  )
  df_douban_250 = rbind(df_douban_250, df_one_page)
}