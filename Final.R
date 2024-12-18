library(tidyverse)
budget <-  read_csv("102年度新北市總預算歲入來源別預算比較總表（經資併計）_export.csv")
glimpse(budget)
budget <- budget %>%
  rename(
    項目 = field1,
    本年度預算數 = field2,
    上年度預算數 = field3,
    前年度決算數 = field4,
    本年度與上年度比較 = field5
  )
glimpse(budget)
library(shiny)
library(tidyverse)
library(scales)

# 數據準備
budget <- tribble(
  ~項目, ~本年度預算數, ~上年度預算數, ~前年度決算數, ~本年度與上年度比較,
  "稅課收入-房屋稅", 9700000, 8843000, 9029156, 857000,
  "稅課收入-契稅", 2200000, 3057000, 2674830, -857000,
  "稅課收入-娛樂稅", 300000, 244843, 248966, 55157,
  "稅課收入-使用牌照稅", 7900000, 7914000, 7840597, -14000,
  "稅課收入-印花稅", 1080000, 1059629, 990088, 20371,
  "稅課收入-菸酒稅", 1503694, 1500003, 1359751, 3691,
  "稅課收入-土地稅", 26460000, 25521438, 24904412, 938562,
  "稅課收入-遺產及贈與稅", 1725000, 1512000, 1630211, 213000,
  "稅課收入-統籌分配稅", 25914067, 24590568, 22845415, 1323499,
  "工程受益費收入-工程受益費收入", 0, 0, 7438, 0,
  "罰款及賠償收入-罰金罰鍰及怠金", 3830373, 3787097, 2305424, 43276,
  "罰款及賠償收入-沒入及沒收財物", 19000, 20000, 22955, -1000,
  "罰款及賠償收入-賠償收入", 123048, 130500, 241903, -7452,
  "規費收入-行政規費收入", 1587396, 1640502, 2168191, -53106,
  "規費收入-使用規費收入", 1610510, 1682581, 1768043, -72071,
  "財產收入-財產孳息", 924910, 624209, 518892, 300701,
  "財產收入-財產售價", 2122985, 2520325, 162045, -397340,
  "財產收入-財產作價", 10089337, 220705, 47003, 9868632,
  "財產收入-投資收回", 0, 0, 14490, 0,
  "財產收入-廢舊物資售價", 26465, 22518, 39221, 3947,
  "營業盈餘及事業收入-非營業特種基金賸餘繳庫", 4433569, 9365330, 10854400, -4931761,
  "營業盈餘及事業收入-投資收益", 19947, 0, 255999, 19947,
  "補助及協助收入-上級政府補助收入", 26964475, 38110139, 45871676, -11145664,
  "捐獻及贈與收入-捐獻收入", 1401237, 1157092, 1947170, 244145,
  "自治稅捐收入-臨時稅課", 0, 0, 19678, 0,
  "其他收入-學雜費收入", 0, 127271, 118629, -127271,
  "其他收入-雜項收入", 3730350, 5071799, 4916347, -1341449,
  "總計", 133666362, 138722549, 142802930, -5056186
)

# 數據轉為長格式
budget_long <- budget %>%
  pivot_longer(
    cols = c(本年度預算數, 上年度預算數, 前年度決算數),
    names_to = "年度",
    values_to = "金額"
  )

# 定義 UI
ui <- fluidPage(
  titlePanel("新北市預算數據視覺化"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "selected_items",
        label = "選擇要展示的項目（最多12個）：",
        choices = unique(budget_long$項目),
        selected = NULL  # 預設不選擇任何項目
      ),
      textOutput("warning_text")  # 顯示警告文字
    ),
    
    mainPanel(
      plotOutput("budget_plot", height = "900px")  # 增加圖表高度
    )
  )
)

# 定義 Server
server <- function(input, output, session) {
  
  # 限制選取的項目數量
  observeEvent(input$selected_items, {
    if (length(input$selected_items) > 12) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_items",
        selected = head(input$selected_items, 12)  # 保留前 12 個選項
      )
    }
  })
  
  # 顯示警告訊息
  output$warning_text <- renderText({
    if (length(input$selected_items) > 12) {
      "警告：最多只能選擇12個項目，已自動保留前12個選項！"
    } else if (is.null(input$selected_items) || length(input$selected_items) == 0) {
      "請選擇至少一個項目以顯示圖表。"
    } else {
      NULL
    }
  })
  
  # 繪製圖表
  output$budget_plot <- renderPlot({
    if (is.null(input$selected_items) || length(input$selected_items) == 0) {
      return(NULL)  # 如果未選擇項目，不顯示圖表
    }
    
    filtered_data <- budget_long %>%
      filter(項目 %in% input$selected_items)
    
    ggplot(filtered_data, aes(x = 年度, y = 金額, fill = 年度)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = scales::comma(金額)), 
                position = position_dodge(width = 0.8, preserve = "single"), 
                size = 2.5, vjust = -0.3, hjust = -0.0025) +  # 調整標籤位置
      facet_wrap(~ 項目, scales = "free_y") +
      labs(
        title = "年度預算比較",
        x = "年度",
        y = "金額 (新台幣)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) +
      scale_y_continuous(
        labels = label_comma(),
        expand = expansion(mult = c(0, 0.2))  # 增加上方空間，避免文字擠出
      )
  })
}

# 啟動 Shiny 應用
shinyApp(ui = ui, server = server)
















