## -------------------------------------------------
## 1. 加载包
## -------------------------------------------------
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(patchwork)
## -------------------------------------------------
## 2. 读取数据
## -------------------------------------------------
data_raw <- read.csv("data/final_lagged_dataset_for_icd.csv")


###心率自回归
HR_t<-data_raw$HR.t.
HR_t_1<-data_raw$HR.t.1.
HR_t_2<-data_raw$HR.t.2.
Cal_t<-data_raw$Cal.t.
Cal_t_1<-data_raw$Cal.t.1.
Cal_t_2<-data_raw$Cal.t.2.
METs_t<-data_raw$METs.t.
METs_t_1<-data_raw$METs.t.1.
METs_t_2<-data_raw$METs.t.2.
Steps_t<-data_raw$Steps.t.
Steps_t_1<-data_raw$Steps.t.1.
Steps_t_2<-data_raw$Steps.t.2.


df <- data.frame(
  HR_t   = HR_t,
  HR_t_1 = HR_t_1,
  HR_t_2 = HR_t_2,
  Cal_t   = Cal_t,
  Cal_t_1 = Cal_t_1,
  Cal_t_2 = Cal_t_2,
  METs_t   = METs_t,
  METs_t_1 = METs_t_1,
  METs_t_2 = METs_t_2,
  Steps_t   = Steps_t,
  Steps_t_1 = Steps_t_1,
  Steps_t_2 = Steps_t_2
)

## -------------------------------------------------
## 3. 画图
## -------------------------------------------------

set1_colors <- brewer.pal(9, "Set1")[1:8]  

df_long <- df |>
  pivot_longer(
    cols = c(METs_t_1, METs_t_2),
    names_to = "Lag",
    values_to = "METs_lag"
  )

p1 <- ggplot(df_long, aes(x = METs_lag, y = HR_t, color = Lag)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(METs_t_1 = "Lag 1: METs(t-1)",
        METs_t_2 = "Lag 2: METs(t-2)")
    )
  ) +
  scale_color_manual(
    values = c(
      METs_t_1 = set1_colors[1],
      METs_t_2 = set1_colors[2]
    ),
    guide = "none"  
  ) +
  labs(
    x = "Lagged Heart Rate",
    y = expression(HR(t))
  ) +
  theme_classic(base_size = 14)

# 保存为 PDF 
ggsave("Figure/cross_METs_HR.png", width = 8, height = 6, dpi = 300, device = "png")



#####
df_long <- df |>
  pivot_longer(
    cols = c(Steps_t_1, Cal_t_1),
    names_to = "Lag",
    values_to = "lag"
  )

p1 <- ggplot(df_long, aes(x = lag, y = HR_t, color = Lag)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(Steps_t_1 = "Lag 1: Steps(t-1)",
        Cal_t_1 = "Lag 2: Cal(t-2)")
    )
  ) +
  scale_color_manual(
    values = c(
      Steps_t_1 = set1_colors[1],
      Cal_t_1 = set1_colors[2]
    ),
    guide = "none"  
  ) +
  labs(
    x = "Lagged Heart Rate",
    y = expression(HR(t))
  ) +
  theme_classic(base_size = 14)

# 保存为 PDF 
ggsave("Figure/cross_StepsAndCals_HR.png", width = 8, height = 6, dpi = 300, device = "png")
