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
    cols = c(HR_t_1, HR_t_2),
    names_to = "Lag",
    values_to = "HR_lag"
  )

p1 <- ggplot(df_long, aes(x = HR_lag, y = HR_t, color = Lag)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(HR_t_1 = "Lag 1: HR(t-1)",
        HR_t_2 = "Lag 2: HR(t-2)")
    )
  ) +
  scale_color_manual(
    values = c(
      HR_t_1 = set1_colors[1],
      HR_t_2 = set1_colors[2]
    ),
    guide = "none"  
  ) +
  labs(
    x = "Lagged Heart Rate",
    y = expression(HR(t))
  ) +
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), 
  strip.placement = "outside") 

# 保存 
ggsave("Figure/AR_HR.png", width = 8, height = 6, dpi = 300, device = "png")


###卡路里自回归
df_cal_long <- df|>
  pivot_longer(
    cols = c(Cal_t_1, Cal_t_2),
    names_to = "Lag",
    values_to = "Cal_lag"
  )

p2 <- ggplot(df_cal_long, aes(x = Cal_lag, y = Cal_t)) +
  geom_point(
    aes(color = Lag),
    alpha = 0.25,
    size = 0.9
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(
        Cal_t_1 = "Lag 1: Cal(t-1)",
        Cal_t_2 = "Lag 2: Cal(t-2)"
      )
    )
  ) +
  
  scale_color_manual(
    values = c(
      Cal_t_1 = set1_colors[1],
      Cal_t_2 = set1_colors[2]
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Lagged Calories",
    y = expression(Cal(t))
  ) +
  
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), 
        strip.placement = "outside") 

# 保存为 PDF 
ggsave("Figure/AR_Cal.png", width = 8, height = 6, dpi = 300, device = "png")

##补充分析df |> filter(Cal_t_1 >= 10)




###代谢当量自回归
df_METs_long <- df|>
  pivot_longer(
    cols = c(METs_t_1, METs_t_2),
    names_to = "Lag",
    values_to = "METs_lag"
  )

p3 <- ggplot(df_METs_long, aes(x = METs_lag, y = METs_t)) +
  geom_point(
    aes(color = Lag),
    alpha = 0.25,
    size = 0.9
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(
        METs_t_1 = "Lag 1: METs(t-1)",
        METs_t_2 = "Lag 2: METs(t-2)"
      )
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
    x = "Lagged METs",
    y = expression(METs(t))
  ) +
  
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), 
        strip.placement = "outside") 

# 保存为 PDF 
ggsave("Figure/AR_METs.png", width = 8, height = 6, dpi = 300, device = "png")


###步数自回归
df_Steps_long <- df|>
  pivot_longer(
    cols = c(Steps_t_1, Steps_t_2),
    names_to = "Lag",
    values_to = "Steps_lag"
  )

p4 <- ggplot(df_Steps_long, aes(x = Steps_lag, y = Steps_t)) +
  geom_point(
    aes(color = Lag),
    alpha = 0.25,
    size = 0.9
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  facet_wrap(
    ~ Lag,
    scales = "free_x",
    labeller = as_labeller(
      c(
        Steps_t_1 = "Lag 1: Steps(t-1)",
        Steps_t_2 = "Lag 2: Steps(t-2)"
      )
    )
  ) +
  
  scale_color_manual(
    values = c(
      Steps_t_1 = set1_colors[1],
      Steps_t_2 = set1_colors[2]
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Lagged Steps",
    y = expression(Steps(t))
  ) +
  
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), 
        strip.placement = "outside") 

# 保存为 PDF 
ggsave("Figure/AR_Steps.png", width = 8, height = 6, dpi = 300, device = "png")


