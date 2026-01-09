library(dplyr)
library(lubridate)

# 1. 读取数据
heartrate <- read.csv("data/heartrate_seconds_merged.csv", stringsAsFactors = FALSE)

# 2. 把 Time 转换成日期时间格式
heartrate$Time <- mdy_hms(heartrate$Time, tz = "UTC")

# 3. 创建一个只保留分钟的时间列
heartrate <- heartrate %>%
  mutate(Time_minute = floor_date(Time, unit = "minute"))

# 4. 按 Id 和分钟分组求平均
heartrate_minute <- heartrate %>%
  group_by(Id, Time_minute) %>%
  summarise(Avg_HeartRate = mean(Value, na.rm = TRUE), .groups = "drop")

# 5. 强制使用英语环境以确保 AM/PM 显示
Sys.setlocale("LC_TIME", "C")  # 或 "English"

# 6. 格式化时间列为带 AM/PM 的字符格式
heartrate_minute <- heartrate_minute %>%
  mutate(Time_minute = format(Time_minute, "%m/%d/%Y %I:%M:%S %p"))

# 7. 查看结果
head(heartrate_minute)

# 8. 保存结果
write.csv(heartrate_minute, "data/heartrate_minutes_merged.csv", row.names = FALSE)
