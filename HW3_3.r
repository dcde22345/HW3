# (a) mom.iq和kid.score間的回歸模型並進行迴歸分析
kid_iq <- read.csv("kid_iq.csv")
par(mar = c(5,7,5,5))
plot(kid_iq)

attach(kid_iq)
plot(mom.iq, kid.score, pch=0, main="Relation between mom.iq and kid.score", cex.main = 3, cex.lab = 3)

# fit1為兩者的U型關係
fit.1 <- lm(kid.score~ mom.iq + I(mom.iq^2), data=kid_iq)

coef(fit.1)[1] # 截距項
coef(fit.1)[2] # 一次項
coef(fit.1)[3] # 二次項

# x = mom.iq
curve(coef(fit.1)[1] + coef(fit.1)[2] * x + coef(fit.1)[3] * x^2, add = TRUE, col = "black", lwd=4)


# ------------------------------------------------
# (b) 檢驗U型關係
# 需要證明兩件事情
# 1. 曲線斜率由正到負
# 2. 找出轉折點

# 1. 證明曲線斜率由正到負
# 建立一個 mom.iq 的值範圍（從最小到最大）
mom_iq_values <- seq(min(mom.iq), max(mom.iq), length.out = 100)

# 計算每個mom_iq_values的斜率(微分後的公式帶入)
slope <- coef(fit.1)[2] + (2 * coef(fit.1)[3]) * mom_iq_values
# 繪製 mom.iq 與斜率的變化曲線
plot(mom_iq_values, slope, type = "l", col = "blue", lwd = 2, main = "Slope change diagram", cex.main = 3, cex.lab=3)
abline(h = 0, col = "red",lwd = 2 ,lty = 2)



# 2. 利用一次微分計算曲線斜率，找除轉折點(斜率=0)
turning_point <- - coef(fit.1)[2] / (2 * coef(fit.1)[3])
turning_point

# 計算轉折點對應的 kid.score 值，將turning_point帶入mom.iq
turning_point_y <- coef(fit.1)[1] + coef(fit.1)[2] * turning_point + coef(fit.1)[3] * turning_point^2

# 在圖中標記轉折點
points(turning_point, turning_point_y, col = "red", pch = 19, cex=2)
text(turning_point-10, turning_point_y + 5, labels = paste("轉折點:", round(turning_point, 2)), pos = 4, col = "red", cex=2)


# ---------------------------------
# (c) 考慮mom.hs對kid.score的影響

par(mar = c(8,7,10,5))

# 基本散佈圖，區分 mom.hs 狀態
plot(mom.iq, kid.score, 
     pch = ifelse(mom.hs == 1, 16, 1), # 以不同符號表示 mom.hs = 1
     col = ifelse(mom.hs == 1, "blue", "red"), # 不同顏色表示 mom.hs = 1
     main = "Relation between mom.iq and kid.score\n(considering mom.hs)", 
     xlab = "Mother's IQ (mom.iq)", 
     ylab = "Kid's Score (kid.score)",
     cex.main = 3, cex.lab = 3, cex.axis = 2)


# 建立交互項模型，包括 mom.hs 和其與 mom.iq 的交互作用
# mom.hs對kid.score的直接影響
# mom.hs對mom.iq的交互作用
# mom.hs對mom.iq^2的交互作用
fit.2 <- lm(kid.score ~ mom.iq + I(mom.iq^2) + mom.hs + mom.iq:mom.hs + I(mom.iq^2):mom.hs, data = kid_iq)

summary(fit.2)

coeffs = coef(fit.2)

# 定義在 mom.hs = 0 時的曲線
curve(coeffs[1] + coeffs[2]*x + coeffs[3]*I(x^2) + coeffs[4]*0 + coeffs[5]*x*0 + coeffs[6]*I(x^2)*0,
      add = TRUE, col = "red", lwd = 2, lty = 1)

# 定義在 mom.hs = 1 時的曲線
curve(coeffs[1] + coeffs[2]*x + coeffs[3]*I(x^2) + coeffs[4]*1 + coeffs[5]*x*1 + coeffs[6]*I(x^2)*1,
      add = TRUE, col = "blue", lwd = 2, lty = 1)

# 在圖的右側添加圖例
legend("bottomright", legend = c("no consider mom.hs", "mom.hs=0", "mom.hs=1"), col = c("black", "blue", "red"), pch = 19, cex=1.5)

