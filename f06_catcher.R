library(tidyverse)
library(reshape2)
library(matrixStats)
library(ggplot2)
library(gridExtra)
library(GGally)
library(mgcv)
library(ggpubr)
library(gtable)

train <- train %>% filter(!is.na(spin_rate)) %>% filter(!is.na(outs)) %>% filter(tilt != "")

catcher <- train %>% filter(catcher_id == "f06c9fdf")
catcher_gen <- train %>% filter(catcher_id != "f06c9fdf")
catcher2 <- catcher %>% filter(pitch_call == "BallCalled" | pitch_call == "StrikeCalled")
catcher_gen2 <- catcher_gen %>% filter(pitch_call == "BallCalled" | pitch_call == "StrikeCalled")

sapply(catcher2, function(y) sum(length(which(is.na(y)))))
sapply(catcher_gen, function(y) sum(length(which(is.na(y)))))
# eliminate all rows w/ na values

# taking out rows w/ NA
catcher2 <- catcher2 %>% filter(!is.na(spin_rate))
catcher_gen <- catcher_gen %>% filter(!is.na(spin_rate)) %>% filter(!is.na(outs))
# catcher2 = "f06"
# catcher_gen = catchers not f06

# strike zone generation
top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Pitcher is on the top side, batter is on the left
catcher2 %>% ggplot(aes(x = plate_side, y = plate_height, col = factor(strike))) + geom_point() + geom_path(data = strike_zone_df, aes(x,y), lwd = 1, color = "black") + coord_fixed() + xlim(-4,4) + ylim(0,5) + labs(x = "Plate (x)", y = "Plate Height", title = "f06c9fdf") + facet_wrap(batter_side~pitcher_side) + scale_color_discrete(name = "Strike/Ball") + andyl1_315_theme
# General issues with corners of the strike zone, especially LvL inside, LvR outside

catcher_gen2 %>% ggplot(aes(x = plate_side, y = plate_height, col = factor(strike))) + geom_point() + geom_path(data = strike_zone_df, aes(x,y), lwd = 1, color = "black") + coord_fixed() + xlim(-4,4) + ylim(0,5) + labs(x = "Plate (x)", y = "Plate Height", title = "League") + facet_wrap(batter_side~pitcher_side) + scale_color_discrete(name = "Strike/Ball") + andyl1_315_theme
# Huge amount of space on top-left on left v left not called (pitcher is listed on the bottom

# Comparison

# LvL - both problems inside, our catcher --> up and away
# LvR - our catcher spotty around the edges, while only corners for general catchers
# RvL - our catcher some problems on the outside, nut better than general catchers, and we also buy more strikes here
# RvR - our catcher has problems top, general catchers have issues inside, buy equal amount of strikes

# Our catcher seems to buy less strikes outside with lefty pitchers against general MLB
# Does better with righty pitchers compared to average

# GAM --> smoothed prediction using location, matchup
strike_model_fit_sea_ll <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher2 %>% filter(batter_side == "Left" & pitcher_side == "Left"))
strike_model_fit_sea_rl <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher2 %>% filter(batter_side == "Left" & pitcher_side == "Right"))
strike_model_fit_sea_lr <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher2 %>% filter(batter_side == "Right" & pitcher_side == "Left"))
strike_model_fit_sea_rr <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher2 %>% filter(batter_side == "Right" & pitcher_side == "Right"))

strike_model_fit_mlb_ll <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher_gen2 %>% filter(batter_side == "Left" & pitcher_side == "Left"))
strike_model_fit_mlb_rl <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher_gen2 %>% filter(batter_side == "Left" & pitcher_side == "Right"))
strike_model_fit_mlb_lr <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher_gen2 %>% filter(batter_side == "Right" & pitcher_side == "Left"))
strike_model_fit_mlb_rr <- gam(strike ~ s(plate_side, plate_height), family = "binomial", data = catcher_gen2 %>% filter(batter_side == "Right" & pitcher_side == "Right"))

x <- seq(-1.25,1.25,length.out = 150)
y <- seq(1,4,length.out = 150)

strike_predict <- data.frame(plate_side = c(outer(x, y*0+1)), plate_height = c(outer(x*0+1, y)))

strike_model_sea_pred_ll <- predict(strike_model_fit_sea_ll, strike_predict)
strike_model_sea_pred_rl <- predict(strike_model_fit_sea_rl, strike_predict)
strike_model_sea_pred_lr <- predict(strike_model_fit_sea_lr, strike_predict)
strike_model_sea_pred_rr <- predict(strike_model_fit_sea_rr, strike_predict)

strike_model_mlb_pred_ll <- predict(strike_model_fit_mlb_ll, strike_predict)
strike_model_mlb_pred_lr <- predict(strike_model_fit_mlb_lr, strike_predict)
strike_model_mlb_pred_rl <- predict(strike_model_fit_mlb_rl, strike_predict)
strike_model_mlb_pred_rr <- predict(strike_model_fit_mlb_rr, strike_predict)

strike_predict_sea_data_ll <- strike_predict %>% mutate(strike_prob = exp(strike_model_sea_pred_ll)/(1+exp(strike_model_sea_pred_ll)))
strike_predict_sea_data_lr <- strike_predict %>% mutate(strike_prob = exp(strike_model_sea_pred_lr)/(1+exp(strike_model_sea_pred_lr)))
strike_predict_sea_data_rl <- strike_predict %>% mutate(strike_prob = exp(strike_model_sea_pred_rl)/(1+exp(strike_model_sea_pred_rl)))
strike_predict_sea_data_rr <- strike_predict %>% mutate(strike_prob = exp(strike_model_sea_pred_rr)/(1+exp(strike_model_sea_pred_rr)))

strike_predict_mlb_data_ll <- strike_predict %>% mutate(strike_prob = exp(strike_model_mlb_pred_ll)/(1+exp(strike_model_mlb_pred_ll)))
strike_predict_mlb_data_lr <- strike_predict %>% mutate(strike_prob = exp(strike_model_mlb_pred_lr)/(1+exp(strike_model_mlb_pred_lr)))
strike_predict_mlb_data_rl <- strike_predict %>% mutate(strike_prob = exp(strike_model_mlb_pred_rl)/(1+exp(strike_model_mlb_pred_rl)))
strike_predict_mlb_data_rr <- strike_predict %>% mutate(strike_prob = exp(strike_model_mlb_pred_rr)/(1+exp(strike_model_mlb_pred_rr)))

# Resulting heatmaps --> quite interesting
sea_ll <- ggplot(strike_predict_sea_data_ll) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(subtitle = "LHP v LHH")

sea_lr <- ggplot(strike_predict_sea_data_lr) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(subtitle = "LHP v RHH")

sea_rl <- ggplot(strike_predict_sea_data_rl) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(subtitle = "RHP v LHH")

sea_rr <- ggplot(strike_predict_sea_data_rr) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(subtitle = "RHP v RHH")

# used GAM --> project strike zones given pitcher/hitter matchup
grid.arrange(sea_ll + theme(legend.position = "none"), sea_lr + theme(legend.position = "none"), sea_rl + theme(legend.position = "none"), sea_rr + theme(legend.position = "none"), get_legend(sea_ll), top = "Strike Probability - Pitcher's Perspective - f06c9fdf", left = text_grob("Vertical Location (feet)", rot = 90, vjust = 1), bottom = "Horizontal Location (feet)", layout_matrix=rbind(c(1,2,3), c(4,5,5)))

mlb_ll <- ggplot(strike_predict_mlb_data_ll) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(subtitle = "LHP v LHH")

mlb_lr <- ggplot(strike_predict_mlb_data_lr) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(subtitle = "LHP v RHH")

mlb_rl <- ggplot(strike_predict_mlb_data_rl) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(subtitle = "RHP v LHH")

mlb_rr <- ggplot(strike_predict_mlb_data_rr) +
  geom_tile(aes(x = plate_side, y = plate_height, fill = strike_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Strike Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  theme(text = element_text(size = 9), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(subtitle = "RHP v RHH")

grid.arrange(mlb_ll + theme(legend.position = "none"), mlb_lr + theme(legend.position = "none"), mlb_rl + theme(legend.position = "none"), mlb_rr + theme(legend.position = "none"), get_legend(sea_ll), top = "Strike Probability - Pitcher's Perspective - League", left = text_grob("Vertical Location (feet)", rot = 90, vjust = 1), bottom = "Horizontal Location (feet)", layout_matrix=rbind(c(1,2,3), c(4,5,5)))

# We look worse, in general, across the board 
# LHP v LHH - marginally better higher in the zone, with knack of getting high balls into strikes, but worse lower
# LHP v RHH - struggling on the edges, especially when compared to the rest of league
# RHP v LHH - relatively close, but worse in getting pitches on the edge into strikes as consistently as league
# RHP v RHH - Again, relatively close, but worse on edges

catcher2$zone = NA
catcher_gen2$zone = NA

for(i in 1:nrow(catcher2)){
  x1 = -.95 + (1.9/3)
  x2 = .95 - (1.9/3)
  y1 = 1.6 + (1.9/3)
  y2 = 3.5 - (1.9/3)
  ht = catcher2$plate_height[i]
  wi = catcher2$plate_side[i]
  if(wi < -.95){
    if(ht < 1.6){
      catcher2$zone[i] = 16
    }
    if(ht > 3.5){
      catcher2$zone[i] = 10
    }
    if(ht >= 1.6 & ht <= 3.5){
      catcher2$zone[i] = 17
    }
  }
  if(wi > .95){
    if(ht < 1.6){
      catcher2$zone[i] = 14
    }
    if(ht > 3.5){
      catcher2$zone[i] = 12
    }
    if(ht >= 1.6 & ht <= 3.5){
      catcher2$zone[i] = 13
    }
  }
  if(wi >= -.95 & wi <= .95){
    if(ht > 3.5){
      catcher2$zone[i] = 11
    }
    if(ht < 1.6){
      catcher2$zone[i] = 15
    }
    if(ht <= y1 & ht >= 1.6){
      if(wi <= x1){
        catcher2$zone[i] = 7
      }
      if(wi <= x2 & wi > x1){
        catcher2$zone[i] = 8
      }
      if(wi > x2){
        catcher2$zone[i] = 9
      }
    }
    if(ht < y2 & ht > y1){
      if(wi <= x1){
        catcher2$zone[i] = 4
      }
      if(wi <= x2 & wi > x1){
        catcher2$zone[i] = 5
      }
      if(wi > x2){
        catcher2$zone[i] = 6
      }
    }
    if(ht >= y2 & ht <= 3.5){
      if(wi <= x1){
        catcher2$zone[i] = 1
      }
      if(wi <= x2 & wi > x1){
        catcher2$zone[i] = 2
      }
      if(wi > x2){
        catcher2$zone[i] = 3
      }
    }
  }
}

for(i in 1:nrow(catcher_gen2)){
  x1 = -.95 + (1.9/3)
  x2 = .95 - (1.9/3)
  y1 = 1.6 + (1.9/3)
  y2 = 3.5 - (1.9/3)
  ht = catcher_gen2$plate_height[i]
  wi = catcher_gen2$plate_side[i]
  if(wi < -.95){
    if(ht < 1.6){
      catcher_gen2$zone[i] = 16
    }
    if(ht > 3.5){
      catcher_gen2$zone[i] = 10
    }
    if(ht >= 1.6 & ht <= 3.5){
      catcher_gen2$zone[i] = 17
    }
  }
  if(wi > .95){
    if(ht < 1.6){
      catcher_gen2$zone[i] = 14
    }
    if(ht > 3.5){
      catcher_gen2$zone[i] = 12
    }
    if(ht >= 1.6 & ht <= 3.5){
      catcher_gen2$zone[i] = 13
    }
  }
  if(wi >= -.95 & wi <= .95){
    if(ht > 3.5){
      catcher_gen2$zone[i] = 11
    }
    if(ht < 1.6){
      catcher_gen2$zone[i] = 15
    }
    if(ht <= y1 & ht >= 1.6){
      if(wi <= x1){
        catcher_gen2$zone[i] = 7
      }
      if(wi <= x2 & wi > x1){
        catcher_gen2$zone[i] = 8
      }
      if(wi > x2){
        catcher_gen2$zone[i] = 9
      }
    }
    if(ht < y2 & ht > y1){
      if(wi <= x1){
        catcher_gen2$zone[i] = 4
      }
      if(wi <= x2 & wi > x1){
        catcher_gen2$zone[i] = 5
      }
      if(wi > x2){
        catcher_gen2$zone[i] = 6
      }
    }
    if(ht >= y2 & ht <= 3.5){
      if(wi <= x1){
        catcher_gen2$zone[i] = 1
      }
      if(wi <= x2 & wi > x1){
        catcher_gen2$zone[i] = 2
      }
      if(wi > x2){
        catcher_gen2$zone[i] = 3
      }
    }
  }
}
# Zone calculations

sz_mlb <- catcher_gen2 %>% group_by(zone) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_sea <- catcher2 %>% group_by(zone) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_mlb$player = "League"
sz_sea$player = "f06"

sz <- rbind(sz_sea, sz_mlb)
sz %>% filter(zone <= 9) %>% ggplot(aes(x = zone, y = strike_rate, fill = player)) + geom_bar(stat = "identity",position = "dodge") + scale_x_continuous(breaks = seq(1,9,by = 1)) + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + andyl1_315_theme + labs(title = "Called Strike%, In Zone", y = "C-Strike Rate")
# MAJOR PROBLEMS --> bottom of the strike zone
sz %>% filter(zone > 9) %>% ggplot(aes(x = zone, y = strike_rate, fill = player)) + geom_bar(stat = "identity",position = "dodge") + scale_x_continuous(breaks = seq(10,17,by = 1)) + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + andyl1_315_theme + labs(title = "Called Strike%, Outside", y = "C-Strike Rate")
# Again, looks decent at the top of the zone, but horrendous in the bottom. 
# better on the outside of the plate than inside

sz_mlb_match <- catcher_gen2 %>% group_by(zone, pitcher_side, batter_side) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_sea_match <- catcher2 %>% group_by(zone, pitcher_side, batter_side) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_mlb_match$player = "League"
sz_sea_match$player = "f06"
sz_match = rbind(sz_sea_match, sz_mlb_match)

bottom <- sz_match %>% filter(zone %in% c(7,8,9,15))
bottom$zone[which(bottom$zone == 15)] = 10
# add new data frame to make plotting easier, avoid x breaks
bottom %>% ggplot(aes(x = factor(zone), y = strike_rate, fill = player)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + scale_x_discrete(labels = c("7","8","9","15")) + andyl1_315_theme + labs(x = "Zone", y = "Strike Rate", title = "Bottom of Zone") + facet_wrap(pitcher_side~batter_side) 
# Again, suck across the board (gonna have to end up changing the bottom shit)
top <- sz_match %>% filter(zone %in% c(1,2,3,11))
top$zone[which(top$zone == 11)] = 4
top %>% ggplot(aes(x = factor(zone), y = strike_rate, fill = player)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + scale_x_discrete(labels = c("1","2","3","11")) + andyl1_315_theme + labs(x = "Zone", y = "Strike Rate", title = "Top of Zone") + facet_wrap(pitcher_side~batter_side) 

# Looking at zone, pitch --> further filtering on matchup would reduce n() to single digits
sz_pitch_mlb <- catcher_gen2 %>% filter(pitch_type %in% c("CH", "CU", "FA", "SL")) %>% group_by(zone, pitch_type) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_pitch_sea <- catcher2 %>% filter(pitch_type %in% c("CH", "CU", "FA", "SL")) %>% group_by(zone, pitch_type) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_pitch_mlb$player = "League"
sz_pitch_sea$player = "f06"
sz_all_pitch <- rbind(sz_pitch_sea, sz_pitch_mlb)

sz_all_pitch %>% filter(zone <= 9) %>% ggplot(aes(x = zone, y = strike_rate, fill = player)) + geom_bar(stat = "identity", position = "dodge") + scale_x_continuous(breaks = c(1:9)) + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + andyl1_315_theme + facet_wrap(~pitch_type) + scale_y_continuous(breaks = c(.5, 1)) + labs(title = "League Average Strike Call/Pitch", y = "Strike Rate", x = "Zone") 
# By pitches ... look for differences

sz_all_pitch %>% filter(zone > 9) %>% ggplot(aes(x = zone, y = strike_rate, fill = player)) + geom_bar(stat = "identity", position = "dodge") + scale_x_continuous(breaks = c(10:17)) + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + andyl1_315_theme + facet_wrap(~pitch_type) + scale_y_continuous(breaks = seq(0, .25, by  = .05)) + labs(title = "League Average Strike Call/Pitch", y = "Strike Rate", x = "Zone")

# Look at 
sz_count_mlb <- catcher_gen2 %>% filter(strikes == 2) %>% group_by(zone, strikes) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_count_sea <- catcher2 %>% filter(strikes == 2) %>% group_by(zone,strikes) %>% summarize(n.obs = n(), strike_rate = sum(strike)/n())
sz_count_mlb$player = "League"
sz_count_sea$player = "f06"

sz_all_count <- rbind(sz_count_sea, sz_count_mlb)

# only focus on pitches outside of zone due to sample size
sz_all_count %>% filter(zone > 9) %>% ggplot(aes(x = zone, y = strike_rate, fill = player)) + geom_bar(stat = "identity", position = "dodge") + scale_x_continuous(breaks = c(10:17)) + scale_fill_manual(values = c("#0C2C56", "#F3B4E9")) + andyl1_315_theme + labs(title = "2-Strikes, Outside", x = "Zone", y = "Strike Rate")

# Above plots shown, explained in writeup
