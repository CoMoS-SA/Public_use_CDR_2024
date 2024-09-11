
# Load packages #####
library(readr)
library(Matching)
library(broom)
library(stringr)
library(countreg)
library(jtools)
library(RColorBrewer)
library(ggridges)
library(gridExtra)
library(ggh4x)
library(ggnewscale)
library(fmsb)
library(tibble)
library(dplyr)
#library(plyr)
library(ggplot2)
library(scales)
library(tidyr)
library(fauxnaif)
library(kableExtra)
library(magrittr)
library(viridis)
library(ggrepel)
library(ggpubr)
library(janitor)
library(ggbump)


# Main text figures ####
# Color-blind palette (Okabe-Ito)
colors <- c("#999999", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Fig1A ####
fig1A <- read_csv("fig1A.csv", col_types = cols(X1 = col_skip()))

fig1A$net1 <- factor(fig1A$net1, levels = c('General', 
                                                  'Afforestation/Reforestation (AR)',
                                                  'Bioenergy with Carbon Capture & Storage (BECCS)',
                                                  'Biochar',
                                                  'Blue Carbon (BC)',
                                                  'Direct Air Capture (DAC)',
                                                  'Enhanced Weathering (EW)',
                                                  'Ocean Fertilization (OF)',
                                                  'Soil Carbon Sequestration (SCS)'))
#View(fig1A)
fig1A <- ggplot(aes(x = factor(year), y = count, fill = net1), data=fig1A) +
  #geom_dotplot() +
  geom_col(color="white") +
  #paletteer::scale_fill_paletteer_d("rcartocolor::Bold") +
  #scale_fill_discrete(hue_pal()(9), direction=-1) + 
  theme_minimal() + ylab("# of articles") + xlab("") +  
  guides(fill = guide_legend(nrow = 3))+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank(), legend.title = element_blank(), legend.position = "top", 
        axis.text = element_text(size=15, face="bold", colour = "black"), legend.text = element_text(face="bold", size=15), axis.title = element_text(face="bold", size=30)) +
  theme(legend.direction = "horizontal") + scale_fill_discrete(direction=-1) +
  scale_fill_manual(values = colors)

# Fig1B  ####
fig1B <- read_csv("fig1B.csv", col_types = cols(X1 = col_skip()))
fig1B <- ggplot(data=fig1B) + 
  geom_map(map = fig1B, aes(long, lat, map_id = region, fill=N),col="white", size=0.2) +
  coord_fixed() + 
  theme_void() + 
  guides(fill = guide_colourbar(title.position = "top",barwidth = 10, barheight = 0.5,ticks = FALSE)) +
  theme(legend.position = "top",
        legend.title.align=0.5, 
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) + 
  scale_fill_viridis() + labs(fill="# of articles") +
  lims(x = c(-160, 180), y = c(-52, 90))

# Fig1C  ####
fig1C <- read_csv("fig1C.csv", col_types = cols(X1 = col_skip()))
fig1C <- ggplot(data=fig1C,aes(fill=net, y=share, x=reorder(country, total)),
       position='stack') + 
  geom_col(color="white", size=1, width = 0.5) + 
  geom_hline(yintercept = 50, linetype=1, color="black", size=2) + 
  #coord_polar() +
  coord_flip() +
  scale_fill_manual(values = c("grey","goldenrod2"), name = "") + 
  theme_minimal() +
  theme(legend.position ="top",
        legend.text = element_text(face="bold", size=15), 
        panel.grid = element_line(color="white"), 
        axis.text.y = element_text(face="bold", color="black", size=15),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face="bold", color="black", size=15),
        axis.title.x = element_text(face="bold", color="black", size=15)) +
  guides(alpha = guide_legend(title = "Total # of articles", 
                              title.position = "left",
                              title.theme = element_text(
                                face = "bold",
                                colour = "black",
                                angle = 0))) + 
  labs(y = "Share of Conventional vs. Novel CDR articles")

fig1 <- ggarrange(fig1B, fig1C, nrow = 1, common.legend = F)
ggarrange(fig1A,fig1,nrow = 2)

# Fig2A  ####
fig2A <- read_csv("fig2A.csv", col_types = cols(X1 = col_skip()))
fig2A <- ggplot(aes(dimension,NET), data=fig2A) + 
  geom_segment(aes(x=dimension, xend=dimension, y=1, yend=NET), color="grey", size=2) +
  geom_point(size=5, shape=21, color="white", fill="black", stroke=1) +
  geom_hline(yintercept = 1, linetype=2,size=1) +
  theme_classic() + labs(x="", y="Relative # of mentions (baseline control)") +
  theme(axis.text = element_text(face="bold", color="black", size=15), 
        axis.title = element_text(face="bold",size=10),
        strip.text.x = element_text(size = 15, color = "black", face = "bold"))

# Fig2A  ####
fig2B <- read_csv("fig2B.csv", col_types = cols(X1 = col_skip()))
fig2B <- ggplot(aes(dimension,NET), data=fig2B) + 
  geom_segment(aes(x=dimension, xend=dimension, y=1, yend=NET), color="grey", size=2) +
  geom_point(size=5, shape=22, color="white", fill="black", stroke=1) +
  geom_hline(yintercept = 1, linetype=2,size=1) +
  theme_classic() + labs(x="", y="Relative # of mentions (climate control)") +
  theme(axis.text = element_text(face="bold", color="black", size=15), 
        axis.title = element_text(face="bold",size=10),
        strip.text.x = element_text(size = 15, color = "black", face = "bold"))
ggarrange(fig2A,fig2B, nrow = 2)

# Fig3  ####
fig3_radar <- read_csv("fig3_radar.csv")

fig3_radar <- as.data.frame(t(fig3_radar)) %>% 
  row_to_names(row_number = 1) %>% 
  add_rownames(var = "dimension") %>% 
  dplyr::select(c("dimension","Conventional","Novel"))

fig3_radar %>% str()

fig3_radar %<>% pivot_longer(cols=c('Conventional', 'Novel'),
                            names_to='CDR',
                            values_to='score')

fig3_radar$score <- as.numeric(as.character(fig3_radar$score))


ggplot(aes(dimension,score), data=fig3_radar) + 
  geom_segment(aes(x=dimension, xend=dimension, y=1, yend=score, color=factor(CDR)), color="black", size=2) +
  geom_point(aes(fill=factor(CDR)),size=5, shape=21, color="black", stroke=1) +
  geom_hline(yintercept = 1, linetype=2,size=1) +
  theme_classic() + 
  scale_fill_manual(values=c("grey","goldenrod2")) +
  labs(x="", y="Relative # of mentions (baseline control)", fill="") +
  theme(axis.text = element_text(face="bold", color="black", size=15), 
        axis.title = element_text(face="bold",size=10),
        legend.position = "top",
        legend.text = element_text(face="bold", size=20),
        strip.text.x = element_text(size = 15, color = "black", face = "bold"))



fig3 <- read_csv("fig3.csv")
fig3 %<>% remove_rownames %>% column_to_rownames(var="X1")

titles <- c("General",rownames(fig3)[5:12])
colors <- c("#999999","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

titles
opar <- par(font=2) 
# Define settings for plotting in a 3x3 grid, with appropriate margins:
par(mar = rep(0.8,4))
par(mfrow = c(3,3))
# Produce a radar-chart for each NET
for (i in 4:nrow(fig3)) {
  radarchart(
    fig3[c(1:3, i), ],
    cglty = 2, cglcol = "gray",
    pcol = c(NA, colors[i-3]), title = titles[i-3], plwd = 5,plty = 1, pfcol = c("#99999980",NA),
    cglwd = 2,vlcex = 2,seg=10, axistype=4,caxislabels = c("","1","","","","5","","","","","10"),calcex = 2,
    axislabcol="black",cex.main = 2,col.main = colors[i-3],line = -1,adj=1
  )
}


# Fig4  ####
fig4_avg_novel <- read_csv("fig4_avg_novel.csv", col_types = cols(X1 = col_skip()))
fig4_ie_novel <- read_csv("fig4_ie_novel.csv", col_types = cols(X1 = col_skip()))

labels <- c("Science","Technology","Policy","Media")
names(labels) <- c("1","2","3","4")
fig4_avg_novel %>% group_by(ID) %>% summarize(max(m_estimate))
line_control <- data.frame(yi = 1)

fig4_novel <- ggplot(fig4_avg_novel, aes(term, m_estimate, colour = term)) + 
  geom_crossbar(aes(ymin = m_low, ymax = m_high)) + 
  theme_bw() + 
  geom_hline(yintercept = 1,linetype="dashed") + 
  coord_flip() +
  theme(panel.grid.minor = element_blank(), legend.position = "none") + xlab("") + 
  #ylab(expression(exp(beta))) +
  ylab("Effect size") +
  geom_pointrange(data=fig4_ie_novel, aes(factor(term), estimate,ymin = conf.low, ymax = conf.high),position = position_jitter(),alpha=0.2) +
  theme(axis.text = element_text(face="bold", color="black", size=15), 
        axis.title = element_text(face="bold",size=20),
        strip.text.x = element_text(size = 15, color = "black", face = "bold"),
        strip.background =element_rect(fill="white")) +  
  facet_grid(~ID,scales = "free_x",labeller = labeller(ID = labels)) +
  facetted_pos_scales(
    y = rep(list(
      scale_y_continuous(breaks = seq(from = 1, to = 10,length.out = 1)),
      scale_y_continuous(breaks = seq(from = 1, to = 10,length.out = 1)),
      scale_y_continuous(breaks = seq(from = 1, to = 10,length.out = 1)),
      scale_y_continuous(breaks = seq(from = 1, to = 10,length.out = 1))
    ))) + scale_color_manual(values=c("grey","goldenrod2")) + theme(aspect.ratio = 1)


fig4_avg <- read_csv("fig4_avg.csv", col_types = cols(X1 = col_skip()))
fig4_ie <- read_csv("fig4_ie.csv", col_types = cols(X1 = col_skip()))

labels <- c("Science","Technology","Policy","Media")
names(labels) <- c("1","2","3","4")
fig4_avg %>% group_by(ID) %>% summarize(max(m_estimate))
line_control <- data.frame(yi = 1)

colors <- c("#999999", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fig4_avg$term <- factor(fig4_avg$term, levels = c("General", "AR", "BECCS", "Biochar", "BC", "DAC","EW","OF","SCS"))

fig4 <- ggplot(fig4_avg, aes(term, m_estimate, colour = term)) + 
  geom_crossbar(aes(ymin = m_low, ymax = m_high)) + 
  theme_bw() + 
  geom_hline(yintercept = 1,linetype="dashed") + 
  coord_flip() +
  theme(panel.grid.minor = element_blank(), legend.position = "none") + xlab("") + 
  #ylab(expression(exp(beta))) +
  ylab("Effect size") +
  geom_pointrange(data=fig4_ie, aes(factor(term), estimate,ymin = conf.low, ymax = conf.high),position = position_jitter(),alpha=0.2) +
  theme(axis.text = element_text(face="bold", color="black", size=15), 
        axis.title = element_text(face="bold",size=20),
        strip.text.x = element_text(size = 15, color = "black", face = "bold"),
        strip.background =element_rect(fill="white")) +  
  facet_grid(~ID,scales = "free_x",labeller = labeller(ID = labels)) +
  facetted_pos_scales(
    y = rep(list(
      scale_y_continuous(breaks = seq(from = 1, to = 3,length.out = 3)),
      scale_y_continuous(breaks = seq(from = 1, to = 15,length.out = 3)),
      scale_y_continuous(breaks = seq(from = 1, to = 9,length.out = 3)),
      scale_y_continuous(breaks = seq(from = 1, to = 9,length.out = 3))
    ))) + 
  scale_color_manual(values=colors) + scale_x_discrete(limits=rev) + theme(aspect.ratio = 1)

ggarrange(fig4_novel,fig4, nrow = 2, align = "v")

# Fig5A  ####
colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
fig5A <- read_csv("fig5A.csv", col_types = cols(X1 = col_skip()))

rows <-  c("BANGLADESH" ,  "CHILE"      ,  "PANAMA"     ,  "TURKEY"      , "COLOMBIA"    , "SINGAPORE"  ,  "GREECE"     ,  "PHILIPPINES" ,
 "POLAND"     ,  "RUSSIA"     ,  "IRAN"       ,  "SAUDI ARABIA", "SOUTH AFRICA", "ISRAEL"     ,  "KENYA"      ,  "MEXICO"  ,
 "PORTUGAL"   ,  "TAIWAN"     ,  "INDONESIA"  ,  "WALES"       , "PAKISTAN"    , "IRELAND"    ,  "ARGENTINA"  ,  "MALAYSIA"    ,
 "AUSTRIA"    ,  "NORWAY"     ,  "FINLAND"    ,  "DENMARK"     , "SOUTH KOREA" , "BELGIUM"    ,  "SWEDEN"     ,  "SWITZERLAND" ,
 "BRAZIL"     ,  "NEW ZEALAND",  "SCOTLAND"   ,  "JAPAN"       , "FRANCE"      , "INDIA"      ,  "NETHERLANDS",  "SPAIN"       ,
 "ITALY"      ,  "CANADA"     ,  "GERMANY"    ,  "ENGLAND"     , "AUSTRALIA"   , "CHINA"      ,  "USA")
 
fig5A$country <- factor(fig5A$country, levels = rows)

fig5A <- ggplot(fig5A, aes(x = factor(variable), y = factor(country), fill = value)) +
  geom_tile(color="white",size=2) + scale_fill_viridis(na.value = NA) + 
  xlab("") + 
  theme_minimal() +
  ylab("")  +
  theme(axis.text.x = element_text(face = "bold",size=10, color="black"),
        axis.text.y.left = element_text(face = "bold",size=8, color="black"),
        legend.position = "top", panel.grid = element_blank(),
        legend.text = element_text(face="bold", size=15), axis.title = element_text(face="bold", size=30),
        legend.title = element_text(face="bold", size=15))  + 
  scale_y_discrete(position = "left") +
  labs(fill="RSA") 

# Fig5B  ####
fig5B <- read_csv("fig5B.csv", col_types = cols(X1 = col_skip()))
fig5B$net <- factor(fig5B$net, levels = c('AR',
                                            'BECCS',
                                            'Biochar',
                                            'BC',
                                            'DAC',
                                            'EW',
                                            'OF',
                                            'SCS'))

fig5B <- ggplot(aes(tech_perc,policy_perc), data=fig5B) + 
  geom_abline(slope = 1 ) + 
  geom_text_repel(aes(label=ifelse(rank<10,country,NA)), size=2, fontface="bold") + 
  geom_point(aes(size=total, fill=net), shape=21) +
  scale_x_continuous(breaks =seq(from = 0, to = 1, by = .2)) + 
  scale_y_continuous(breaks =seq(from = 0, to = 1, by = .2)) + 
  scale_fill_manual(values=c(colors)) +
  xlim(0,1) + ylim(0,1) +
  facet_wrap(~net, ncol =2) +
  coord_fixed() + 
  theme_bw() +
  theme(legend.position = "none", 
        strip.background = element_blank(), 
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black")) +
  labs(x= "Technology coverage %", y="Policy coverage %")

ggarrange(fig5A,fig5B)

