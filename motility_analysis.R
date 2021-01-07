###Data analysis of the motility essay:
analysis_time <- date()
exp_day <- "2020/Oct/21"
library(ggplot2)
###Getting the data
swim_raw <- read.csv(file.choose(), header = T, sep = ",")
swarm_raw <- read.csv(file.choose(), header = T, sep = ",")
twitch_raw <- read.csv(file.choose(), header = T, sep = ",")

##Prepating the statistics:
#Swim
pvalues <- c(); tested <- c()
for (i in unique(swim_raw$Cepa)){
  tested <- c(tested, i)
  pvalues <- c(pvalues,
               t.test(swim_raw$Area[swim_raw$Cepa == "Wt"],
                    swim_raw$Area[swim_raw$Cepa == i], paired = FALSE)[["p.value"]])
}
stat_swim <- data.frame(Wt = c(rep("Wt", length(tested))), Testado = tested, p_value = pvalues)
stat_swim <- transform(stat_swim, H0_Reject = (p_value < 0.05))

#Swarming
#Calculating occupancy:
swarm_raw <- transform(swarm_raw, Ocupacao = Area_Ocup/Area_plc)
pvalues <- c(); tested <- c()
for (i in unique(swarm_raw$Cepa)){
  tested <- c(tested, i)
  pvalues <- c(pvalues,
               t.test(swarm_raw$Ocupacao[swarm_raw$Cepa == "Wt"],
                      swarm_raw$Ocupacao[swarm_raw$Cepa == i], paired = FALSE)[["p.value"]])
}
stat_swarm <- data.frame(Wt = c(rep("Wt", length(tested))), Testado = tested, p_value = pvalues)
stat_swarm <- transform(stat_swarm, H0_Reject = (p_value < 0.05))

##Twitch
#T.test between strains
pvalues <- c(); tested <- c(); media <- c()
for (i in unique(twitch_raw$Cepa)){
  for (j in unique(twitch_raw$Meio)){
    tested <- c(tested, i)
    media <- c(media, j)
    pvalues <- c(pvalues,
                 t.test(twitch_raw$Area[twitch_raw$Cepa == "Wt" & twitch_raw$Meio == j],
                        twitch_raw$Area[twitch_raw$Cepa == i & twitch_raw$Meio == j],
                        paired = FALSE)[["p.value"]])
  }
}
stat_twitch <- data.frame("Wt", Cepa = tested, Meio = media, p_values = pvalues)
stat_twitch <- transform(stat_twitch, HO_Reject = (p_values < 0.05))

#Printing the Data;
setwd("/home/hneves/Documentos/lab_work/lab_baldini/Dados_experimentais/")
sink("motility_22Oct.txt")
print(exp_day); cat("Data da Análise:", analysis_time); cat("\n")
cat(">Significance of Swimming:"); cat("\n")
print(stat_swim); cat("\n\n")
cat(">Significance of Swarmming:"); cat("\n")
print(stat_swarm); cat("\n\n")
cat(">Significance of Twitching:"); cat("\n")
print(stat_twitch); cat("\n\n")
cat(">Significance of twitching in LB-Lennos"); cat("\n")
print(stat_twitch[stat_twitch$Meio == "Lennox",])
sink()

##Preparing the graphs:
swim_graph <- aggregate(list(Media = swim_raw$Area), list(Cepa = swim_raw$Cepa),
                        FUN = mean)
swim_graph <- cbind(swim_graph,
                    SEM = aggregate(list(SEM = swim_raw$Area), list(Cepa = swim_raw$Cepa),
                              FUN = sd)$SEM)
swim_graph$SEM <- swim_graph$SEM/sqrt(3)

ggplot(data = swim_graph, aes(x = Cepa, y = Media))+
  geom_errorbar(aes(ymin = Media - SEM, ymax = Media + SEM), width = .25, position = position_dodge(.9),
                colour = "black")+
  geom_bar(stat="identity", position=position_dodge(.9), fill = "dark green") +
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Swimming", x = "Cepas", y = "Área do Halo")+
  theme_classic()

ggsave("swimming.png")

##Swarmming:
swarm_graph <- aggregate(list(Media = swarm_raw$Ocupacao), list(Cepa = swarm_raw$Cepa),
                        FUN = mean)
swarm_graph <- cbind(swarm_graph,
                    SEM = aggregate(list(SEM = swarm_raw$Ocupacao), list(Cepa = swarm_raw$Cepa),
                                    FUN = sd)$SEM)
swarm_graph$SEM <- swarm_graph$SEM/sqrt(3)

ggplot(data = swarm_graph, aes(x = Cepa, y = Media))+
  geom_errorbar(aes(ymin = Media - SEM, ymax = Media + SEM), width = .25, position = position_dodge(.9),
                colour = "black")+
  geom_bar(stat="identity", position=position_dodge(.9), fill = "blue") +
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Swarming", x = "Cepas", y = "Ocupação da placa (%)")+
  theme_classic()

##Twitching:
twitch_raw_lennox <- twitch_raw[twitch_raw$Meio == "Lennox",]
twitch_graph <- aggregate(list(Media = twitch_raw_lennox$Area), list(Cepa = twitch_raw_lennox$Cepa),
                         FUN = mean)
twitch_graph <- cbind(twitch_graph,
                     SEM = aggregate(list(SEM = twitch_raw_lennox$Area), list(Cepa = twitch_raw_lennox$Cepa),
                                     FUN = sd)$SEM)
twitch_graph$SEM <- twitch_graph$SEM/sqrt(3)
twitch_graph <- twitch_graph[c(3,4,1,5),]
twitch_graph <- data.frame(Cepa = twitch_graph$Cepa,
                           Média = twitch_graph$Media,
                           SEM = twitch_graph$SEM)


ggplot(data = twitch_graph, aes(x = fct_inorder(Cepa), y = Média))+
  geom_errorbar(aes(ymin = Média - SEM, ymax = Média + SEM), width = .25, position = position_dodge(.9),
                colour = "black")+
  geom_bar(stat="identity", position=position_dodge(.9), width = 0.5, fill = "red") +
  scale_y_continuous(expand = c(0, 0))+
  labs(x = "Cepas", y = "Área (Cm²)")+
  #theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  expand_limits(y = c(0, 1.75))+
  geom_signif(comparisons = list(c("Wt", "ΔpilA")),
              y_position = 1.60,
              map_signif_level = TRUE,
              annotations = "***")+
  geom_signif(comparisons = list(c("Wt", "46810::Gm")),
              y_position = 1.50,
              map_signif_level = TRUE,
              annotations = "**")+
  geom_signif(comparisons = list(c("Wt", "ΔdgcP")),
              y_position = 1.40,
              map_signif_level = TRUE,
              annotations = "***") +
  geom_signif(comparisons = list(c("ΔdgcP", "46810::Gm")),
              y_position = 1,
              map_signif_level = TRUE,
              annotations = "N.S.")
  
