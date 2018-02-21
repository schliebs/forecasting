library(plyr)
library(tidyverse)
library(ggplot2)
library(voteR)
library(haven)
library(magrittr)

data <- foreign::read.spss(file = "data/offline/data_project_700861_2018_02_09 (1).sav") %>% as.data.frame()
names(data)


data %<>% within({
  
  erst <- car::recode(v_1,'1 = "Cannata"; 2 = "Horn"; 3 = "Troendle"; 4 = "Enthaltung"; 5 = "Nichtwahl"')
  zweit <- car::recode(v_2,'1 = "Cannata"; 2 = "Horn"; 3 = "Troendle"; 4 = "Enthaltung"; 5 = "Nichtwahl"')
  cannata <- v_3
  troendle <- v_4
  horn <- v_5
  enthaltung <- v_6
  mail <- v_7
})

data$id = 1:nrow(data)

data_on <- 
  data %>% 
  select(erst,zweit,cannata,troendle,horn,enthaltung,mail) %>% 
  gather(key = "vote",
         value = "candidate",erst,zweit) %>% 
  mutate(type = "online")

# Online Only
data_on$candidate %>% table() %>% prop.table()

# Mit Matej offline: 

# 28.31373 24.41176  32.5098   12.80392
data_prog <- 
  data %>% select(cannata,horn,troendle,enthaltung,mail) %>% 
  filter((cannata * horn * troendle) != 0)

me = data_prog %>% select(1:4)%>% summarise_all(funs(mean(as.numeric(.),na.rm = TRUE))) ;me
med = data_prog %>% select(1:4)%>% summarise_all(funs(median(as.numeric(.),na.rm = TRUE))) ;med


data_prog <- 
  data_prog %>% 
  bind_rows(data.frame(mail = "ZU mean",me)) %>% 
  bind_rows(data.frame(mail = "ZU median",med)) %>% 
  bind_rows(data.frame(mail = "Ergebnis",cannata = 100*0.2976*0.92,horn = 100*0.2976*0.92,troendle = 100*0.4048*0.92,enthaltung = 100*0.08))


resultsmatrix = matrix(rep(c(0.2976*0.92,0.2976*0.92,0.4048*0.92,0.08),nrow(data_prog)),ncol = 4,byrow = TRUE)
x =(((data_prog[,1:4])/rowSums((data_prog[,1:4]))) - resultsmatrix)
data_prog$sq = round(rowSums(abs((x*100)^2))/4,4)
data_prog$abs = round(rowSums(abs((x*100)))/4,4)

View(data_prog[-1,])
options(scipen = 999)

#### Gewinner

sub = data_prog %>% arrange(abs) %>% head(8) %>% .[-1,]

gggw <- 
  ggplot2::ggplot(data = sub) +
  ggplot2::geom_bar(aes(x = reorder(mail,abs),
                        y = abs),
                    stat = "identity",
                    alpha = 0.3,
                    width = 0.8) + 
  
  ggplot2::geom_text(aes(x = reorder(mail,abs) ,
                         y = abs + 0.2,
                         label = paste0(round(abs,2))),
                     stat = "identity",
                     #  hjust = 0.5,
                     size = 5,
                     alpha = 1.0) +

  ggplot2::scale_x_discrete(breaks = c("ZU mean",
                                       "ZU median",
                                       "b.thies@zeppelin-university.net",
                                       "j.meibert@zeppelin-university.net",
                                       "m.wobith@zeppelin-university.net",
                                       "p.truckenmueller@zeppelin-university.net",
                                       "j.volkmann@zeppeln-university.net"),
                             labels = c("Mean ZU",
                                        "Median ZU",
                                        "Ben Thies",
                                        "Josephine Meibert",
                                        "M. Wobith",
                                        "Patricia Truckenmüller",
                                        "Johannes Volkmann"))+
  ggplot2::labs(x = NULL,
                y = "Abweichung",
                title = "Tippspielergebnis",
                subtitle = "Durchschnittliche Abweichung vom Endergebnis",
                caption = NULL) +

  hrbrthemes::theme_ipsum(grid = "none")+
  ggplot2::theme(legend.position = "none",
                 axis.text.x = element_text(angle = 90));gggw

ggsave(filename = "gewinnspiel.png",
       plot = gggw,
       width = 10,
       height = 6,
       device = "png",
       dpi = 500)


#####


sub2 = data_prog %>% arrange(abs) %>% head(50) %>% .[-1,] %>% filter(mail != -99 & mail != -66)

gggw2 <- 
  ggplot2::ggplot(data = sub2) +
  ggplot2::geom_bar(aes(x = reorder(mail,abs),
                        y = abs,
                        fill = mail %in% c("ZU mean","ZU median")),
                    stat = "identity",
                    alpha = 0.3,
                    width = 0.8) + 
  
  # ggplot2::geom_text(aes(x = reorder(mail,abs) ,
  #                        y = abs + 0.2,
  #                        label = paste0(round(abs,2))),
  #                    stat = "identity",
  #                    #  hjust = 0.5,
  #                    size = 5,
  #                    alpha = 1.0) +
  
  ggplot2::scale_x_discrete(breaks = c("ZU mean",
                                       "ZU median",
                                       "b.thies@zeppelin-university.net",
                                       "j.meibert@zeppelin-university.net",
                                       "m.wobith@zeppelin-university.net",
                                       "p.truckenmueller@zeppelin-university.net",
                                       "j.volkmann@zeppeln-university.net"),
                            labels = c("Mean ZU",
                                       "Median ZU",
                                       "Ben Thies",
                                       "Josephine Meibert",
                                       "M. Wobith",
                                       "Patricia Truckenmüller",
                                       "Johannes Volkmann"))+
  ggplot2::labs(x = NULL,
                y = "Abweichung",
                title = "Wisdom of the Crowd?",
                subtitle = "Durchschnittliche Abweichung vom Endergebnis",
                caption = NULL) +
  
  hrbrthemes::theme_ipsum(grid = "none")+
  ggplot2::theme(legend.position = "none",
                 axis.text.x = element_text(angle = 90,
                                            size = 10));gggw2

ggsave(filename = "gewinnspiel_ALL.png",
       plot = gggw2,
       width = 10,
       height = 6,
       device = "png",
       dpi = 500)



####




data_prog3 <- 
  data_prog %>% select(cannata,horn,troendle,enthaltung,mail,sq,abs) %>% as.data.frame() 

data_prog4 <- data_prog3 %>% as.matrix()

#29.76
#29.76
#40.48

data_off <- 
  data.frame(type = "offline",
             candidate = c(rep("Cannata",18),
                      rep("Troendle",18),
                      rep("Horn",24),
                      rep("Enthaltung",3)))

data_all <- bind_rows(data_on,data_off)

data_all$candidate %>% table() %>% prop.table()

data_all$candidate %>% table(data_all$type) %>% prop.table(2) %>% round(3)

data_prog <- 
  data %>% select(cannata,horn,troendle,enthaltung) %>% as.matrix()

data_prog %<>% na.omit() %>% as.data.frame()


data_prog %>% select(1:4)%>% summarise_all(funs(mean(as.numeric(.),na.rm = TRUE))) 

x = data_prog %>% summarise_all(funs(mean(.,na.rm = TRUE))) %>% .[1:3]  ; x
x/sum(x)

data_prog <- data_prog[data_prog$cannata*data_prog$horn*data_prog$troendle != 0,]

gg1 <- 
  ggplot(data_prog %>% select(-enthaltung) %>% gather(key = "candidate","value")) + 
  geom_density(aes(x = value,color = candidate),size = 1.3) + 
  labs(x = "Stimmanteil",
       y = "Häufigkeit",
       title = "Erwartetes Wahlergebnis",
       subtitle = "in % anteilig an allen gültigen Stimmen",
      caption = paste0("n = ",nrow(data_prog))) + 
  hrbrthemes::theme_ipsum(grid= "X")

ggsave(filename = "predictions.png",
       plot = gg1,
       width = 10,
       height = 6,device = "png",dpi = 1000)



####

t = data_all$candidate %>% table()  ; t
tprop = data_all$candidate %>% table() %>% prop.table ; tprop


sam = voteR::sample_dirichlet(tprop,sample_n = sum(t),n_draw = 10000)
gg2 <- 
  ggplot(sam  %>% select(-Nichtwahl,-Enthaltung) %>% gather(key = "candidate","value")) + 
  geom_density(aes(x = value,color = candidate),size = 1.3) + 
  labs(x = "Stimmanteil",
       y = "Häufigkeit",
       title = "Unsicherheitssimulation",
       subtitle = "too close to call",
       caption = paste0("n = ",nrow(data_all)/2)) + 
  hrbrthemes::theme_ipsum(grid= "X");gg2

ggsave(filename = "sample_density.png",
       plot = gg2,
       width = 10,
       height = 6,device = "png",dpi = 1000)


####
tprop/0.95
t

vote = c(cannata = tprop[1],
         horn = tprop[3],
         troendle = tprop[5]);
                      order = "alphabetical";
                      sample_confidence_bounds = TRUE;
                      sample_n = sum(t[c(1,3,5)])/2;
                      n_draw = 10000;
                      show_quantiles = c(0.10,0.90);
                      round = 1;
                      xlab = "Kandidat";
                      ylab = "Prognose";
                      title = "Senatorenwahl - Exit Poll";
                      subtitle = "too close to call - Neuwahlen nicht ausgeschlossen";
                      caption = paste0("n = ",round(sum(t)/2));
                      theme_ipsum = FALSE;
                      grid = "Y";
  
data = sample_dirichlet_quantiles(sample_n = sample_n,
                                  vote = vote,
                                  show_quantiles = show_quantiles,
                                  round = round + 1)

    gg3a <- ggplot2::ggplot(data = data) +
      ggplot2::geom_bar(aes(x = party,
                            y = q_90,
                            fill = party),
                        stat = "identity",
                        alpha = 0.3,
                        width = 0.8)+
      ggplot2::geom_bar(aes(x = party,
                            y = q_10,
                            fill = party),
                        stat = "identity",
                        alpha = 1.0,
                        width = 0.8) +
      ggplot2::geom_crossbar(aes(x = party,
                                 y = mean,
                                 color = party,
                                 ymin = q_10, ymax = q_90),
                             width = 0.8)+
      #### left
      ggplot2::geom_text(aes(x = party ,
                             y = q_90+ 0.02,
                             col = party,
                             label = paste0(round(100*q_10,round),"%")),
                         stat = "identity",
                         #  hjust = 0.5,
                         size = 2,
                         alpha = 1.0) +
      
      ####### right
      ggplot2::geom_text(aes(x = party,
                             y = q_90 + 0.08,
                             col = party,
                             label = paste0(round(100*q_90,round),"%")),
                         stat = "identity",
                         #  hjust = -0.5,
                         size = 2,
                         alpha = 1.0) +
      ggplot2::geom_text(aes(x = party,
                             y = q_90 + 0.05,
                             col = party,
                             label = paste0(round(100*mean,round),"%")),
                         stat = "identity",
                         size = 4,
                         alpha = 1.0)
 
  
  
  gg3b <- gg3a +
    
    ggplot2::scale_y_continuous(limits = c(0,1),breaks = seq(0.05,1  ,0.2),
                                labels = paste0(100*seq(0.05,1,0.2),"%")) +
    # ggplot2::scale_fill_manual(name = "Partei",
    #                            values = partycolors)+
    # ggplot2::scale_color_manual(name = "Partei",
    #                             values = partycolors_t)+
    ggplot2::scale_x_discrete(breaks = c("cannata.Cannata","horn.Horn","troendle.Troendle"),
                              labels = c("Cannata","Horn","Troendle"))+
    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title,
                  subtitle = subtitle,
                  caption = caption) +
    ggplot2::theme(legend.position = "none")
  
    gg3c <- gg3b +
      hrbrthemes::theme_ipsum(grid = grid)+
      ggplot2::theme(legend.position = "none")
gg3c

ggsave(filename = "sample2.png",
       plot = gg3c,
       width = 10,
       height = 6,device = "png",dpi = 500)

