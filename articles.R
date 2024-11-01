library(tidyverse)
library(ggplot2)
library(lubridate)
library(fmsb)
library(gganimate)
library(ggthemes)
library(gridExtra)
library(gridGraphics)
library(stringr) 


df <- read.csv("Halloween/horror_articles.csv")

# Adcionar fonte
windowsFonts(
  font = windowsFont("KreepTown")  # Substitua "Mitoos" pelo nome exato da fonte.
)

# Resumindo Publicação para ano
df$published <- year(df$published)

# Caracteristicas Gerais
summary(df)
a1 <- n_distinct(df$author)
ra1 <- n_distinct(df$rating)
t1 <- max(df$published)
t2 <- min(df$published)
cat("Número de autores:",a1,"Número de classificações de histórias:",ra1,"Período:",t1,"-",t2)
#------------------------------------------------------------------------------#
# Análise de rating

# Filtrar rating
df_filter <- df %>% filter(rating %in% c("false", "legend","true"))

# Agrupar
df_0 <- df %>% group_by(published, rating) %>% 
  summarize(n = n())


# Gráfico de linhas, x = date, y = count de historias, legenda = rating
gif <- df_1 %>%  ggplot(aes(x = published, y = n, shape = rating, color = rating)) +
               geom_line() +
  theme_par() +
  xlab("Ano")+
  ylab("Numero")+
  labs(title = "Quantidade de artigos postados por ano e por tipo") +
  theme(axis.title.x = element_text(family = "font", size = 12),
        plot.title = element_text(family = "font", size = 15),
        panel.grid.major = element_blank(),
        panel.background = element_rect("#180010"),
        axis.title.y = element_text(family = "font", size = 12)) +
  scale_color_manual(values = c("#520066", "#40ff00", "#f25a02")) +
  gganimate::transition_reveal(published)
  
anim_save("lines.gif", animation = gif)
#-----------------------------------------------------------------------------#
# Análise de autores
# Agrupar
df_2 <- df %>% group_by(author) %>% 
summarize(n = n())  

df_2$author <- str_wrap(df_2$author, width = 5)
gif2 <- df_2 %>% ggplot(aes(x = author, y = n)) +
  geom_bar(stat = "identity", fill = "#520066") +
  theme_par() +
  xlab("Autor")+
  ylab("Contagem")+
  labs(title = "Quantidade de artigos postados por author") +
  theme(axis.title.x = element_text(family = "font", size = 12),
        plot.title = element_text(family = "font", size = 15),
        axis.title.y = element_text(family = "font", size = 12),
        panel.grid.major = element_blank(),
        panel.background = element_rect("#180010")) +
  transition_states(states = author, transition_length = 5, state_length = 1, wrap = FALSE) + 
  enter_grow() +
  shadow_mark(alpha = 1)

anim_save("bar.gif", animation = gif2, width = 1200, height = 800)
# Radar: Barbara Mikkelson
df_bm <- df %>% group_by(author, rating) %>% 
  summarize(n = n()) %>% 
  filter(author %in% c("Barbara Mikkelson"))

df_bm <- pivot_wider(df_bm, names_from = rating, values_from = n)
df_bm <- df_bm %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
df_bm$author <- NULL

# Adicionar linhas para os limites máximos e mínimos
df_bm <- rbind(rep(60, ncol(df_bm)),  # Limite superior
                    rep(0, ncol(df_bm)),   # Limite inferior
                    df_bm)
png(filename ="halloween/r0.png")
radarchart(df_bm,
           axistype = 7,
           pcol = "#520066",
           pfcol = scales::alpha("#520066", 0.5),
           plwd = 2,
           plty = 1,
           title = "Barbara Mikkelson")
dev.off()


# Radar: Bethania Palma
df_bm <- df %>% group_by(author, rating) %>% 
  summarize(n = n()) %>% 
  filter(author %in% c("Bethania Palma","Barbara Mikkelson"))

df_bm <- pivot_wider(df_bm, names_from = rating, values_from = n)
df_bm <- df_bm %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
df_bm <- df_bm %>% filter(author == "Bethania Palma")
df_bm$author <- NULL

# Adicionar linhas para os limites máximos e mínimos
df_bm <- rbind(rep(60, ncol(df_bm)),  # Limite superior
               rep(0, ncol(df_bm)),   # Limite inferior
               df_bm)
png(filename ="halloween/r1.png")
radarchart(df_bm,
           axistype = 7,
           pcol = "#520066",
           pfcol = scales::alpha("#520066", 0.5),
           plwd = 2,
           plty = 1,
           title = "Bethania Palma")
dev.off()


# Radar: David Mikkelson
df_bm <- df %>% group_by(author, rating) %>% 
  summarize(n = n()) %>% 
  filter(author %in% c("David Mikkelson","Barbara Mikkelson"))

df_bm <- pivot_wider(df_bm, names_from = rating, values_from = n)
df_bm <- df_bm %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
df_bm <- df_bm %>% filter(author == "David Mikkelson")
df_bm$author <- NULL

# Adicionar linhas para os limites máximos e mínimos
df_bm <- rbind(rep(60, ncol(df_bm)),  # Limite superior
               rep(0, ncol(df_bm)),   # Limite inferior
               df_bm)

png(filename ="halloween/r2.png")
radarchart(df_bm,
           axistype = 7,
           pcol = "#520066",
           pfcol = scales::alpha("#520066", 0.5),
           plwd = 2,
           plty = 1,
           title = "David Mikkelson")
dev.off()

png("r2.png", width = 1200, height = 400)


#-----------------------------------------------------------------------------#
# Analise dos filmes

df <- readxl::read_excel("Halloween/filmes.xlsx")
df$Filmes <- gsub("-", "", df$Filmes)
df_main <- df[2:6,1:2]

df_complete <- df_main %>%
  mutate(
    # Extrai o conteúdo entre parênteses
    ano = str_extract(Filmes, "\\(([^()]+)\\)") %>% 
      str_sub(2,-2),
    
    # Remove caracteres indesejados
    Filmes = Filmes %>%
      str_replace_all('[()"-]', "") %>%   
      str_sub(5, -9))

                                  




