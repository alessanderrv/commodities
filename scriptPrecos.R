# melhorias desse projeto:
# fazer uma funcao que importa os dados de uma unica planilha com purr map, já colar os dados organizados por mes na planilha
# fazer uma funcao que voce escolhe duas comodities e ele faz o gráfico
# Atalhos
# Collapse — Alt+L.
# Expand — Shift+Alt+L.
# Collapse All — Alt+O.
# Expand All — Shift+Alt+O.

library(tidyverse)
library(readxl)
library(lubridate)



# importa dados -----------------------------------------------------------



# importa ouro
ouro_indexMundi <- read_excel("dados/ouro_indexMundi.xlsx")
head(ouro_indexMundi)
# importa boi
boi <- read_excel("dados/boiCepea.xls", skip = 3)
# edita boi para > 2005
boi %>% 
  mutate(Data=dmy(Data)) %>% 
  filter(Data>"2004-12-31")-> boi
head(boi)


# importa igpm
igpm <- read_excel("dados/igpm.xlsx")
head(igpm)




# 

# organiza df ------------------------------------------------------------

# organiza ouro
ouro_indexMundi %>% 
  mutate(soma=lead(preco,1), cum=(soma/preco-1), cum2= 100*(1+(cumsum(cum)))) %>% 
  select(1,6) %>% 
  rename(preco=cum2) %>% 
  mutate(nome="ouro") -> dfprecos
# organiza boi
boi %>% 
  rename(real= `À vista R$`) %>% 
  mutate(soma=lead(real,1), cum=(soma/real-1), cum2= 100*(1+(cumsum(cum)))) %>% 
  select(1,6) %>% 
  rename(preco=cum2) %>% 
  mutate(nome="boi") %>% 
  group_by(ano=year(Data), mes=month(Data)) %>% 
  summarise(preco=mean(preco)) %>% 
  mutate(data=make_date(ano, mes, 1))  -> group_boi 
dataFrameBOI <- data.frame(group_boi)

dataFrameBOI %>% 
  select(4,3) %>% 
  mutate(nome="boi")-> BoiMes
# ajusta a data para ymd
dfprecos <- dfprecos %>% 
  mutate(data=ymd(data))
head(BoiMes)


# igpm

igpm %>% 
  mutate( cum=(valor/100), cum2= 100*(1+(cumsum(cum)))) %>% 
  select(1,3) %>% 
  rename(preco=cum2) %>%
  mutate(data=ymd(data)) %>% 
  mutate(nome="igp-m") -> dfIgpm


preco_cmdt <- bind_rows(BoiMes, dfprecos, dfIgpm)


#  plota linhas com plot "l" ------------------------------------------------------------------

#pdf("plot.pdf")

plot(x = BoiMes$data, y = BoiMes$preco, type = "l", 
     ylim = c(0, 300), xlab = "ano", ylab = "preco", main = "comparação Ouro x Boi (base 100) ")
lines(dfprecos$data, dfprecos$preco, type = "l", col = "red") 
text(as.Date("2007-01-01"), 200,  "Ouro", col = "red")
text(as.Date("2010-01-01"), 100,  "Boi")

#dev.off()

# valor maximo igpm 187.89
preco_cmdt %>% 
  filter(nome=="igp-m") %>% 
  .$preco %>% # aprendi curso de stanford - cria vetor com dados da coluna
  max()



#  plota linhas ggplot ------------------------------------------------------------------

# salva gráfico

pdf("plot_2.pdf")

preco_cmdt %>% 
  ggplot(aes(x=data, y=preco, color=nome)) +
  geom_line()+
  theme_bw()+
  ggtitle("comparação de preços (base 100)") 

 dev.off()




