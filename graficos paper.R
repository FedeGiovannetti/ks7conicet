library(tidyverse)
library(ggpubr)
library(plotly)

base <- read.csv("Series históricas ingresos y becas psicología CONICET - Hoja 2.csv", sep = ";")

base$Type <- factor(base$Type, levels = c("Doctoral fellows", "Postdoctoral fellows", "Permanent Research Fellows"))

plot_1 = base%>%
  ggplot()+
  geom_rect(aes(xmin = 2015.5, xmax = 2019.5, ymin = -Inf, ymax = Inf),
            fill = "light grey", alpha = 0.014)+
  geom_point(aes(x = (Convocatoria), y= Psicologos.cada.100.becaries, group = Type, color = Type), size = 2)+
  geom_line(aes(x = (Convocatoria), y= Psicologos.cada.100.becaries, group = Type, color = Type), size = 1.2)+
  

  scale_x_continuous(limits = c(2011, 2020), breaks = seq(2011, 2020))+
  xlab(label = "Open Call")+
  ylab(label = "Psychology and Education Committee\n researchers Every 100 new job positions")+
  ylim(1,4)+
  theme_minimal()

plot_1

ggplotly(plot_1)



plot_2 = base%>%
  ggplot()+
  geom_rect(aes(xmin = 2015.5, xmax = 2019.5, ymin = -Inf, ymax = Inf),
            fill = "light grey", alpha = 0.014)+
  geom_point(aes(x = (Convocatoria), y= Otorgadas, group = Type, color = Type), size = 2)+
  geom_line(aes(x = (Convocatoria), y= Otorgadas, group = Type, color = Type), size = 1.2)+
  
  scale_x_continuous(limits = c(2011, 2020), breaks = seq(2011, 2020))+
  xlab(label = "Open Call")+
  ylab(label = "Psychology and Education Committee\n new job positions")+
  ylim(0,60)+
  theme_minimal()

plot_general_1 = ggarrange(plot_2, plot_1, labels = c("A", "B"),common.legend = T)

ggsave("plot_1.png", plot_general_1)


plot_3 = base%>%
  ggplot()+
  geom_rect(aes(xmin = 2015.5, xmax = 2019.5, ymin = -Inf, ymax = Inf),
            fill = "light grey", alpha = 0.014)+
  
  geom_point(aes(x = (Convocatoria), y= Postulaciones, group = Type, color = Type), size = 2)+
  geom_line(aes(x = (Convocatoria), y= Postulaciones, group = Type, color = Type), size = 1.2)+


  ylab(label = "Psychology and Education Committee\n applicants")+
  scale_x_continuous(limits = c(2011, 2020), breaks = seq(2011, 2020))+
  xlab(label = "Open Call")+
  # ylim(0,60)+
  theme_minimal()



plot_4 = base%>%
  ggplot()+
  geom_rect(aes(xmin = 2015.5, xmax = 2019.5, ymin = -Inf, ymax = Inf),
            fill = "light grey", alpha = 0.014)+
  
  geom_point(aes(x = (Convocatoria), y= Porcentaje, group = Type, color = Type), size = 2)+
  geom_line(aes(x = (Convocatoria), y= Porcentaje, group = Type, color = Type), size = 1.2)+
  
  
  scale_x_continuous(limits = c(2011, 2020), breaks = seq(2011, 2020))+
  xlab(label = "Open Call")+
  ylab(label = "Psychology and Education Committee\n percentaje of new job positions")+
  ylim(0,100)+
  theme_minimal()


plot_general_2 = ggarrange(plot_3, plot_4, labels = c("A", "B"),common.legend = T)

ggsave("plot_2.png", plot_general_2)

