library(tidyverse)
library(dagitty)
library(ggdag)

dag3 <- dagify(ap ~ a,
               a ~ p,
               p ~ m,
               s ~ p,
               a ~ p + s,
               a ~ dc,
               labels = c("a" = "Individual-level Outparty Animus",
                          "p" = "Party ID",
                          "m" = "Hostile Media",
                          "s" = "Social Welfare Preferences",
                          "dc" = "Demographic Controls",
                          "ap" = "Affective Polarization"),
               exposure = c("p", "m"),
               outcome = c("ap", "a"))
ggdag(dag3, text = F, label_col = "black", text_size = 3, node_size = 10, use_labels = "label", stylized = F) +
  guides(status = F) +
  labs(title = "Directed Acyclic Graph",
       subtitle = "Iyengar, Sood, & Lelkes (2012)")+
  theme_dag_blank()

ggsave(width = 7, height = 4, units = "in", "fig/ext-dag.png", dpi = 300)

#dag2 <- dagify(y ~ x + z2 + w2 + w1,
#               x ~ z1 + w1,
#               z1 ~ w1 + v,
#               z2 ~ w2 + v,
#               w1 ~~ w2,
#               exposure = "x",
#               outcome = "y")

#ggdag(dag2)
  