library(clusterProfiler)
library(ggplot2)
library(ggtext)
library(DOSE)

data(geneList, package = "DOSE")
de <- names(geneList)[1:100]
go <- enrichGO(de, 'org.Hs.eg.db', ont="ALL", pvalueCutoff=0.01)

df <- go@result %>% head(12)


ggplot(df)+
  geom_col(aes(x=-log10(p.adjust), y = Description),
           position = "identity",color = "white", fill = "orange", alpha = 0.2
  )+
  geom_text(aes(x=-log10(p.adjust), y = Description, label = Description, 
                color = parse_ratio(BgRatio) ),
            hjust = 0,x=0.1
  )+
  labs(y=NULL,x="-log<sub>10</sub>(p.adjust)")+
  scale_x_continuous(expand = c(0,0))+
  scale_color_gradient(low = "red", high = "blue", name = "BgRatio")+
  scale_y_discrete(limits = df$Description)+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_markdown()
  )