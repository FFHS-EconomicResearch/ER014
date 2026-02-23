# Setup ----
source(xfun::from_root('R','00_setup.R'))


# Daten importieren -----

## filename ----
date <- '2023-04-16'
my_in_file <- glue::glue('bev_struktur_{date}.rds')

## load ----
#load(file=xfun::from_root("data","tidy",my_in_file))

tbl_pop <- read_rds(xfun::from_root('data','tidy',my_in_file))


## Dataviz -----

### Negative Werte Männer -----
tbl_plot <- tbl_pop %>%
  mutate(Anzahl = if_else(Geschlecht=="m",
                          Anzahl*(-1),
                          Anzahl)
  )


### Farbpalette ----
ISBAblue <- '#232461'
ISBAred <- '#d84116'
mycols <- c(ISBAblue,ISBAred)

### Plots erzeugen -----

#### Fig3 Bevölkerungspyramide ----
p <- ggplot(tbl_plot, aes(x = Alter, y = Anzahl, fill = Geschlecht))
p <- p  +  geom_bar(data = subset(tbl_plot, Geschlecht == "w"), stat = "identity")
p <- p  +  geom_bar(data = subset(tbl_plot, Geschlecht == "m"), stat = "identity")
p <- p  +  scale_y_continuous(breaks=seq(-1000,1000,250),labels=abs(seq(-1000,1000,250)))
p <- p  +  coord_flip()
p <- p  +  facet_wrap( ~ Simulationsjahr, ncol=2)
#p <- p  +  ggtitle("Deutschland")
p <- p  +  xlab("Alter")
# p <- p  +ylab("Bevölkerung")
p <- p  +  labs( y = "Anzahl (in 1000)")
p <- p  +  guides(fill=guide_legend(title="Geschlecht"))
#p <- p  + scale_fill_discrete(name="", breaks=c("m", "w"), labels=c("Männer", "Frauen"))
#p <- p  +  scale_fill_brewer(palette = "Set1",breaks=c("Male", "Female"), labels=c("Männer", "Frauen"))
p <- p  +  scale_fill_manual(values = mycols, breaks = c("m", "w"),
                             labels=c("Männer", "Frauen"))
p <- p  + theme_bw() + theme(legend.position='bottom',
                             legend.title=element_text(size=rel(.7)),
                             legend.text = element_text(size=rel(.5)),
                             axis.text.x=element_text(size=rel(.5)),
                             axis.text.y=element_text(size=rel(.5)),
                             axis.title.x = element_text(size=rel(.5)),
                             axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('fig','Bev_pyr_D.svg'),width=1250,height = 900,units="px")




### save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("Bev_Pyr_{date}.pdf",)
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")

### interaktiv ----
ggplotly(p)
