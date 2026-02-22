library(tidyverse)
library(lessR)

# Define data
tbl_plot <- tibble(
  label = c(
    "Schnittstellen\n (Programme & Programmiersprachen)",
    "(free) Open-Source und plattformunabhängig\n (MS-Windows, Mac-OS, Linux)",
    "Professionelle Grafiken\ (ggplot, plotly)",
    "Große Community\n (stackoverflow.com, community.rstudio.com)",
    "Großer Funktionsumfang\n (optimiert für Datenanalyse)",
    "Dokumentation & Reproduzierbarkeit\n (R-Skript, RMarkdown/Quarto)",
    "Programmierumgebung (Flexibilität)",
    "Kontinuierliche Weiterentwicklung"
  ),
  value = c(8, 7, 6, 5, 4, 3, 2, 1)
)

p <- PieChart(label,data=tbl_plot,main=NULL,values="off",fill="viridis")
#fig_name <- "whyR.svg"
#ggsave(filename=xfun::from_root("img",'PVA1',fig_name),
#       plot = p,
#       width = 18,  height = 10,  units = "cm")
