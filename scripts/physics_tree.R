###############################################################################
###   FUNCTIONS                                                             ###
###############################################################################



###############################################################################
###   READ DATA                                                             ###
###############################################################################

library(readxl)

data.dir <- "../data"
filename <- "edges.xlsx"
sheet <- "Sheet1"
range <- "A1:H103"

spreadsheet <- read_xlsx(paste(data.dir,filename,sep="/"),
                         sheet = sheet,
                         range = range,
                         col_names = TRUE)

###############################################################################
###   CREATE GENEALOGY                                                      ###
###############################################################################

genealogy <- data.frame(child = paste(spreadsheet$`Imię Doktoranta`, spreadsheet$`Nazwisko Doktoranta`, sep = "\n"),
                        parent = paste(spreadsheet$`Imię Promotora`, spreadsheet$`Nazwisko Promotora`, sep = "\n"),
                        year = spreadsheet$`Rok obrony doktoratu`,
                        university = spreadsheet$`Uczelnia nadająca stopień doktora Doktorantowi`)

genealogy$parent <- replace(genealogy$parent, which(genealogy$parent=="NA\nNA"), NA)

###############################################################################
###   CREATE TREE                                                           ###
###############################################################################

library(ggenealogy)

ig <- dfToIG(genealogy, vertexinfo = c("year","university"))
getBasicStatistics(ig)

###############################################################################
###   PLOTS                                                                 ###
###############################################################################

library(ggraph)
library(magrittr)

my_theme <- theme_minimal() + theme(panel.grid.minor.x = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.ticks.x = element_blank())

ig %>%
  create_layout(layout = "sugiyama") -> lay

lay$y <- lay$x
lay$x <- lay$year

ggraph(lay) +
  my_theme +
  geom_node_text( aes(label=name), size=1 ) +
  geom_edge_link( arrow = arrow(length = unit(1,"mm")),
                  start_cap = circle(3, "mm"),
                  end_cap = circle(3, "mm"))
ggsave("../plots/drzewo2.png", device="png", width = 30, height = 50, units = "cm")