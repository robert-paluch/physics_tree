###############################################################################
###   FUNCTIONS                                                             ###
###############################################################################

make_layout <- function(graph, geneal) {
  l <- data.frame(x = geneal$year,
                  y = 0)
  return(l)
}

getGen <- function(v1, geneal) {
  ancestors <- getAncestors(v1, geneal, dim(genealogy)[1])
  if(dim(ancestors)[1])
    return(max(ancestors$gen)+1)
  else
    return(1)
}

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

genealogy <- data.frame(child = paste(spreadsheet$`Imię Doktoranta`, spreadsheet$`Nazwisko Doktoranta`),
                        parent = paste(spreadsheet$`Imię Promotora`, spreadsheet$`Nazwisko Promotora`),
                        year = as.character(spreadsheet$`Rok obrony doktoratu`),
                        university = spreadsheet$`Uczelnia nadająca stopień doktora Doktorantowi`)

genealogy$parent <- replace(genealogy$parent, which(genealogy$parent=="NA NA"), NA)

###############################################################################
###   CREATE TREE                                                           ###
###############################################################################

library(igraph)
library(ggenealogy)

ig <- dfToIG(genealogy, vertexinfo = c("year","university"))
bs <- getBasicStatistics(ig)
ig.comp <- components(ig)
genealogy$tree.id <- ig.comp$membership
genealogy$gen <- sapply(genealogy$child, getGen, genealogy)

###############################################################################
###   PLOTS                                                                 ###
###############################################################################

library(ggraph)
library(magrittr)

my_theme <- theme_minimal() + theme(panel.grid.minor.y = element_blank(),
                                    panel.grid.major.y = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank())

my_layout <- make_layout(ig, genealogy)

ggraph(ig, layout = my_layout) +
  my_theme +
  geom_node_text( aes(label=name), size=1 ) +
  geom_edge_link( start_cap = circle(3, "mm"),
                  end_cap = circle(3, "mm"))
ggsave("../plots/drzewo3.png", device="png", width = 30, height = 50, units = "cm")