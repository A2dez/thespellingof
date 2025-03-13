library(magrittr)
library(stringr)
# install.packages("stringr")
URL = "https://norvig.com/ngrams/count_1w.txt"

wordlist <- read.table(URL) 

wh_words <- wordlist[str_detect(wordlist$V1,"wh"),]
View(wh_words)


# circlepackeR map --------------------------------------------------------

data1 <- wh_words[1:20,]
v3 <- c("wh", "wh", "wh", "wh", "wh", 
        "lexical", "lexical", "wh", 
        "wh", "other", "other", "other", 
        "wh", "other", "lexical", "compound", "lexical", 
        "compound", "compound", "compound")

data2 <- wh_words[21:1020,]
data2$V3 <- c("other", rep(c("other", "lexical", "compound"), 333))


data2$pathString <- paste("wordgroup", 
                          data2$V3, 
                          data2$V1, 
                          sep = "/")

population <- as.Node(rbind(data1,data2) %>% select(V1, V3, V2, pathString))
circlepackeR(population, 
             size = "V2", 
             color_min = "hsl(56,80%,80%)", 
             color_max = "hsl(341,30%,40%)")  


# -------------------------------------------------------------------------


# library
library(treemap)
library(shiny)

# Build Dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

# treemap
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="index"
) 

data1 <- wh_words[1:20,]
v3 <- c("wh", "wh", "wh", "wh", "wh", 
        "lexical", "lexical", "wh", 
        "wh", "other", "other", "other", 
        "wh", "other", "lexical", "compound", "lexical", 
        "compound", "compound", "compound")
data1$V3 <- v3

treemap(data1,
        index=c("V3","V1"),
        vSize="V2",
        type="index"
) 
glimpse(data1)
# -------------------------------------------------------------------------

# treemap r graphy gallery ------------------------------------------------




# -------------------------------------------------------------------------
https://r-graph-gallery.com/315-hide-first-level-in-circle-packing.html

# -------------------------------------------------------------------------
c("ggraph", "igraph", "tidyverse", "viridis") %>% 
  sapply(install.packages)

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
library(plotly)
# install.packages("plotly")

# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
View(flare)
edges=flare$edges
vertices = flare$vertices
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Hide the first level (right)
(ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE") ) %>% 
  ggplotly


# # Circlepacker package-------------------------------------------------------------------------


install.packages("devtools")
install.packages("data.tree")
library(circlepackeR)       
library(data.tree)
devtools::install_github("jeromefroe/circlepackeR") # If needed

# create a nested data frame giving the info of a nested dataset:
data <- data.frame(
  root=rep("root", 15),
  group=c(rep("group A",5), rep("group B",5), rep("group C",5)), 
  subgroup= rep(letters[1:5], each=3),
  subsubgroup=rep(letters[1:3], 5),
  value=sample(seq(1:15), 15)
)

data1
data
  
  # Change the format. This use the data.tree library.
  # This library needs a column that looks like root/group/subgroup/..., so I build it
  library(data.tree)
  data$pathString <- paste("world", data$group, data$subgroup, data$subsubgroup, sep = "/")
  population <- as.Node(data)
  population <- as.Node(data1) 
# Make the plot
  circlepackeR(population, size = "value")
  
  # You can custom the minimum and maximum value of the color range.
  (p <- circlepackeR(population, 
                    size = "value", 
                    color_min = "hsl(56,80%,80%)", 
                    color_max = "hsl(341,30%,40%)")
  )
  
  # save the widget
  # library(htmlwidgets)
  # saveWidget(p, file=paste0( getwd(), "/HtmlWidget/circular_packing_circlepackeR2.html"))

# -------------------------------------------------------------------------
# https://jeromefroe.github.io/circlepackeR/
  
  library(circlepackeR)
  library(data.tree)
  library(treemap)
  
  data(GNI2014)
  head(GNI2014)
  GNI2014$pathString <- paste("world", 
                              GNI2014$continent, 
                              GNI2014$country, 
                              sep = "/")
  # <- GNI2014[GNI2014$continent == "Asia",]
  population <- as.Node(GNI2014)
  population <- as.Node(GNI2014 %>% select(country, continent, population, pathString))
  
  data1$pathString <- paste("wordgroup", 
                            data1$V3, 
                            data1$V1, 
                            sep = "/")
  
  population <- as.Node(data1 %>% select(V1, V3, V2, pathString))
  # ?as.Node
  
  circlepackeR(population, 
               size = "V2", 
               color_min = "hsl(56,80%,80%)", 
               color_max = "hsl(341,30%,40%)")  
  
  # ?circlepackeR
  

  

# -------------------------------------------------------------------------

  # libraries
  library(packcircles)
  library(ggplot2)
  library(viridis)
  
  # Create data
  data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 
  
  # Generate the layout
  packing <- circleProgressiveLayout(data$value, sizetype='area')
  packing$radius <- 0.95*packing$radius
  data <- cbind(data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=50)
  
  # Plot 
  ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = data, aes(x, y, size=value, label = group), color="black") +
    theme_void() + 
    theme(legend.position="none")+ 
    coord_equal()  
  