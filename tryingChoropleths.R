#making a choropleth map
library(magrittr)
library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
str(df)
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(
                z = ~total.exports, text = ~hover, locations = ~code,
                color = ~total.exports, colors = 'Purples'
        ) %>%
        colorbar(title = "Millions USD") %>%
        layout(
                title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
                geo = g
        )

#function to plot plotly things in browser plotPlotly() in usefulFunctions.R
        plotPlotly(p)


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="choropleth/ag")
chart_link


#trying with leaflet
m = leaflet() %>% addTiles() %>% setView(-71.0382679, 42.3489054, zoom = 18)

m  %>% leaflet::getMapData()# the RStudio 'headquarter'
m %>% fitBounds(-72, 40, -70, 43)
m %>% clearBounds() %>%getMapData() # world view
df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles() %>%addTiles()

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
        addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m = leaflet() %>% addTiles()
df = data.frame(
        lat = rnorm(100),
        lng = rnorm(100),
        size = runif(100, 5, 20),
        color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

