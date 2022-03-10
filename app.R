library(dash)
#library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

genres <- c("pop","rap","rock","latin","r&b","edm")

app$layout(
    dbcContainer(
        list(
            htmlH1('Dongxiao Li: DashR heroku deployment'),
            dccGraph(id='plot-area'),
            htmlBr(),
            htmlLabel('Popularity Slider'),
            dccRangeSlider(
                id = 'pop-slider',
                min = 0,
                max = 100,
                marks = list("0"="0", "25"="25","50"="50","75"="75","100"="100"),
                value = list(5,100)
            ),
            htmlBr(),
            htmlLabel('Music Genre Dropdown Menu'),
            dccDropdown(
                id='genre-select',
                options = genres %>% purrr::map(function(genre, pop) list(label = genre, value = genre)),
                value=list("rock","pop"),
                multi=TRUE)
            
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('genre-select', 'value'),
         input('pop-slider','value')),
    function(genre, pop) {
        p <- df %>% 
            filter(playlist_genre %in% genre) %>%
            filter(track_popularity >= pop[1] & track_popularity <= pop[2]) %>%
             ggplot(aes(y = playlist_genre,
                 fill = playlist_genre)) +
            geom_bar(alpha = 0.6) +
            xlab("Music Genres") +
            ylab("Number of Song Records") +
            ggthemes::scale_fill_tableau() +
            theme_bw()
        p <- p + guides(fill=guide_legend(title="Music Genre"))
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
