#reusable code for ggplot2
# + needs to come at the end of a line!!
ggplot(data = <DATA>) + 
    <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = hwy, y = cyl))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, color = class))

#manual asesthetic setting, note color outside geom_point
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, shape = drv, color = displ > 2))


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point() + 
    geom_smooth()
# is = to

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

#note se

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point(mapping = aes(color = class)) + 
    geom_smooth(data = filter(mpg, class == "subcompact"))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, stroke= 2)) + 
    geom_point(mapping = aes( fill = drv), size = 3, color = 'white', shape = 21) + 
    geom_smooth( mapping = aes( linetype = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_jitter()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_count()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot() + 
    coord_flip()

bar <- ggplot(data = diamonds) + 
    geom_bar(
        mapping = aes(x = cut, fill = color), 
        show.legend = FALSE,
        width = 1,
        position = 'fill'
    ) + 
    theme(aspect.ratio = 1) +
    labs(x = NULL, y = NULL)

state = map_data("state")

ggplot(state, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    coord_quickmap()

ggplot(diamonds) +
    stat_count(mapping = aes( x = cut, fill = ..count.. ))

ggplot( data = Orange, aes(x = age, y = circumference, color = Tree))+
    geom_line()

ggplot( data = Puromycin, aes( x = rate, y = conc, ))
