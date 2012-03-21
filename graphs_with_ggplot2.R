require("ggplot2")

# Loading Iris dataset
columns <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")
iris <- read.table("/Users/thomas/Documents/data/datasets/iris/iris_learn.csv", sep=',', col.names=columns)


# Simple bar graphs wrapped inside a function
# Showing the mean value of a given variable
graph.mean <- function (variable) {
                 
  qplot(
    class, 
    get(variable),
    data=iris,
    stat="summary",
    fun.y="mean",
    geom="bar",
    width=.5,
    fill=factor(class)
  )
  
  last_plot() + coord_flip() + ylab(variable)
                 
}
graph.mean("sepal_length")
graph.mean("sepal_width")
graph.mean("petal_length")
graph.mean("petal_width")


# Boxplots showing variable distribution
graph.distrib <- function (variable) {
  
  qplot(
    factor(class), 
    get(variable),
    data=iris, 
    geom="boxplot",
    fill=factor(class)
  )
  
  last_plot() + coord_flip() + ylab(variable) + xlab("class")
  
}
graph.distrib("sepal_length")
graph.distrib("sepal_width")
graph.distrib("petal_length")
graph.distrib("petal_width")


# Correlation between 2 variables
scatter <- function (variable1, variable2) {
  
  qplot(
    get(variable1), 
    get(variable2),
    data=iris, 
    geom="point",
    colour=factor(class)
    )
  
  last_plot() + xlab(variable1) + ylab(variable2) + stat_smooth(
    method=lm, 
    aes(fill = factor(class)), 
    alpha = 0.1
    )
  
}
scatter("sepal_length", "sepal_width")
scatter("petal_length", "petal_width")


# Another way to look at distribution
#graph.mix <- function (variable) {
#
#  cde <- geom_histogram (
#    position="fill", 
#    binwidth=.1
#  )
#  
#  ggplot(
#    iris, 
#    aes(x=get(variable), fill=class)
#  ) + cde
#  
#}
#graph.mix("sepal_length")
#graph.mix("sepal_width")
#graph.mix("petal_length")
#graph.mix("petal_width")
