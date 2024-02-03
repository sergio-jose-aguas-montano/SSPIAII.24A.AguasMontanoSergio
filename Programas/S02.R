
library(ggplot2)

ggplot(df.Salary.Train) +
 aes(x = YearsExperience, y = Salary) +
 geom_point(shape = "plus", size = 1.5, 
 colour = "#B22222") +
 theme_dark()
