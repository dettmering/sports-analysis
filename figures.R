# Figures

library(ggplot2)

ggplot(a, aes(x = x, y = y)) +
  geom_point(aes(size = Elevation.m, color = Speed.kmh)) +
  xlab("X (m)") +
  ylab("Y (m)") +
  scale_color_continuous(name = "Speed (km/h)") +
  scale_size_continuous(name = "Elevation (m)") +
  coord_equal() +
  theme_bw()

ggplot(a, aes(x = Speed.kmh)) +
  geom_histogram(binwidth = 5) +
  annotate("segment", x = median(a$Speed.kmh, na.rm = TRUE), xend = median(a$Speed.kmh, na.rm = TRUE), y = 0, yend = 1000, color = "red") + 
  xlab("Speed (km/h)") + 
  ylab("Number of points") +
  theme_bw()