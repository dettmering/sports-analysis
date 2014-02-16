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

ggplot(a, aes(x = Cumulative.Distance.m, y = Elevation.m)) +
  geom_area(alpha = 0.8) +
  xlab("Distance (m)") +
  ylab("Elevation (m)") +
  theme_bw()

ggplot(a, aes(x = Cumulative.Distance.m, y = Speed.kmh)) +
  geom_line() +
  annotate("rect", xmin = 0, xmax = max(a$Cumulative.Distance.m, na.rm = T), ymin = mean(a$Speed.kmh, na.rm = TRUE) - sd(a$Speed.kmh, na.rm = TRUE), ymax = mean(a$Speed.kmh, na.rm = TRUE) + sd(a$Speed.kmh, na.rm = TRUE), alpha = .2) +
  annotate("segment", y = mean(a$Speed.kmh, na.rm = TRUE), yend = mean(a$Speed.kmh, na.rm = TRUE), x = 0, xend = max(a$Cumulative.Distance.m, na.rm = T), color = "red") +
  xlab("Distance (m)") +
  ylab("Speed (km/h)") +
  theme_bw()

ggplot(a, aes(x = Speed.kmh)) +
  geom_histogram(binwidth = 5) +
  annotate("segment", x = mean(a$Speed.kmh, na.rm = TRUE), xend = mean(a$Speed.kmh, na.rm = TRUE), y = 0, yend = 1000, color = "red") + 
  xlab("Speed (km/h)") + 
  ylab("Number of points") +
  theme_bw()