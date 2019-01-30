Lam <- read_csv("data/laminaria.csv")
ggplot(data = Lam, aes( x = total_length , y = blade_length , colour = site)) +
  geom_point( colour = "green") +
  geom_line(aes(group = total_length )) +
  geom_smooth(method = "lm") +
  facet_wrap(~site, ncol = 4)
