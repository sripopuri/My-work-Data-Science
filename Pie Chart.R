# Pie Chart
df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50)
)
head(df)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
pie + scale_fill_manual(values=c("red", "darkblue", "#56B4E9"))
pie + scale_fill_brewer(palette="Dark2")
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal() +
  ggtitle("Pie chart of group and value") +
  theme(plot.title = element_text(face = "bold.italic",hjust = 0.5)) + 
  theme(axis.title = element_text(face = "plain",hjust = 0.5)) +
  labs(x = "",y="")