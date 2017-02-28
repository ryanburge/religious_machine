count <- c(5210, 3093, 1696, 5125, 388, 1212, 3523)
label <- c("Evangelical", "Mainline", "Black Protestant", "Catholic", "Jewish", "Other Religion", "No Religion")
reltrad <- data.frame(label, count)

reltrad$label <- factor(reltrad$label, levels=unique(reltrad$label))

ggplot(reltrad, aes(x=label, y=count)) +  geom_col(fill = "coral", colour = "black") +
  ggtitle("                                            Distribution of Reltrad") + 
  xlab("Religious Tradition") + ylab("Number of Respondents")  + 
  theme(text=element_text(size=16, family="KerkisSans")) + 
  annotate("text", x = 6.65, y = 5250, label = "religioninpublic.blog", size = 5)