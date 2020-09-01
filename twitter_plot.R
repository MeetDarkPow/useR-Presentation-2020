library(rtweet)
library(ggplot2)
acts <- read.csv("https://raw.githubusercontent.com/r-community/tweet-explorer/master/twitter_account.csv", encoding = "UTF-8")
acts2 <- gsub("@", "", acts$Twitter_account)
usrs <- lookup_users(acts2)
usrs_df <- usrs[,c("screen_name","followers_count")]
usrs_df <- usrs_df[usrs_df$screen_name!="plotlygraphs",]
new_df <- head(usrs_df[order(-usrs_df$followers_count),],20)
head(new_df,20)
ggplot(data=new_df, aes(x=screen_name, y=followers_count, group=1)) +
  geom_line(color="red")+
  geom_point() +
  labs(title = "Top 20 most followed R accounts on Twitter", 
    x = "Twitter Account Names", y="Twitter Followers Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_label(
    label=new_df$followers_count, 
    nudge_x = 0.1, nudge_y = 0.1
  )
