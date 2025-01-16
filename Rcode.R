install.packages("tidyverse")
library(readr)
library(tidyverse)

data1<- read_tsv("D:\\Aut_semester\\introduction to data science\\dataset\\title.basics.tsv") # Read the file
data2<- read_tsv("D:\\Aut_semester\\introduction to data science\\dataset\\title.ratings.tsv")
View(data1)
View(data2)
result <- left_join(data2, data1, by = "tconst")  #Perform a left join of data1 to data2
View(result)
result_1<-select(result,tconst,titleType,primaryTitle,startYear,genres,averageRating,numVotes) #Select the relevant columns
View(result_1)
result_1<-result_1[result_1$titleType=="movie",] # Filter the movie rows
View(result_1)

result_1$genres <- as.character(result_1$genres) # Ensure that the genres column is of string type

result_1$genres[result_1$genres == "\\N"] <- NA #Replace \N with NA

result_1 <- result_1[!is.na(result_1$genres), ] #Remove rows containing NA
View(result_1)

genre_seperate<-result_1 %>% separate_rows(genres,sep=",") #A single work may belong to multiple genres. These should be split, and then the average ratings for each genre should be calculated.
View(genre_seperate)

genre_seperate_rating_average<-group_by(genre_seperate,genres) %>% summarise(count=n(),AverageRating=mean(averageRating, na.rm=TRUE))
View(genre_seperate_rating_average)

library(ggplot2)
ggplot(genre_seperate_rating_average, aes(x = reorder(genres, count), y = count, fill = genres)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Movie Genres by Count",
       x = "Genres",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a scatter plot.
ggplot(genre_seperate_rating_average, aes(x = reorder(genres, -AverageRating), y = AverageRating)) +
  geom_point() +
  coord_flip() +  
  labs(
    title = "Rating of Different Types of Movies",
    x = "Genres",
    y = "Average Rating"
  ) +
  theme_minimal()

genre_seperate <- genre_seperate[!is.na(genre_seperate$startYear), ]  # Remove rows where the 'startYear' column contains NA.
genre_seperate <- genre_seperate[genre_seperate$startYear != "\\N", ]         # Remove rows where the 'startYear' column contains "\\N".

genre_seperate <- genre_seperate %>%
  mutate(startYear = as.numeric(startYear))

divided_decade<-genre_seperate %>%mutate(decade=floor(startYear/10)*10)
View(divided_decade)






# Calculate the total box office for each genre in each decade.
popular_genres_by_decade <- divided_decade %>%
  group_by(decade, genres) %>%
  summarise(totalVotes = sum(numVotes), .groups = 'drop') %>%
  arrange(decade, desc(totalVotes))

# Identify the most popular genre in each decade.
most_popular_each_decade <- popular_genres_by_decade %>%
  group_by(decade) %>%
  slice_max(totalVotes) %>%
  ungroup()
View(most_popular_each_decade)

#visualization
ggplot(most_popular_each_decade, aes(x = decade, y = totalVotes, fill = genres)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = genres), angle = 90, vjust = 0.5, hjust = 0) +  # 添加文本标签
  labs(title = "Most Popular Movie Genres by Decade", x = "Decade", y = "total votes")


# Assuming the data is already grouped by decade
# Group the data by decade and genre, then calculate the average rating.
genre_by_decade <- divided_decade %>%
  group_by(decade, genres) %>%
  summarise(AverageRating = mean(averageRating, na.rm = TRUE)) %>%
  ungroup()

# Select the genre with the highest average rating from each decade.
top_genre_by_decade <- genre_by_decade %>%
  group_by(decade) %>%
  slice_max(AverageRating, n = 1) %>%
  ungroup()

ggplot(top_genre_by_decade, aes(x = decade, y = AverageRating, fill = genres)) +
  geom_col() +  # Use a bar chart to display.
  geom_text(aes(label = genres), angle = 90, hjust = 0,vjust = ifelse(top_genre_by_decade$decade == "1970" & top_genre_by_decade$genres == "Reality-TV", -0.5, 0.5)) +  # 添加文本标签显示最高评分的类型
  labs(title = "Highest Average Rated Movie Genre by Decade",
       x = "Decade",
       y = "Average Rating") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")  # Use a visually appealing color palette.

View(genre_by_decade)
View(top_genre_by_decade)


library(ggplot2)

# Fit the model and make predictions for the next ten years.
future_decade <- max(genre_by_decade$decade) + 10
predictions <- genre_by_decade %>%
  group_by(genres) %>%
  do({
    model <- lm(AverageRating ~ decade, data = .)
    data.frame(genres = .$genres[1], decade = future_decade, predictedRating = predict(model, newdata = data.frame(decade = future_decade)))
  }) %>%
  ungroup()

# Select the genre with the highest predicted votes.
most_popular_genre <- predictions %>%
  arrange(desc(predictedRating)) %>%
  slice(1) %>%
  pull(genres)


print(most_popular_genre)# Output the most popular genre.
View(predictions)
# visualization
ggplot(predictions, aes(x = decade, y = predictedRating, color = genres)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Popularity of Movie Genres for Next Decade", x = "Decade", y = "Predicted Averageratings")





































