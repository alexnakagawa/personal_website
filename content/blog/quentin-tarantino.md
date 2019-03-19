---
title: "Quentin Tarantino"
tags: ["movies", "data analysis", "data"]
categories: ["Data Analysis"]
date: 2017-7-25T07:55:00-07:00
draft: true
---

# Tarantino Movie Training Model

When it comes to movies, it is difficult to talk about the historical fiction genre without mentioning the name [Quentin Tarantino](http://m.imdb.com/name/nm0000233/filmotype/director?ref_=m_nmfm_4). The man is the master of dialogue and violence. A perfect example of his craft can be shown below.

![](http://giant.gfycat.com/AdmirableYellowElephantbeetle.gif)

Damn. One begs the question, **"How does this man generate critically-acclaimed movies as well as he does?"** To generalize, Tarantino's style relies heavily on characters who are bellicose in nature, either verbally or physically. Luckily, [FiveThirtyEight](http://fivethirtyeight.com) has a public dataset on Github [here](http://github.com/fivethirtyeight/data/tree/master/tarantino), and has dedicated an entire [article](http://fivethirtyeight.com/features/complete-catalog-curses-deaths-quentin-tarantino-films/) to tracking every curse word and death in seven of Tarantino's best films.

I couldn't imagine that cursing or blood spill in a movie would contribute to a higher critic score, no matter how epic it seems. I decided to do some digging around to see what exactly _what_ caused this director's success.

### Setting Up

```r
tarantino <- read.csv("/users/alexnakagawa/iXperience/tarantino/tarantino.csv")
# This is my local file path for the tarantino.csv dataset downloaded from FiveThirtyEight's GitHub!
```

The initial table named `tarantino` had `movie, type, word, & minutes_in` columns. I had several problems right off the bat.

1. Type had a binaural distinction, meaning that it could either be a `word` or `death`. If it was `death` in that row, that meant that the `word` column would be filled with a null value. The data was super messy.

2. There were no totals for the words and deaths. Also, there were no data on the ratings for each movie. I also needed columns that would show the box office revenue and the budget of each of the movies as well (explained later).

### Data Wrangling

So I got down to business.

```r
# Count how many entries there are for each movie, add movie rating for each one
movies <- count(tarantino, movie)
names(movies)[2] <- "total"
movies$RTrating <- c(88, 89, 87, 85, 84, 94, 90)     # Rotten Tomatoes rating
movies$budget <- c(100, 70, 12, 30, 30, 8.5, 1.2)    # The movie's budget, given in millions USD
movies$boxoffice <- c(425.4, 321.5, 74.7, 180.9, 152.2, 213.9, 2.8)  # Box office, given in millions USD
head(movies)
```

I now had a neater table with the Rotten Tomatoes score of each movie, as well as the total curse words and deaths, budget, and box office. I now shifted my attention to wrangling with the individual words deaths, which I then joined with my initial `movies` table. I also thought that it would be interesting to show the ratio between the deaths and curse words in each movie just for reference, because sometimes it is really difficult to keep track of just how much vulgarity is in Tarantino's movies.

```r
# Separate by number of entries in each category
words <- filter(tarantino, type == 'word')
deaths <- filter(tarantino, type == 'death')
deaths <- deaths[-3]

# Find the total number of deaths and words in each movie
words_total <- group_by(words, movie) %>% summarise(n())
names(words_total)[2] <- "words_per_movie"
deaths_total <- group_by(deaths, movie) %>% summarise(n())
names(deaths_total)[2] <- "deaths_per_movie"

# Combine
movies <- left_join(movies, words_total, by = 'movie')
movies <- left_join(movies, deaths_total, by = 'movie')

# Add new ratio for the number of deaths to curse wordsa
movies$death_word_ratio <- deaths_total$deaths_per_movie / words_total$words_per_movie
new_data <- movies[order(movies$death_word_ratio), ]

summary(movies)
```

Interesting right? Something right off the bat to notice: Tarantino's films average at an 88 rating on Rotten Tomatoes, which is ludicrous. 

### Initial plotting

The following plot shows the frequency (per ~1-2 minutes) of curse words (black) with deaths (red) for each of Tarantino's movies.

```r
ggplot(data = words, aes(x = minutes_in)) + theme_minimal() +
  geom_dotplot(binwidth = 1.0, color = "black") +
  ggtitle("Distribution of Total Curse Words and Deaths in Each Tarantino Film") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(movie ~ ., scales = "free_x") + ylab("Frequency") +
  geom_dotplot(data = deaths, binwidth = 1.0, colour = "red") + 
  theme(strip.text = element_text(face="bold", size=3.9))
```

You can see some of the highlights of movies, such as the scene in which Hitler dies in _Inglorious Basterds_ or that big patch of black towards the end of Pulp Fiction when John Travolta's character tries to save his boss' girl from O.D.

### The failed attempts

Now onto the modeling part. There were some attempts to find a good correlation to work with, but most ended in despair. It just turns out that there really is no work-around with the amount of randomness in the curse words and a film's popularity, it would be too much of a stretch.

Take the example of plotting the Rotten Tomatoes rating as a function of the budget (this is why I added it to the table earlier). You simply cannot judge a movie's merit based off its production value (pay close attention to Jackie Brown).

```r
ggplot(data = movies, aes(x = budget, y = RTrating)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = 1.6 , hjust = "inward", size = 3)
```

Anoter failed attempt below. Again, just illustrating the fact that there is no correlation between the amount of f-bombs and rating.

```r
ggplot(data = movies, aes(x = RTrating, y = words_per_movie)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = -1.0, hjust = "inward", size = 3)
```

### Hitting the Jackpot

Finally, as I was about to toss in the towel, I figured that the amount of budget could be correlated to the amount of box office revenue. It seems to figure that the more effort for special effects or expensive (recognizable) actor faces would equate to a bigger hype surrounding that specific movie. Crossed my fingers and then...

```r
ggplot(data = movies , aes(x = budget, y = boxoffice)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = 1.6 , hjust = "inward", size = 3) +
  ggtitle("Box Office Revenue (millions USD) as a Function of Budget (millions USD)")
```

This was perfect. With a few exceptions, it seemed to be that there was a linear best fit line that I could train using bootstrapping here.

```r
fit <- lm(budget ~ boxoffice, movies)
summary(fit)

train(budget ~ boxoffice, data = movies, method = "lm") # Uses bootstrapping method to train the dataset to be a linear regression model
```

```r
ggplot(data = movies , aes(x = budget, y = boxoffice)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = 1.6 , hjust = "inward", size = 3) +
  ggtitle("Box Office Revenue (millions USD) as a Function of Budget (millions USD)") +
  geom_smooth(method='lm')
```

The `Rsquared` value I got in the end was a solid 0.89%, meaning that my linear regression model has a .89% variance from the mean line, with 0% being perfect. **Mission accomplished**. To test the accuracy of the line, I tested on his most recent feature.

* _The Hateful Eight_, 155.8 million, with 44 million in budget lies within the probable area.

### Conclusion

While I did not reach the conclusion I wanted, I at least was able to prove a linear correlation between a Tarantino movie's production value with the box office results. This means that for any budget he creates, I should be able to predict how much box office revenue he will generate (as long as he gives it as much effort as he has in the past). There is certainly not enough data to make a fully accurate prediction, but it is a good indicator of where his movies stand. That's a lot of pressure on Tarantino to do well. But in all fairness, with the return values he is getting on his movies, a large budget would not be a bad gamble.

![](http://akns-images.eonline.com/eol_images/Entire_Site/2013015/travoltadance.gif)

