Dataset sources:

* One from David Garcia (aggregation service) that gave us tweet ids, Yelena requested those ids to get the full Tweet objects, I extracted the author and Yelena got the timelines of the authors (up to 3200 Tweets).

* One from Yelena

* Control Group from David Garcia, his query at the aggregation service also gave back "NOT query", people who have never mentioned any of the hashtags. Same procedure as above, I randomly sampled users from it (because it's large) and Yelena requested their timelines.

Selection criteria:

* Generally, retweets were filtered.

* Generally, tweets that were empty after tokenization (because they contain only a URL or a hashtag) were filtered.

* For ano and bp groups initial user were selected by having at least 2 tweets with one of the hashtags in their group and none in the other (because there are also some users that have used hashtags from both groups) in the two years of our observation period.

* Final user selection for the groups had the criterion of at least 10 tweets pre and at least 10 tweets post covid (these can be any tweets by the user not necessarily with any hashtag)

Variables:

* Gender information comes from Yelena's script (using the names of Twitter users)

* LIWC

* 11 emotions: Mohammad, S., Bravo-Marquez, F., Salameh, M., & Kiritchenko, S. (2018). SemEval-2018 task 1: Affect in tweets. Proceedings of the 12th International Workshop on Semantic Evaluation, 1–17. https://doi.org/10.18653/v1/S18-1001; in abridged form (only 4 emotions) it's also part of the tweeteval benchmark: Barbieri, F., Camacho-Collados, J., Neves, L., & Espinosa-Anke, L. (2020). TweetEval: Unified Benchmark and Comparative Evaluation for Tweet Classification. ArXiv:2010.12421 [Cs]. http://arxiv.org/abs/2010.12421

* period post: date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15")

* period pre date >= as.Date("2019-03-15") & date < as.Date("2020-03-15")
