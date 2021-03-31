import tweepy
from tweepy import OAuthHandler
import pandas as pd
import csv
import os
from datetime import datetime    
import time

print("Extracting tweets!")

access_token = "1125284051571564544-hCQhN5LFncslgOd5Zt0w9DgIjkwRNv"
access_token_secret = "1T8GvsSpnwLxbfFtTmT1gS540WJEEQxeFdiFOZbh9cr9r"
consumer_key = "4vpDORM9o8HoNlUB0LkLGwPrl"
consumer_secret = "qqhok8uKdHhJVgyXsRXOBv699Ccq9sJp4p2PIoZncPU29FSWyq"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)

# search terms
search_terms= "#covidburnout -filter:retweets"
date_since = "2021-01-01"
tweets = []

for tweet in tweepy.Cursor(api.search, q=search_terms, lang="en", tweet_mode='extended', since=date_since).items():
	

	try: 
		data = [tweet.created_at, tweet.id, tweet.user._json['location'], tweet.full_text, tweet.user._json['screen_name'], tweet.user._json['name'], tweet.user._json['created_at']]
		data = tuple(data)
		tweets.append(data)

	except tweepy.TweepError as e:
		print(e.reason)
		continue

	except StopIteration:
		break

to_csv_timestamp = datetime.today().strftime('%Y%m%d_%H%M%S')
    # Define working path and filename
path = os.getcwd()
filename = path + to_csv_timestamp + 'all_depression_tweets.csv'
df = pd.DataFrame(tweets, columns = ['created_at','tweet_id', 'tweet_location', 'tweet_text', 'screen_name', 'name', 'account_creation_date'])
df.to_csv(filename, index = False)
#df.to_csv(path_or_buf = '/Users/susanchen/Documents/DS_capstone/covidburnout.csv', index=False)