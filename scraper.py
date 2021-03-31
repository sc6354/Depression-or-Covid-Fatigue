import tweepy
import os 
import pandas as pd
import csv
import re 
import string

api_key =  "4vpDORM9o8HoNlUB0LkLGwPrl"
api_secret_key = "qqhok8uKdHhJVgyXsRXOBv699Ccq9sJp4p2PIoZncPU29FSWyq"
access_token =  "1125284051571564544-hCQhN5LFncslgOd5Zt0w9DgIjkwRNv"
access_token_secret =  "1T8GvsSpnwLxbfFtTmT1gS540WJEEQxeFdiFOZbh9cr9r"
# authorize the API Key
auth = tweepy.OAuthHandler(api_key, api_secret_key)
# authorization to user's access token and access token secret
auth.set_access_token(access_token, access_token_secret)
# call the api
api = tweepy.API(auth, wait_on_rate_limit=True)

# search terms
search_terms= "#PandemicBlues -filter:retweets"
date_since = "2021-01-01"

csvFile = open('PandemicBlues', 'a')
csvWriter = csv.writer(csvFile)
 
for tweet in tweepy.Cursor(api.search,q=search_terms,count=100,
                           lang="en",
                           since_id=date_since).items():
    csvWriter.writerow([tweet.created_at, tweet.text.encode('utf-8'),tweet.user.screen_name.encode('utf-8'), tweet.user.location.encode('utf-8')])