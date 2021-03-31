#AAAAAAAAAAAAAAAAAAAAADseNQEAAAAAgtlmV%2FFosmp8LQh3Kf7pvyVTmjU%3DZlMTvilLqX2MGCUzZqb4huoyAwkJTVKjQ8y7cuoEFhCaWJghqk

import requests
import os
import json



def create_url():
    query = "covidfatigue -is:retweet"
    # Tweet fields are adjustable.
    # Options include:
    # attachments, author_id, context_annotations,
    # conversation_id, created_at, entities, geo, id,
    # in_reply_to_user_id, lang, non_public_metrics, organic_metrics,
    # possibly_sensitive, promoted_metrics, public_metrics, referenced_tweets,
    # source, text, and withheld
    tweet_fields = "tweet.fields=author_id,created_at"
    url = "https://api.twitter.com/2/tweets/search/recent?query={}&{}".format(
        query, tweet_fields
    )
    return url


def create_headers(bearer_token):
    headers = {"Authorization": "Bearer {}".format(bearer_token)}
    return headers


def connect_to_endpoint(url, headers):
    response = requests.request("GET", url, headers=headers)
    print(response.status_code)
    if response.status_code != 200:
        raise Exception(response.status_code, response.text)
    return response.json()


def main():
    bearer_token = 'AAAAAAAAAAAAAAAAAAAAADseNQEAAAAAgtlmV%2FFosmp8LQh3Kf7pvyVTmjU%3DZlMTvilLqX2MGCUzZqb4huoyAwkJTVKjQ8y7cuoEFhCaWJghqk'
    url = create_url()
    headers = create_headers(bearer_token)
    json_response = connect_to_endpoint(url, headers)
    print(json.dumps(json_response, indent=4, sort_keys=True))


if __name__ == "__main__":
    main()