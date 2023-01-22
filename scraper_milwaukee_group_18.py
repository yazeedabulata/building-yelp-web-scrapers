# -*- coding: utf-8 -*-
"""
Created on Sun May 22 17:15:05 2022

@author: yaz
"""

# Importing needed packages
import requests
from bs4 import BeautifulSoup
import pandas as pd
from re import search
import re

################Create the Motherlist containing the names, links, overall rating and number of reviews

# Gathering the names and links for all bars in Milwaukee (24 pages)
url_yelp = 'https://www.yelp.com/search?find_desc=Bars&find_loc=Milwaukee%2C+WI&start='
s = requests.Session()

#Create empty list variables
name_bars = []
link_bars = []
ovr = []
nreviews = []

#Loop through all 24 restaurant pages to collect the name, business link, overall rating and number of reviews
for j in range(0,24):
    
    html = s.get(url_yelp + str(j*10), timeout=4)
    
    soup = BeautifulSoup(html.content, "lxml")
    links = soup.select(".css-1egxyvc .css-1m051bw")
   
    for link in links:
        name_bars.append(link.string)
        link_bars.append(link.get('href'))
        
    soup_ovrs = soup.select('.overflow--hidden__09f24___ayzG')[2:]

    for x in soup_ovrs:
        ovr.append(x.attrs['aria-label'])
    
    soup_nreviews = soup.select('.reviewCount__09f24__tnBk4')
    
    for y in soup_nreviews:
        nreviews.append(y.text)  
    print(j)
    
# Completing the bar links & clean the overall rating column
link_bas=["https://www.yelp.com" + s for s in link_bars]
ovr  = [re.sub(' star rating', '',  i) for i in ovr]

#Putting the restaurant list into a csv
BarDataSet = list(zip(name_bars,link_bars,ovr, nreviews))
df_bars = pd.DataFrame(data = BarDataSet , columns=['name', 'url', 'overall_rating', 'n_reviews'])

#Check that no duplicates were generated
df_bars['url'].nunique()

#Convert Motherlist to CSV
df_bars.to_csv('milwaukee_bars_yelp.csv',index=False,header=True,encoding='utf8')

##################Using the Motherlist we scrape the last ten reviews from all Bars in Milwaukee#######################################
#The collected mandatory variables are the username, rating, date of review and review text as prescribed
#Additional variables collected are the useful, cool and funny votes of each review, the hometown of the reviewer, as well as the category tags and price range of the bars
for n in range(0,240):
    try:                                                    
        #Extract url of bar
        url = df_bars['url'][n]

        html = s.get(url, timeout=10)
        soup = BeautifulSoup(html.content, 'lxml')
        
        soup_username = soup.select('.css-ux5mu6 .css-1m051bw')
        
        username = []
        
        for name in soup_username:
            username.append(name.string)
            
        #Find row indices of valid reviews
        soup_reviews_info =soup.select('.margin-b1-5__09f24__NHcQi .vertical-align-middle__09f24__zU9sE')
        valid_review_index = []
        
        for index,item in enumerate(soup_reviews_info):
            if not(search("Previous", item.get_text())):
                valid_review_index.append(index)
        
        # Get review ratings
        soup_stars=soup.select('#main-content .vertical-align-middle__09f24__zU9sE .overflow--hidden__09f24___ayzG')
        
        rating = []
        for stars in soup_stars:
            rating.append(stars.attrs['aria-label'])
        
        # Get valid review ratings, based on valid review index list
        rating = [rating[index] for index in valid_review_index]
        
        # Get rid of text "star rating" & convert from string to number
        rating  = [re.sub(' star rating', '',  r) for r in rating]
        rating = [float(i) for i in rating]
        
        #Get date of rating
        soup_date = soup.select('.margin-b1-5__09f24__NHcQi .css-chan6m')
        
        # Get valid dates
        soup_date = [soup_date[index] for index in valid_review_index]
        
        date_review = []
        
        for extract_date_review in soup_date:
            date_review.append(extract_date_review.get_text())
      
        #Get the review text
        review_texts =soup.select('.comment__09f24__gu0rG')
        review_texts = [review_texts[index] for index in valid_review_index]
        review_text = []
        
        for t in review_texts:
            review_text.append(t.get_text())
        
        ####Additional Variables####
        #Get Hometown of Users
        soup_location=soup.select('.arrange-unit-fill__09f24__CUubG .border-color--default__09f24__NPAKY .border-color--default__09f24__NPAKY .border-color--default__09f24__NPAKY .border-color--default__09f24__NPAKY .css-qgunke')
        location = []

        for l in soup_location:
            location.append(l.get_text())
            
        #Get Useful Votes
        soup_useful=soup.select('.margin-r1-5__09f24__ot4bd:nth-child(1) .css-12i50in')

        useful = []
        for votes1 in soup_useful:
            useful.append(votes1.get_text())
        
        #Strip word Useful from selection and replace it with zero if the review received no useful votes
        useful  = [re.sub('Useful', '0',  u) if u == "Useful" else re.sub('Useful ', '',  u) for u in useful]

        #Get Funny Votes
        soup_funny=soup.select('.margin-r1-5__09f24__ot4bd:nth-child(2) .css-12i50in')

        funny = []
        for votes2 in soup_funny:
            funny.append(votes2.get_text())
            
        #Strip word Funny from selection and replace it with zero if the review received no funny votes
        funny  = [re.sub('Funny', '0',  f) if f == "Funny" else re.sub('Funny ', '',  f) for f in funny]

        #Get Cool Votes
        soup_cool=soup.select('.margin-r1-5__09f24__ot4bd:nth-child(3) .css-12i50in')

        cool = []
        for votes3 in soup_cool:
            cool.append(votes3.get_text())
            
        #Strip word Cool from selection and replace it with zero if the review received no cool votes
        cool = [re.sub('Cool', '0',  c) if c == "Cool" else re.sub('Cool ', '',  c) for c in cool]
        
        ####Add Business Information####
        #Extract already existing bar-level variables
        name_business = df_bars['name'][n]
        ovr = df_bars['overall_rating'][n]
        nreviews = df_bars['n_reviews'][n]
        #Multiply to add Business Name, Link, Overall Rating and Number of Reviews List
        name_business_mult = [name_business] * len(username)
        url_bar_mult = [url] * len(username)
        ovr_mult = [ovr] * len(username)
        nreviews_mult = [nreviews] * len(username)
        
        #Add Business Tags
        soup_tags = soup.select('.css-1fdy0l5 .css-1m051bw')
        tags = []
        for labels in soup_tags:
            tags.append(labels.get_text())
            
        #Merge Business Tags into one Category Column and Multiply into List    
        categories = ', '.join(tags)
        categories_mult = [categories] * len(username)
        
        #Add Business Price Range: If not provided replace with 'NA'
        soup_price = soup.select('.css-1ir4e44')
        
        if len(soup_price)>0:
            price = soup_price[0].text.strip()
        else:
            price = str('NA')
        price_mult = [price] * len(username)

        ####Combine and Add to dataset####
        RatingDataSet = list(zip(name_business_mult, url_bar_mult, ovr_mult, nreviews_mult, categories_mult, price_mult, username, rating, date_review, review_text, location, useful, funny, cool))

        df_rating = pd.DataFrame(data = RatingDataSet, columns=['name_business', 'url', 'overall_rating', 'total_reviews', 'business_category', 'price', 'username', 'rating', 'date_review', 'text', 'hometown', 'useful', 'funny', 'cool'])

        with open('milwaukee_bar_ratings.csv', 'a',newline='', encoding='utf8') as f:
            df_rating.to_csv(f, index=False, header=False)
                
    except Exception as e:
        print(e)
        print('Error at:' + str(n))
    