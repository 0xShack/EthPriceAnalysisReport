# imports
import pandas as pd
from bs4 import BeautifulSoup
from datetime import datetime, timedelta, time
import time
import requests
import re

def findHrs(x):
  for i, e in enumerate(x):
    if e == 'hrs' or e == 'hr':
      return int(x[i-1])
  return 0

def findMins(x):
  for i, e in enumerate(x):
    if e == 'mins' or e == 'min':
      return int(x[i-1])
  return 0

def findSecs(x):
  for i, e in enumerate(x):
    if e == 'secs' or e == 'sec':
      return int(x[i-1])
  return 0

def runScrape():
    print("starting scrape...")
    df = pd.read_csv('C:/.../blocks.csv') # set file path
    print("Data set is loaded!")
    
    # turns timestamp into a datetime object based on extract_time - timestamp
    # since etherscan gives time data in terms of 'x days y hrs z mins w secs ago'
    timestamp = df['timestamp']
    no_ago = r'(?<=.)ago'
    timestamp = timestamp.apply(lambda x: re.sub(no_ago, '', x))
    timestamp = timestamp.apply(lambda x: x.split())
    days = timestamp.apply(lambda x: int(x[0]) if x[1] == 'days' or x[1] == 'day' else 0)
    hrs = timestamp.apply(findHrs)
    mins = timestamp.apply(findMins)
    secs = timestamp.apply(findSecs)
    remove_mill = r'(?<=.)[.]\d+'
    df['extract_time'] = df['extract_time'].apply(lambda x: re.sub(remove_mill, '', x))
    df['extract_time'] = df['extract_time'].apply(lambda x: datetime.strptime(x, '%Y-%m-%d %H:%M:%S'))
    td = pd.to_timedelta(days, unit='D') + pd.to_timedelta(hrs, unit='h') + pd.to_timedelta(mins,unit='m') + pd.to_timedelta(secs, unit='s')
    time_df = df['extract_time'] - td
    df['timestamp'] = time_df

    print("time is fixed!")

    txn_content = []
    txn_df = pd.DataFrame([])
    crawl_list = df['txn_link'].tolist()

    print("starting txn content scrape...")

    for i, url in enumerate(crawl_list):
        if i % 97 != 0:
            txn_content.append([])
            continue
        block = requests.get(url, headers={
            'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36'})
        code = block.status_code
        if code == 200:
            soup = BeautifulSoup(block.content, 'html.parser')
            tbl = soup.find("table")
            txn_df = pd.read_html(str(tbl))[0]
            tup_col = txn_df[['Method', 'From', 'To', 'Value']].apply(tuple, axis=1)
            tup_lst = tup_col.tolist()
            txn_content.append(tup_lst)
            print(f'page {i+1} successfully retrieved')
        else:
            txn_content.append([])
            print(f'!!!!!!!!!!!!page {i} failed with status code {code}!!!!!!!!!!!!')
        time.sleep(1)

    print("txn content scraped successfully!!")
    df['txn_content'] = txn_content

    # save as csv
    path = 'C:/' # set file path
    df.to_csv(path, index=False)

    print("df saved as csv")

if __name__ == '__main__':
    runScrape()
    print('scrape complete')
