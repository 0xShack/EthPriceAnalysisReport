"""Etherscan.io webscrape of block data
   by Caleb Shack
"""

# imports
import pandas as pd
from bs4 import BeautifulSoup
from datetime import datetime
import time
import requests

def runScrape():
    df_list = []

    # change index for more or less blocks
    for i in range(1, 19600):
        url = f'https://etherscan.io/blocks?ps=100&p={i}'
        block = requests.get(url, headers={
            'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36'})
        code = block.status_code
        if code == 200:
            soup = BeautifulSoup(block.content, 'html.parser')
            tbl = soup.find("table")
            df = pd.read_html(str(tbl))[0]
            df = df.rename(columns={'Block': 'block', 'Unnamed: 1': 'timestamp',
                                    'Txn': 'txn', 'Uncles': 'uncles', 'Miner': 'miner',
                                    'Gas Used': 'gas_used', 'Gas Limit': 'gas_limit',
                                    'Base Fee': 'base_fee', 'Reward': 'reward',
                                    'Burnt Fees (ETH)': 'burnt_fees'})
            df['extract_time'] = datetime.now()
            df['txn_link'] = df['block'].apply(lambda x: f'https://etherscan.io/txs?block={x}')
            df_list.append(df)
            print(f'page {i} successfully retrieved')
        else:
            print(f'!!!!!!!!!!!!page {i} failed with status code {code}!!!!!!!!!!!!')
        time.sleep(1)

    block_df = pd.concat(df_list).reset_index(drop=True)

    # save as csv
    path = 'C:/...' # input file path here
    df.to_csv(path, index = False)

if __name__ == '__main__':
    runScrape()
    print('scrape complete')
