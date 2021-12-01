# EthScrape
Web scraping script for etherscan.io. Retrieves block data listed on https://etherscan.io/blocks.

## etherscan_scrape.py
Scrapes block data from etherscan
output: blocks.csv

## txn_scrape.py
Scrapes transactions from each row in etherscan and appends to blocks.csv a list of list of tuples with (method_name, to_address, from_address, value)
output: new_blocks.csv
