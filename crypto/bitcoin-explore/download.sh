#!/bin/bash
cd /mnt/data/blog/crypto/bitcoin-trend
curl -L -o bitcoin-historical-data.zip https://www.kaggle.com/api/v1/datasets/download/mczielinski/bitcoin-historical-data
unzip bitcoin-historical-data.zip
rm bitcoin-historical-data.zip
