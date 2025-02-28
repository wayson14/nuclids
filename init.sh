#!/bin/bash
python -m venv venv
source venv/bin/activate
pip install -r req.txt
./cleaner.sh #clean files from previous simulations
./simproc.sh results.csv 1 5 4 4 #scrape, process, present data from relcodes 1-5, max channel n=4, p=4