#!/bin/bash

mv ../hivapein.dat.3 ./hivapein.dat
./hivapn
python hi2txt.py hivaperg.dat output_3.dat
mv output_3.dat ../results/output_3.dat