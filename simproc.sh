#!/bin/bash

if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ] || [ -z "$5" ]; then
  echo "Usage: $0 <csv_input (might be provided by scraper)> <start relcode> <end relcode...> <n_channel> <p_channel>"
  exit 1
fi

source="$1"
start_relcode="$2"
stop_relcode="$3"
max_n="$4"
max_p="$5"

#python scraper.py "$start_relcode" "$stop_relcode"
python hip.py "$source" "$start_relcode" "$stop_relcode" "$max_n" "$max_p"
cd simulation_box
./hivap_operator.sh "$start_relcode" "$stop_relcode"