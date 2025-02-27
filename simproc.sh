#!/bin/bash
source="$1"
start_relcode="$2"
stop_relcode="$3"


python hip.py "$source" "$start_relcode" "$stop_relcode"
cd simulation_box
./hivap_operator.sh "$start_relcode" "$stop_relcode"