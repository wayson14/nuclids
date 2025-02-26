#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <start relcode of experiments to process> <end relcode...>"
  exit 1
fi

n="$1"
s="$2"
# Optional: Validate that n is a positive integer
if ! [[ "$n" =~ ^[0-9]+$ ]]; then
  echo "Error: n must be a positive integer."
  exit 1
fi

# Iterate n times, appending n value to output.txt
mkdir ../hivapergs 
mkdir ../results
whole_start_time=$(date +%s)
for (( i=n; i<s+1; i++ )); do
    start_time=$(date +%s)
    echo "================================================================================="
    echo "Processing relocde ${i}..."
    cp "../hivapein.dat.${i}" ./hivapein.dat && echo "Copied hivapein.dat.${i} into simulation_box"
    ./hivapn || echo "^^^ Error in hivapn ^^^"
    cp hivaperg.dat "../hivapergs/hivaperg_${i}.dat" && echo "Copied hivaperg.dat.${i} into hivapergs"
    python hi2txt.py -v hivaperg.dat "output_${i}.dat" || echo "^^^ Error in hi2txt.py ^^^"
    mv "output_${i}.dat" "../results/output_${i}.dat" && echo "Copied output_${i}.dat into results"
    end_time=$(date +%s)
    elapsed_time=$(( end_time - start_time ))
    echo "Execution time: $elapsed_time seconds"
    # echo "$n" >> output.txt
done
whole_end_time=$(date +%s)
whole_elapsed_time=$(( whole_end_time - whole_start_time ))
echo "================================================================================="
echo "Whole procedure execution time: $whole_elapsed_time seconds"
# echo "Appended '$n' to output.txt, $n times."



#mv ../hivapein.dat.3 ./hivapein.dat
#./hivapn
#python hi2txt.py hivaperg.dat output_3.dat
#mv output_3.dat ../results/output_3.dat