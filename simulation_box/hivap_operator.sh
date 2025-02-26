#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <relcodes of experiments to process>"
  exit 1
fi

n="$1"

# Optional: Validate that n is a positive integer
if ! [[ "$n" =~ ^[0-9]+$ ]]; then
  echo "Error: n must be a positive integer."
  exit 1
fi

# Iterate n times, appending n value to output.txt
for (( i=1; i<n; i++ )); do
    cp "../hivapein.dat.${n}" ./hivapein.dat
    ./hivapn
    python hi2txt.py hivaperg.dat "output_${n}.dat"
    mv "output_${n}.dat" "../results/output_${n}.dat"
    # echo "$n" >> output.txt
done

# echo "Appended '$n' to output.txt, $n times."



#mv ../hivapein.dat.3 ./hivapein.dat
#./hivapn
#python hi2txt.py hivaperg.dat output_3.dat
#mv output_3.dat ../results/output_3.dat