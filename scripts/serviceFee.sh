#!/bin/bash

# Define the variables
total_lovelace_spent=$1
min_service_lovelace_fee=1500000

# Check if beta tester token is present
if [ "$2" = "true" ]; then
  service_fee=0
else
  lovelace_by_fee_rate=$(echo "scale=0; $total_lovelace_spent * 1 / 100" | bc)
  if (( lovelace_by_fee_rate < min_service_lovelace_fee )); then
    service_fee=$min_service_lovelace_fee
  else
    service_fee=$lovelace_by_fee_rate
  fi
fi

echo $service_fee
