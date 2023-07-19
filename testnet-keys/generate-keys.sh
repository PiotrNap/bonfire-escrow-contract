#!/bin/bash

##
# Provided first argument ($1), creates a set of keys together with a testnet address
##

if [ -z "$1" ]
then
    echo "Missing argument specifying directory"
else
    echo " "
    echo "Generating keys..."
    echo "Target directory: $1"
    echo " "
fi

KEYS_DIR=$1

# Generate a 15-word mnemonic
mnemonic=$(cardano-address recovery-phrase generate --size 15 | tail -1)
echo "Mnemonic: $mnemonic"
echo "$mnemonic" >> $KEYS_DIR/mnemonic

# Create a root key from the mnemonic
root_key=$(echo $mnemonic | cardano-address key from-recovery-phrase Shelley)
echo "Root Key: $root_key"
echo $root_key >> $KEYS_DIR/root_key

# Derive a private key for account 0
acct_0_key=$(echo $root_key | cardano-address key child 1852H/1815H/0H)
echo "Account 0 Key: $acct_0_key"
echo $acct_0_key >> $KEYS_DIR/acct_0_key

# Derive a private key for the external chain (change 0)
ext_chain_key=$(echo $acct_0_key | cardano-address key child 0/0)
echo "External Chain Key: $ext_chain_key"
echo $ext_chain_key >> $KEYS_DIR/ext_chain_key

# Generate a public key for the external chain
ext_chain_pub=$(echo $ext_chain_key | cardano-address key public --with-chain-code)
echo "External Chain Public Key: $ext_chain_pub"
echo $ext_chain_pub >> $KEYS_DIR/ext_chain_pub

# Generate a stake key for the account
stake_key=$(echo $acct_0_key | cardano-address key child 2/0 | cardano-address key public --with-chain-code)
echo "Stake Key: $stake_key"
echo $stake_key >> $KEYS_DIR/stake_key

# Generate a testnet address
addr=$(echo $ext_chain_pub \
  | cardano-address address payment --network-tag testnet \
  | cardano-address address delegation $(echo $stake_key | cardano-address key hash))
echo "Testnet Address: $addr"
echo $addr >> $KEYS_DIR/addr

# Convert extended signing key into Shelley format key
cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $KEYS_DIR/ext_chain_key --out-file $KEYS_DIR/addr.skey
