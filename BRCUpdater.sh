#!/bin/bash

#Requires $BRC_PATH be set to the install directory of BRC

mkdir .brc_tmp
git clone https://github.com/tjade273/BigRedCoin .brc_tmp
cur_commit=`git -C $BRC_PATH rev-parse HEAD`
newest_commit=`git -C .brc_tmp rev-parse HEAD`

if [ "$cur_commit" != "$newest_commit" ]; then		
	if [ "$BRC_PATH" == "" ]; then
		echo "Failed to update: Please set BRC_PATH."
	else
		rm -rf $BRC_PATH/*
		cp .brc_tmp/* $BRC_PATH
	fi
else 
	echo "BRC is update to date! Not updating."	
fi

#Cleanup tmp
rm -rf .brc_tmp
