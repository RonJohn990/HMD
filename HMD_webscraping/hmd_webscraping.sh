#! /bin/bash

# coun="AUS"
# file="Deaths_1x1.txt"

read coun
read file

curl --anyauth --user rjmorpheus@gmail.com:Password "https://mortality.org/hmd/$coun/STATS/$file"
