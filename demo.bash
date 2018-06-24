#!/usr/bin/env bash

echo "setup emacs with top window as Demo"
echo "bottom as scratch"

read 
sleep 2

stepsleeptime=1
scrolldown() { xdotool key ctrl+alt+v  sleep .25; }

## 0
# go to now page
xdotool \
   key ctrl+c n \
   sleep .5 \

## 1
# find link to emacs page
xdotool \
   key ctrl+c l \
   sleep .5 \
   type --delay 30 "zim emacs.txt"
# and insert it
xdotool \
   sleep .5 \
   key Return
sleep $stepsleeptime

# change start to change link
xdotool \
   key T key colon \
   sleep .25 \
   type "ct]"
# to "Example"
xdotool type Example 
sleep $stepsleeptime

## 2
# go there
scrolldown
xdotool \
   key Escape key F key colon \
   key ctrl+c key Return
sleep $stepsleeptime
   
## 3
# scroll down on the top window
scrolldown
# add now link
xdotool \
  key G key key i key Return key Return \
  sleep .5 \
  key ctrl+c key N key Return 
sleep $stepsleeptime

## 4
# scroll down on the top window
scrolldown
xdotool type "testing out zw-mode. It's not nearly as fancy as "
sleep $stepsleeptime

## 5
# insert link to cider
scrolldown
xdotool \
   key ctrl+c key ctrl+l sleep .5 \
   type --delay 30 "emacs cider install"

xdotool sleep .5 key Return
sleep $stepsleeptime

## 6
# go to cider page, called 
scrolldown
xdotool key ctrl+c key f sleep .5 type "simple-stat"
xdotool sleep .5 key Return
sleep $stepsleeptime

## 7 
scrolldown
# link to previous page
xdotool key G key i key Return key Return
xdotool key ctrl+c ctrl+p type " is another emacs package. But I should revisit this one" 
sleep $stepsleeptime

## 8 
scrolldown
# link page to now page
xdotool \
  key ctrl+c key ctrl+n \
  sleep .5 \
  key i type " - revisit this package"
sleep $stepsleeptime

## 9
xdotool key Return key Return type "a:b:c"
xdotool sleep .5 key ctrl+c key w
sleep $stepsleeptime

