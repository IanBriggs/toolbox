#!/bin/bash

# modified from:
# https://misc.flogisoft.com/bash/tip_colors_and_formatting

# escape code, usually one of '\e', '\033', or '\x1B'
esc="\033"


echo "8/16 colors and formatting variations"

# Background
for clbg in {40..47} {100..107} 49 ; do
    # Foreground
    for clfg in {30..37} {90..97} 39 ; do
	# Formatting
	for a in 0 1 2 4 5 7 ; do
	    # Print the result
            msg="${esc}[${a};${clbg};${clfg}m %11s ${esc}[0m"
	    printf "${msg}" "^[${a};${clbg};${clfg}m"
	done
	echo # Newline
    done
done


echo -e "\n\n\n\n"
echo "88/256 colors"

# Foreground / Background
for fgbg in 38 48 ; do
    # Colors
    for color in {0..255} ; do
        # Display the color
        printf "${esc}[${fgbg};5;%sm  %3s  ${esc}[0m" $color $color
        # Display 6 colors per lines
        if [ $((($color + 1) % 6)) == 4 ] ; then
            echo # New line
        fi
    done
    echo # New line
done
