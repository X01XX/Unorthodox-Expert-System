#!/usr/bin/bash

# Given a directory of Rust source files, some with test modules at the end.

# Move the files to a work directory, removing the test modules and comments.

# Get a list of fuction names.

# Search the files for each function name.

# For function names that have only one reference, that is the function definition,
# and will be displayed.

# A function may be in a trait, and used implicitly, so should be kept.

# A function may be unsed only in test modules, so may be moved there.

# Othewise the function may be deleted.

# Clear/create work directory.
if [ -d ~/rust/ues/work ]
then
	rm -R ~/rust/ues/work
fi
mkdir ~/rust/ues/work

# Put each Rust file into work directory, without test code, or comments.
for f in ~/rust/ues/src/*.rs
do
	# echo $f
	# diff /media/$USER/UDISK/rust/$UES/src/$(basename -- $f) $f
	count=`grep -c "mod\stests" $f`
	if [ $count -lt 1 ]
	then
	       	# echo $f no
		file=`(basename -- $f)`
		cat $f | sed -e 's/\/\/.*//' > ~/rust/ues/work/$file
	else
		linenum=`grep -n "mod tests" $f | sed -e 's/:.*//g'`
	       	# echo $f yes $linenum
		file=`(basename -- $f)`
		head -$linenum $f | sed 's/\/\/.*//' > ~/rust/ues/work/$file
	fi

done

# Combine the work files.
cat ~/rust/ues/work/*.rs >  ~/rust/ues/work/tmp.txt

# Parse out function names.
functions=`grep "\sfn\s" ~/rust/ues/work/*.rs | sed -e 's/.*fn\s//' | sed -e 's/<.*//' | sed -e 's/(.*//' | sort -u`

# Check the number of references for each function name.
for line in $functions
do
	line2=`echo $line | tr -d '\n'`
	count=`grep -c $line ~/rust/ues/work/tmp.txt`
	if [ $count -lt 2 ]
       	then
	    fileat=`grep -l $line2 ~/rust/ues/work/*.rs`
	    fileat2=`(basename -- $fileat)`
	    echo "-> $count " $line2 " " $fileat2

	fi
done
