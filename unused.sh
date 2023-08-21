#!/usr/bin/bash

# Given a directory of Rust source files, some with test modules at the end.

# Move the files to a work directory, removing the test modules and comments.

# Get a list of function names.

# Search the files for each function name.

# For function names that have only one reference, that is the function definition,
# and will be displayed.

# A function may be in a trait, and used implicitly, so should be kept.

# A function may be used only in test modules, so may be moved there.

# Otherwise the function may be deleted.

# Set source file dir.
srcdir=~/rust/ues/src

# Set work directory
wrkdir=~/rust/ues/work

# Clear/create work directory.
if [ -d $wrkdir ]
then
	rm -R $wrkdir
fi
mkdir $wrkdir

# Put each Rust file into work directory, without test code, or comments.
for f in $srcdir/*.rs
do
	# echo $f
	count=`grep -c "mod\stests" $f`
	if [ $count -lt 1 ]
	then
	       	# echo $f no
		file=`(basename -- $f)`
		cat $f | sed -e 's/\/\/.*//' > $wrkdir/$file
	else
		linenum=`grep -n "mod tests" $f | sed -e 's/:.*//g'`
	       	# echo $f yes $linenum
		file=`(basename -- $f)`
		head -$linenum $f | sed 's/\/\/.*//' > $wrkdir/$file
	fi

done

# Parse out function names.
functions=`grep "\sfn\s" $wrkdir/*.rs | sed -e 's/.*fn\s//' | sed -e 's/<.*//' | sed -e 's/(.*//' | sort -u`

# Combine the work files.
cat $wrkdir/*.rs >  $wrkdir/tmp.txt

# Check the number of references for each function name.
for line in $functions
do
	line2=`echo $line | tr -d '\n'`
	count=`grep -c $line $wrkdir/tmp.txt`
	if [ $count -lt 2 ]
       	then
	    fileat=`grep -l $line2 $wrkdir/*.rs`
	    fileat2=`(basename -- $fileat)`
	    echo "-> $count " $line2 " " $fileat2

	fi
done
