#! /bin/sh

# Work out install dir
me="`readlink -f $0`"
APSIM=`dirname "$me"`

# Set up dynamic libraries
if [ -n "${LD_LIBRARY_PATH:-x}" ] ; then
  export LD_LIBRARY_PATH=$APSIM:$LD_LIBRARY_PATH
else
  export LD_LIBRARY_PATH=$APSIM
fi

for file in "$@" ; do
  if [ -d "$file" ] ; then
    # Try and run every file in that directory
    for f in $file/*.apsim $file/*.con $file/*.sim ; do
      $0 "$f"
    done
  else
    # Ensure output files are written in the same directory
    cd "`dirname \"$file\"`"
    "$APSIM/Apsim.exe" "`basename "$file"`"
  fi
done

