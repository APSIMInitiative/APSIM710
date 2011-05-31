#! /bin/sh

# Work out install dir
me="`readlink -f $0`"
APSIM=`dirname "$me"`

# Set up dynamic libraries
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$APSIM

for file in "$@" ; do
  if [ -d "$file" ] ; then
    # Try and run every file in that directory
    for f in $file/* ; do
      $0 "$f"
    done
  else
    # Ensure output files are written in the same directory
    cd "`dirname \"$file\"`"
    file=`basename "$file"`
    ext=`echo $file | awk -F . '{print $NF}'`
echo wd= `pwd` file = $file
    # Convert to sim; run apsim and collect summary file
    case "$ext" in
      apsim)
        "$APSIM/ApsimToSim.exe" "$file" | cut -b 9- | while read simfile
        do 
          if [ -f "$simfile" ] ; then
             "$APSIM/Apsim.x" "$simfile" > `basename "$simfile" .sim`.sum
             rm -f "$simfile"
          fi
        done
      ;;
      con)
        "$APSIM/ConToSim.x" "$file" | cut -b 9- | while read simfile
        do 
          if [ -f "$simfile" ] ; then
             "$APSIM/Apsim.x" "$simfile" > `basename "$simfile" .sim`.sum
             rm -f "$simfile"
          fi
        done
      ;;
      sim)
        "$APSIM/Apsim.x" "$file" > `basename "$file" .sim`.sum
      ;;
    esac
  fi
done

