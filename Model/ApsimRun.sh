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
    file=`basename "$file"`
    ext=`echo $file | awk -F . '{print $NF}'`

    # Convert to sim; run apsim and collect summary file
    case "$ext" in
      apsim)
        simfiles=`basename "$file" .apsim`.simfiles 
        "$APSIM/ApsimToSim.exe" "$file" 2> "$simfiles"
        cut -b 9- "$simfiles"  | while read simfile
        do 
          if [ -f "$simfile" ] ; then
             "$APSIM/Apsim.x" "$simfile" > `basename "$simfile" .sim`.sum
             rm -f "$simfile"
          fi
        done
        rm -f "$simfiles"
      ;;
      con)
        simfiles=`basename "$file" .apsim`.simfiles 
        "$APSIM/ConToSim.x" "$file" 2> "$simfiles"
        cut -b 9- "$simfiles" | while read simfile
        do 
          if [ -f "$simfile" ] ; then
             "$APSIM/Apsim.x" "$simfile" > `basename "$simfile" .sim`.sum
             rm -f "$simfile"
          fi
        done
        rm -f "$simfiles"
      ;;
      sim)
        "$APSIM/Apsim.x" "$file" > `basename "$file" .sim`.sum
      ;;
    esac
  fi
done

