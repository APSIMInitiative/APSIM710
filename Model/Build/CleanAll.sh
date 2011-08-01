#! /bin/sh

#rem -------------------------------------------------------------
#rem Remove all unwanted files.
#rem -------------------------------------------------------------
find  .. -name *.o -exec rm {} \;
find  .. -name *.mod -exec rm {} \;
for type in so dll exe mdb config ; do rm -f ../*.$type ; done

(cd ../TclLink && ../Build/RunMake.sh clean)


