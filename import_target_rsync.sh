#!/bin/sh

LDRIVE=/run/user/1000/gvfs/smb-share:server=caslab.ad.ilstu.edu,share=bio/Comte/alain/RivFishTimeBiodiversityFacets/_targets/
MYDIR=$HOME/Documents/post-these/isu/RivFishTimeBiodiversityFacets/_targets

#rsync -av $LDRIVE $MYDIR
# DRYRUN:
rsync -anv $LDRIVE $MYDIR
