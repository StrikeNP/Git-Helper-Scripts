#!/usr/bin/env bash
##################################################################
# This script is intended to update our release version of
# clubb (the one "publicly" visible). It should be ran nightly.
#
# Input options:
#    * source repo: URL for the original repo (the repo yours was forked from)
#    * destination repo: URL for your repo
#    * source branch: branch you want to pull updates from (defaults to master)
#    * destination branch: branch you want to push updates to (defaults to master)
#    * force update: adds the '-f' flag to the push command, thus force pushes
#
# Author: Nicolas Strike, 2018
##################################################################
workdir=temprepo
srcbranch=master
destbranch=master
forceupdate=0
    while [ "$1" != "" ]; do
        case $1 in
            -s | --source-repo )    shift
                                    srcrepo=$1
                                    ;;
            -d | --destination-repo )     shift
                                    destrepo=$1
                                    ;;
            --source-branch )       shift
                                    srcbranch=$1
                                    ;;
            --dest-branch )         shift
                                    destbranch=$1
                                    ;;
            -f | --force-update )   forceupdate=1
                                    ;;
            -h | --help )           usage
                                    exit
                                    ;;
        esac
        shift
done


rm -rf $workdir
git clone $srcrepo $workdir
cd $workdir
git remote add destination_repo $destrepo

if [ $forceupdate -eq 1 ]
then
      echo "Force pushing updates from $srcrepo:$srcbranch to $destrepo:$destbranch"
      git push -f -u destination_repo $destbranch
else
      echo "Pushing updates from $srcrepo:$srcbranch to $destrepo:$destbranch"
      git push -u destination_repo $destbranch
fi

wait
cd ..
rm -rf $workdir
