########################################
# This script is designed to update
# a git repo from the most recent
# svn revision.
# Author: Nicolas Strike, 2018
########################################


svnSrcDir=svnSourceExport
svnSrcCO=svnSourceCheckout
gitClone=gitClone

########################################
# Process command line arguments
# 
# TODO: handle missing/required args
########################################

    #forcedelete=0
branch=master
    while [ "$1" != "" ]; do
        case $1 in
            -g | --git-repo-url )   shift
                                    gitrepo=$1
                                    ;;
            -s | --svn-repo-url )   shift
                                    svnrepo=$1
                                    ;;
            -b | --branch )         shift
                                    branch=$1
                                    shift
                                    ;;
            #-f | --force-delete )   forcedelete=1
             #                       ;;
            -h | --help )           usage
                                    exit
                                    ;;
            * )                     usage
                                    exit 1
        esac
        shift
done


########################################
# Ensure the foldernames this script
# uses do not currently exist
########################################
deleteExistingClones(){
    echo "Ensuring there are no local clones from previous runs of this script."
    rm -rf svnSrcDir
    rm -rf svnSrcCO
    rm -rf gitClone
}


msg=msg.txt

##########################################
# Clone/Checkout repos into local folders
##########################################
cloneRepos(){
    echo "Checkingout/cloning repositories"
    echo "Updating git repo: $gitrepo"
    echo "from svn repo: $svnrepo"
    svn export $svnrepo $svnSrcDir --ignore-keywords
    wait
    #checkout a diff svn clone so we can get revision info (not included with the export keyword)
    svn co $svnrepo $svnSrcCO
    wait
    git clone $gitrepo $gitClone
    wait
}


########################################
# Delete large files before copying to
# prevent conflicts. Hopefully these
# files aren't needed. LFS is a pain.
########################################
#removeLFSfiles(){
    #TODO
#}

########################################
# Process command line arguments
########################################
copySvnToGitClone(){
    echo "Copying svn files into git clone"
    'cp' $svnSrcDir/. $gitClone/ -R
}

########################################
# Generate a semi-useful git commit msg
########################################
generateCommitMsg(){
    #Create Git Commit Message
    cd $gitClone
    printf "Updated from SVN, details below:\n" > ../$gitClone/$msg
    svn info >> ../$gitWRFClubbDir/$msg
    cd ..
}

########################################
# Commit changes to git
########################################
commitToGit(){
    echo "Pushing update to git"
    cd $gitClone
    git add -A
    git commit -a -F $msg
    git push --set-upstream origin $branch
    cd ..
}

########################################
# Delete the folders created by this script
########################################
deleteClones(){
    echo "Deleting local clones/checkouts"
    rm -rf $svnSrcDir
    rm -rf $svnSrcCO
    rm -rf $gitClone
}

########################################
# Run the script
########################################
#processArgs
deleteExistingClones
cloneRepos
#removeLFSfiles
copySvnToGitClone
generateCommitMsg
commitToGit
deleteClones
