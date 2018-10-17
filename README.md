This is intended to be a set of bash scripts that helps with:
1) The tranistion from an SVN+Trac development environment to a GitHub development environment
2) Maintaining the GitHub dev environment (dependancy/subtree managment)
3) Help with maintaining GitHub Wiki and adding features ontop of their implementation (scripts not yet added to repo)


These scripts were developed with a specific use case in mind, and then converted after the fact towards a more generalized use case (for you).

| Script | Use case | Documentation |
| --- | --- | --- |
| updateGitFork.sh | Updates your fork of another repo. It is most useful when ran frequently and automatically| |
| updateGitFromSvn.sh | Updates a git repo from an svn repo by doing a 'semi' intelligent copy/paste. History will not be preserved with this script as is. | |
| updateGitSubtree.sh | Updates a git subtree from an external repo/dependancy. It supports only including specific folders from the external repo in the subtree | |

* Note that this repo is new, and that not everything may work yet.


Please feel free to use and modify these scripts. If you make changes/improvements, send a pull request and I'll try to include them.

I'm not responsible if these scripts break your stuff, but they shouldn't.
