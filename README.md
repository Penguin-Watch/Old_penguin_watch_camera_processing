# pwatch
PenguinWatch camera processing/modeling.

To modify, checkout a branch from the master. Please do not work on the master branch.

Be sure you DO NOT commit files larger than 100MB. This will break the repository. We can set up git lfs if this need to be done.


# Notes about project workflow
1) First clone the repository onto your local machine (this will only need to be done once)


2) Whenever you want to make changes to the code, you should 'checkout a branch'. By this I mean create a new branch off of the master. 

To create a new branch, open up the 'Shell' from the 'Tools' menu. Enter `git checkout -b BRANCH_NAME_HERE` into the command line. This will create a new branch and then switch to this branch. Make sure you see your branch name in the drop down menu in the git window of RStudio.


3) Now you can change code, add files etc. in this new branch. Commit your changes either from the command line with `git add FILE_HERE` `git commit -m 'MESSAGE_HERE` or with the commit button in the git window of RStudio. All changes made in this branch will not affect the master.


4) Once you are done working on the branch push that branch to github. You can come back to it at a later time. To push the branch to github go back to the shell and enter `git push origin BRANCH_NAME_HERE`. This branch will now be on github.


5) If you're done working on the branch permanently (maybe you've fixed a bug or added a feature) and you want to integrate thosechanges into the master branch, submit a pull request. Go to the pwatch github site, find your branch, and click 'pull request'. Make a note about the changes you've made

