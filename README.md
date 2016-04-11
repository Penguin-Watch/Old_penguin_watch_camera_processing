# pwatch
PenguinWatch camera processing/modeling

To modify, checkout a branch from the master. Do not work on the master branch.

Also, DO NOT COMMIT files greater than 100MB in size. These will need to be added through git lfs.

# Notes about project workflow
1) First clone the repository onto your local machine

2) Whenever you want to make changes to the code, you should 'checkout a branch'. By this I mean create a new branch off of the master. Please to do not work on the master branch.

To create a new branch, open up the 'Shell' from the 'Tools' menu. Enter `git checkout -b BRANCH_NAME_HERE` into the command line. This will create a new branch and then switch to this branch. Make sure you see your branch name in the drop down menu in the git window of Rstudio.

3) Now you can change code, add files etc. in this new branch. Commit your changes either from the command line or with the commit button in the git window of Rstudio.

All changes made in this branch will not affect the master.

4a) Once you're done working on the branch for the time being you might wnat to push that branch to github to come back to it at a later time. To push the branch to github go back to the shell and enter `git push origin BRANCH_NAME_HERE`.

This branch will now be on github.

4b) If you're done working on the branch permanently (maybe you've fixed a bug or added a feature) and you want to integrate thosechanges into the master branch, submit a pull request.

