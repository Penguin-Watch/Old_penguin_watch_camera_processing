# pwatch
PenguinWatch camera processing/modeling.

**General info:**

- To modify, checkout a branch from the master. Please do not work on the master branch.

- Push all commits to GitHub after each session and be sure to pull from GitHub before beginning each session.

- Be sure you DO NOT commit files larger than 100MB. This will break the repository. We can set up git lfs if this needs to be done. Add large files (or folders that contain large files) to the '.gitignore' file to avoid adding these large files to the repository.

- Although 'Data' (click data) and 'Images' (camera images) folders are not tracked by git, these folders should exist in your Rstudio project directory


# Notes about project workflow
1) First clone the repository onto your local machine (this will only need to be done once).


2) Whenever you want to make changes to the code, you should 'checkout a branch'. By this I mean create a new branch off of the master. 

To create a new branch, open up the 'Shell' from the 'Tools' menu. Enter `git checkout -b BRANCH_NAME_HERE` into the command line. This will create a new branch and then switch to this branch. Make sure you see your branch name in the drop down menu in the 'Git' window of RStudio.


3) Now you can change code, add files etc. in this new branch. Commit your changes either from the command line with `git add FILE_HERE` `git commit -m 'MESSAGE_HERE` or with the 'Commit' button in the git window of RStudio. All changes made in this branch will not affect the master.


4) Once you are done working on the branch push that branch to GitHub. You can come back to it at a later time. To push the branch to GitHub go back to the shell and enter `git push -u origin BRANCH_NAME_HERE`. This branch will now be on GitHub. NOTE: the 'Push' button in RStudio can be used to push commits to GitHub, but only after the **initial** push is made through the shell.


5) To integrate any changes from the master branch into a development branch, you can use `git checkout BRANCH_NAME_HERE` (switches to development branch), `git fetch origin` (grabs new material from repo), `git merge origin/master` (merges changes from master branch into development branch), `:wq` (confirms the default commit). NOTE: you can look at what will be integrated into the development branch from the master branch between the `fetch` and `merge` steps by looking at the log/differences.


6) If you're done working on the branch permanently (maybe you've fixed a bug or added a feature) and you want to integrate those changes into the master branch, submit a 'pull request'. Go to the pwatch GitHub site, find your branch, and click 'pull request'. Make a note about the changes you've made.

Once a pull request is made leave your thoughts on the proposed changes. I'll worry about merging the changes into the master branch to make sure nothing breaks.

**THAT'S IT!**
