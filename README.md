# git-hook-puller
A service which is able to receive webhook request regarding releases from github and "git pull" the latest version

# Usage
create a config.txt file which contains
- the *port* to listen for webhook requests on
- a *secret key* for hashing in the second line
- in the following lines: *repository name* and *local path to cloned repo* (space seperated) 

## Example:
8080  
thisIsTheSecret  
repo1 /repos/repo1  
repo2 /repos/repo2  
