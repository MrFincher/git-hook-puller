# git-hook-puller
A service which is able to receive webhook request regarding releases from github and "git pull" the latest version to the local directory where the repository was cloned

# Usage
### setup server
create a `config.yaml` file in the same folder as the executable which contains:
- the *port* to listen for webhook requests on in the first line as `port`
- a *secret key* for hashing in the second line as `secret`
- in the following lines: `repos`, followed by the *repository name* along with the *local path to cloned repo* as `path` and the *event type* (push or release) as `event`.
- optionally, the command to be executed after updating the repository as `setupCmd`.

##### Example:
```
port: 8080
secret: thisIsTheSecret
repos:
  repo1:
    event: push
    path: "/repos/repo1"
  repo2:
    event: release
    path: "/repos/repo2"
    setupCmd: "systemctl restart service2"
```

### setup github webhook
Create a webhook for releases in the settings of the repository you want the be automatically updated.
Provide the URL to your server running the git-hook-puller (including the port) and the choosen secret. Select "application/json" as content type.

# Planned Features
- https support
- specify setup command to be run after pull (e.g. compiling)


