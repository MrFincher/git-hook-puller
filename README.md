# git-hook-puller
A service which is able to receive webhook request regarding events like releases or pushs from github and "git pull" the latest version to the local directory where the repository was cloned

# Usage
### setup server
create a `config.yaml` file in the same folder as the executable and specify:
- the *port* to listen for webhook requests on as `port`
- a *secret key* for hashing in the second line as `secret`
- the `repos` for each repository:
  - the *repository name*
  - the *local path to cloned repo* as `path`
  - and the *event type* (push or release) as `event`.
  - optionally, the command to be executed after updating the repository as `setupCmd`

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
Create a webhook in the settings of the repository you want the be automatically updated:
- Provide the URL to your server running the git-hook-puller (including the port)
- Enter the choosen secret
- Select "application/json" as content type
- Choose either *release* or *push* events. This needs to be the same as the event type specified in the config file.

# Planned Features
- https support
