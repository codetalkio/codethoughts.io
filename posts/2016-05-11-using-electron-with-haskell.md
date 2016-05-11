---
title: Using Electron with Haskell
tags: haskell, electron
---

_Versions used:_

* _Electron 1.0.1_
* _Stackage LTS 5.15_
* _servant 0.4.4.7_

_If you want to grab the whole code from this post, it can be found at [codetalkio/Haskell-Electron-app](https://github.com/codetalkio/Haskell-Electron-app)._

Not much literature exist on using `Electron` as a GUI tool for Haskell development, so I thought I'd explore the space a little. Being initially a bit clueless on how `Electron` would launch the Haskell web server, I was watching [the Electron meetup talk by Mike Craig from Wagon HG](https://youtu.be/mUAu7lcgYWE?t=6m54s) (they use `Electron` with Haskell) and noticed they actually mention it on the slides:

<blockquote>
    * Statically compiled Haskell executable
    * Shipped in Electron app bundle
    * main.js in Electron spawns the subprocess
    * Executable creates a localhost HTTP server
    * JS talks to Haskell over AJAX
</blockquote>

Importantly the bit _main.js in Electron spawns the subprocess_ is the part that was somehow missing in my mental model of how this would be structured (my JavaScript experience mainly lies in webdev and not really with Node.js and server-side/desktop JS).

Riding on this epiphany, I decided to document my exploration, seeing as this is an area that is sorely lacking (GUI programming in Haskell in general). Before we start anything though, let me lay out what the project structure will look like:

```
Haskell-Electron-app/
  haskell-app/
    resources/
      ...
    ... electron files
  backend/
    stack.yaml
    backend.cabal
    ... servant files
```

## Setting up Electron
`Electron` has a nice [quick start guide](http://electron.atom.io/docs/latest/tutorial/quick-start/), which helps you get going fairly, well, quick. For our purposes, the following will set up the initial app we will use throughout.

<div class="snippet-title">In your shell/terminal</div>
```bash
$ cd Haskell-Electron-app
$ git clone https://github.com/electron/electron-quick-start haskell-app
$ cd haskell-app
$ npm install && npm start
```

And that's it really. You've now got a basic `Electron` app running locally. The `npm start` command is what launches the app for you. But! Before doing anything more here, let's take a look at the Haskell side.


## Setting up the Haskell webserver
We'll be using [servant](http://haskell-servant.readthedocs.io/en/stable/) for a minimal application, but you could really use anything that will run a web server (such as [Yesod](http://www.yesodweb.com), [WAI](https://www.stackage.org/package/wai), [Snap](http://snapframework.com), [Happstack](http://www.happstack.com) etc, you get the idea :).

Assuming that `stack` is installed, you can set up a new `servant` project with

<div class="snippet-title">In your shell/terminal</div>
```bash
$ cd Haskell-Electron-app
$ stack new backend servant
$ cd backend
$ stack build
```

which will download the `servant` project template for you (from the [stack templates repo](https://github.com/commercialhaskell/stack-templates)) and build it.

To test that it works run `stack exec backend-exe` which will start the executable that `stack build` produced. You now have a web server running at `127.0.0.1:8080` - try and navigate to  [127.0.0.1:8080/users](http://127.0.0.1:8080/users) and check it out! :)

For the lack of a better named I have called the application _backend_, but it could really be anything you fancy.


## Contacting Servant/Haskell from Electron
For now, let us proceed with `Electron` and `servant` running separately, and later on explore how we can start the `servant` server from inside `Electron`.

Since the `servant` template project has given us the endpoint `127.0.0.1:8080/users` from which it serves `JSON`, let's set up `Electron` to call that and display the results.

By default the chromium developer tools are enabled in `Electron`. I suggest you keep them enabled while debugging, since that makes it a lot easier to see if anything went wrong. If you want to disable it, you just need to comment/remove a line in `Haskell-Electron-app/haskell-app/main.js`:

<div class="snippet-title">Haskell-Electron-app/haskell-app/main.js</div>
```javascript
...
function createWindow () {
  // Create the browser window.
  mainWindow = new BrowserWindow({width: 800, height: 600})

  // and load the index.html of the app.
  mainWindow.loadURL('file://' + __dirname + '/index.html')

  // Open the DevTools.
  // mainWindow.webContents.openDevTools() <-- this one here
  ...
}
...
```

Short interlude: since I'm a bit lazy let's go ahead and download [jQuery 2.2.3 minified](https://code.jquery.com/jquery-2.2.3.min.js) and put that into `Haskell-Electron-app/haskell-app/resources/jQuery-2.2.3.min.js` so we can include it later on and get the nice AJAX functionality it provides.

Back to work, lets change the `index.html` page and prepare it for our list of users.

<div class="snippet-title">Haskell-Electron-app/haskell-app/index.html</div>
```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Heya Servant!</title>
  </head>
  <body>
    <h1>User list:</h1>
    <div id="status"><!-- The request status --></div>
    <div id="userList">
      <!-- We'll fill this with the user information -->
    </div>
  </body>
  <script>
    // Avoid clashing Node.js/Electron with jQuery as per
    // http://electron.atom.io/docs/v0.37.8/faq/electron-faq/
    window.nodeRequire = require;
    delete window.require;
    delete window.exports;
    delete window.module;
    // Fetch jQuery
    window.$ = window.jQuery = nodeRequire('./resources/jQuery-2.2.3.min.js')
    // The JS file that will do the heavy lifting
    nodeRequire('./renderer.js')
  </script>
</html>
```

And finally we'll implement the logic in `renderer.js`.

<div class="snippet-title">Haskell-Electron-app/haskell-app/renderer.js</div>
```javascript
// Backend and endpoint details
const host     = 'http://127.0.0.1:8080'
const endpoint = '/users'
// Retry configuration
let maxNoOfAttempts        = 50,
    waitTimeBetweenAttempt = 250

let _fetchUserList = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpoint, function(users) {
    $('#status').html(`Fetched the content after attemt no.
                       ${currentAttemptNo}!`)
    // Construct the user list HTML output
    let output = "";
    for (let i in users) {
      let user = users[i]
      output += `ID: ${user.userId},
                 Firstname: ${user.userFirstName},
                 Lastname: ${user.userLastName}
                 <br>`
    }
    $('#userList').html(output)
  }).fail(function() {
    $('#status').html(`Attempt no. <b>${currentAttemptNo}</b>. Are you sure the
                       server is running on <b>${host}</b>, and the endpoint
                       <b>${endpoint}</b> is correct?`)
    // Keep trying until we get an answer or reach the maximum number of retries
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchUserList(waitTime, maxAttempts, currentAttemptNo+1)
      }, waitTime)
    }
  })
}

// Convenience function for _fetchUserList
let fetchUserList = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

// Start trying to fetch the user list
fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts)
```

We simply request the `JSON` data at `http://127.0.0.1:8080/users`, with `$.getJSON(...)`, and display it if we received the data. If the request failed, we keep retrying until we either get a response or reach the maximum number of attempts (here set to 50 via `maxNoOfAttempts`).

The real purpose behind the retry will become apparent later on, when we might need to wait for the server to become available. Normally you will use a status endpoint that you are 100% sure is correct and not failing to check for the availability (inspired by the answer [Mike from Wagon HQ gave here](https://www.reddit.com/r/haskell/comments/4ipah2/resources_for_electron_haskell/d30mupm)).


## Launching the Haskell web server from Electron
Now to the interesting part, let's try to launch the Haskell web server from inside of `Electron`.

First though, let us set the `haskell-app/resources` folder as the target for our binary, in the `stack.yaml` file, with the `local-bin-path` configuration value.

<div class="snippet-title">Haskell-Electron-app/backend/stack.yaml</div>
```yaml
resolver: lts-5.15
local-bin-path: ../haskell-app/resources
...
```

Now let's compile the executable.

<div class="snippet-title">In your shell/terminal</div>
```bash
$ cd Haskell-Electron-app/backend
$ stack build --copy-bins
```

The `--copy-bins` (or alternatively you can just do `stack install`) will copy over the binary to `Haskell-Electron-app/haskell-app/resources` as we specified (it defaults to `~/.local/bin` if `local-bin-path` is not set).

After that, it is surprisingly easy to launch the executable from within `Electron` (well, easy once you already know how). We will change `main.js` to spawn a process for the web server upon app initialization (i.e. the `ready` state). Since there are bits and pieces that are added I'll include the whole file, with most of the comments removed.

<div class="snippet-title">Haskell-Electron-app/haskell-app/main.js</div>
```javascript
const electron = require('electron')
// Used to spawn processes
const child_process = require('child_process')
const app = electron.app
const BrowserWindow = electron.BrowserWindow

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
// Do the same for the backend web server
let backendServer

function createWindow () {
  mainWindow = new BrowserWindow({width: 800, height: 600})
  mainWindow.loadURL('file://' + __dirname + '/index.html')
  mainWindow.webContents.openDevTools()
  mainWindow.on('closed', function () {
    mainWindow = null
  })
}

function createBackendServer () {
  backendServer = child_process.spawn('./resources/backend-exe')
}

app.on('ready', createWindow)

// Start the backend web server when Electron has finished initializing
app.on('ready', createBackendServer)

// Close the server when the application is shut down
app.on('will-quit', function() {
  backendServer.kill()
})

app.on('window-all-closed', function () {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function () {
  if (mainWindow === null) {
    createWindow()
  }
})
```

We are using the [child_process.spawn](https://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options) command to launch our backend web server. Let's briefly go through what is happening:

* Imported the `child_process` module with `const child_process = require('child_process')`
* Defined a variable `let backendServer` that'll let us keep the backend server from being garbage collected
* Added a function `createBackendServer` that runs `child_process.spawn('./resources/backend-exe')` to spawn the process
* Added the `createBackendServer` function to the `ready` hook with `app.on('ready', createBackendServer)`
* * Close the `backendServer` when the event `will-quit` occurs

__And voila!__ We now have Electron spawning a process that runs a Haskell web server! :)

Next step would be to package the app up for distribution to see if that affects anything, but I'll save that for another time (and `Electron` already has [a page on distribution here](http://electron.atom.io/docs/v0.37.8/tutorial/application-distribution/)).
