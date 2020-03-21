---
title: Local notifications from irssi on a remote server
tags: irssi
versions:
- OS X 10.9 Mavericks (should work on all, unless they change launchtl plists)
- irssi v0.8.15
- autossh v1.4c
- terminal-notifier v1.6.1
---

So, if you're like me and like to have your IRC client (in this instance [irssi](http://www.irssi.org)) running on a server in a `tmux` or `screen` session to never miss out on the conversation, you might feel like you're missing some of the benefits of running a local IRC client.

In particular, I was missing local notifications when someone would highlight my nickname. I could of course use a bouncer, but hey! it's no fun running `irssi` locally and having to close it for a reboot just as you've gotten it precisely the way you like it 🙂...

So, how does one solve this problem?

<div></div><!--more-->


## The setup
The final setup will look something like this,

```bash
autossh -> irssi + fnotify -> terminal-notifier
```

You can replace `terminal-notifier` with whatever notification program will work on your system. The rest is pretty much OS agnostic, except for the automatic startup of the script.


## What you need
First off, I'm assuming you're running the [fnotify](http://www.leemhuis.info/files/fnotify/fnotify) plugin in irssi, and that you have setup private keys between you and the server. These are the only things you need to do server-side.

On your local setup you'll need,
* [autossh](http://www.harding.motd.ca/autossh/)
  * `sudo port install autossh`
* [terminal-notifier](https://github.com/alloy/terminal-notifier)
  * `sudo port install terminal-notifier` or
  * `sudo gem install terminal-notifier`

We use `autossh` to keep our ssh connection from dropping, and killing the script (so you don't have to constantly restart it whenever you close the lid on your laptop).

To display the notifications, we use `terminal-notifier`, which creates native notifications on OS X. You can change this out with whatever program your OS needs.


## Get it going
So, to the fun part. First off, create a file called something like `irssi_notifier.sh` in a location you don't mind it being in forever, but can still remember (at least until the end of this article).

In this script, you'll need the following code. I'll explain what it does below (__replace the two user@server instances with your details!__),

```bash
#!/bin/bash
irssi_notifier() {
  (ssh user@server -o PermitLocalCommand=no \
    ": > .irssi/fnotify ; tail -f .irssi/fnotify " |  \
  while read heading message; do                      \
    url=`echo "${message}" | grep -Eo 'https?://[^ >]+' | head -1`; \
    if [ ! "$url" ]; then terminal-notifier -sender com.apple.Terminal -message "${message}" -title "${heading}" -activate com.apple.Terminal; \
    else terminal-notifier -sender com.apple.Terminal -message "${message}" -title "${heading}" -open "${url}"; \
    fi; \
  done)
}
# Make sure we don't make a new autossh connection
if ! ps aux | grep -q 'autoss[h]'; then
     /opt/local/bin/autossh -M 0 -f -N -p 22 -g -c 3des -D 1080 user@server;
fi;
# Avoid relaunching the notifier function if it's already running
if ! ps aux | grep -q 'tail -f .irssi/fnotif[y]'; then
     irssi_notifier
fi;
```


## So, what happens here?
* Our she-bang line (the line with #) makes sure the script gets run as a bash script without the need to specify bash on launch.
* We create a function for the notification part of the script
  * Inside the `irssi_notifier` function, we connect to the server with `irssi` on, and read via tail from the fnotify file.
  * When somethings comes in, we divide it into a heading and a message, and check for an URL in the message part.
  * If no URL is found, we make construct the notification via `terminal-notifier` to just activate our terminal upon click on the notification.
  * If there is a URL, we open the URL in a browser upon click.
* Just after the function definition, we create a connection to the server via and monitor it with `autossh`.
* We call the previously defined `irssi_notifier` function, and start the notification service.

In the last two calls, we check if the processes exist, and only call them if they don't. This avoids spamming a zillion `autossh` connections etc.

### Testing
If you just want to test it, you can just run the script (remember to chmod +x it) without the `autossh` part (just comment it out). If you use `autossh`, you'll need to kill the process if you relaunch the script, else it'll keep spawning more and more `autossh` processes.


## Launch on startup
_NOTE: This is specific for OS X, but I'm sure there are ways for other OSs_

If you're lazy like me, you probably aren't satisfied quite yet. There is still the part about starting the script yourself whenever you boot up your system.

On OS X, this is quite simple (once you know how to do it, that is). We will use [launchctl](https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/launchctl.1.html), which manages programs that launch upon startup.


To launch the script, you need to create a file called `com.irssi.notifier.plist` in `~/Library/LaunchAgents/`. The contents will be,


```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>com.irssi.notifier</string>
  <key>ProgramArguments</key>
  <array>
    <string>/absolute/path/to/irssi_notifier.sh</string>
  </array>
  <key>KeepAlive</key>
  <true/>
</dict>
</plist>
```


The only thing you need to change is under `ProgramArguments`, where you substitute `/absolute/path/to/irssi_notifier.sh` with the actual path to the script file.

Finally, you need to do `launchctl load ~/Library/LaunchAgents/com.irssi.notifier.plist`.

## Conclusion
After this, you should be good to go! Now you can enjoy getting IRC highlights whenever you have a working connection.

Try and reboot, and check if it works :)

__Q: Why not use mosh instead of autossh?__
The reason I didn't use `mosh` to do what I accomplished with `autossh`, is because of running into trouble executing commands directly after a connection initiation (`mosh user@server 'echo 1'` fails instantly, to give an example).

That said, I do recommend using mosh instead of ssh normally, since it works excellent :).
