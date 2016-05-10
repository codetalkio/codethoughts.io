---
title: Irssi notifications on iOS and Android
tags: irssi
---

Or really anywhere that [https://pushover.net](https://pushover.net) supports.


In light of the [earlier article](/posts/2014-09-24-local-notifications-from-irssi-on-a-remote-server.html), I thought I'd might as well supercharge my IRC setup. So, now we're gonna get some notifications for our mobile devices via pushover.

## Setting up pushover
There really isn't much to do here. First, go create an account and download the app on the device you want notifications on.

Then, register a pushover application. You can do this from the dashboard, or
directly from [https://pushover.net/apps/build](https://pushover.net/apps/build). It will look something like this,

<img src="/images/pushover-config.png" alt="Pushover Application Registration" title="Pushover Application Registration" width="100%">


## Setting up the script

So, like the article mentioned before, I'm assuming you're using [irssi](http://www.irssi.org) and the [fnotify](http://www.leemhuis.info/files/fnotify/fnotify) plugin.

Create a file called something like `irssi_pushover.sh` with the contents below,

<?prettify?>
```
#!/bin/bash

# Replace `YOUR_APP_TOKEN` and `YOUR_USER_TOKEN` with your values from pushover
tail -f /home/user/.irssi/fnotify | while read heading message; do
    url=`echo "${message}" | grep -Eo 'https?://[^ &gt;]+' | head -1`;
    if [ ! "$url" ]; then
        curl -s \
            --form-string "token=YOUR_APP_TOKEN" \
            --form-string "user=YOUR_USER_TOKEN" \
            --form-string "message=${message}" \
            --form-string "title=${heading}" \
            https://api.pushover.net/1/messages.json
    else
        curl -s \
            --form-string "token=YOUR_APP_TOKEN" \
            --form-string "user=YOUR_USER_TOKEN" \
            --form-string "message=${message}" \
            --form-string "title=${heading}" \
            --form-string "url=${url}" \
            https://api.pushover.net/1/messages.json
    fi;
done;
```

This script will read from `/home/user/.irssi/fnotify` and send a `POST` request
with the message and title via `curl` using the [pushover API](https://pushover.net/api).

You need to change the destination to the correct location of your fnotify
file, and of course also replace `YOUR_APP_TOKEN` and `YOUR_USER_TOKEN` with your values from pushover (the user token is on the dashboard and the app token is under the app).


## Conclusion

Now you've got notifications on your phone too! :) Never miss a moment of IRC again. A tip: you can make pushover open links directly to the browser under settings in the app.
