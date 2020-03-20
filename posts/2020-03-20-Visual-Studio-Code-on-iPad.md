---
title: Visual Studio Code on iPad
tags: vscode, ipad, dx, digital nomad
---

*Versions and Tools used:*

- *`code-server` Version 3.0.0*
- your own server
- your own domain

With [Apple increasing their focus](https://www.apple.com/newsroom/2020/03/apple-unveils-new-ipad-pro-with-lidar-scanner-and-trackpad-support-in-ipados/) on making the iPad a viable device for work, it is time to revisit using my iPad as a workstation for programming.

I rely heavily on command-line tools and language specific tools (rust-analyser, node, ghcide, etc) for my day-to-day programming, and my current setup features:

- Blink with mosh to a remote server
- Neovim and [wemux]() on the remote server
- iSH to play around with very simple CLI needs locally on the iPad

On my computer I use Visual Studio Code, and its long been a wish to get that running somehow on my iPad. This is an attempt to make VS Code available on the iPad, under the restrictions that we have to deal with.

<div class="callout">
  <div class="callout-bulb">💡</div>
  This setup unfortunately doesn't eliminate the need for a server yet! We'll have to dream of that day to come.
</div>

## Enter code-server
[code-server](https://github.com/cdr/code-server) enables you to run VS Code on a remote server, and access it through a browser. While not ideal, this is at least one way to get VS Code onto an iPad.

I'll assume you have a server running somewhere, that you intend to use for this setup.

First, SSH into your server, so that we can setup `code-server`. We are going to download the latest release from GitHub, and set it up. Checkout the latest release at [https://github.com/cdr/code-server/releases](https://github.com/cdr/code-server/releases), and pick the asset for `linux_x86_64`,

```bash
$ ssh user@example.com
$ wget https://github.com/cdr/code-server/releases/download/3.0.0/code-server-3.0.0-linux-x86_64.tar.gz
tar.gz
...
code-server-3.0.0-linux-x86_6 100%[==============================================>]  64.31M  3.47MB/s    in 9.4s
$ tar -zxvf code-server-3.0.0-linux-x86_64.tar.gz
```

You should now have a folder called `code-server-3.0.0-linux-x86_64`. Let's rename it and make the put the executable on our `PATH`,

```bash
$ mv code-server-3.0.0-linux-x86_64 .code-server
$ ln -s "$HOME/.code-server" /usr/local/bin/code-server
```

Try firing it up,

```bash
$ code-server
info  code-server 3.0.0
info  Server listening on http://127.0.0.1:8080
info    - Password is xxxxxxxxxxxxxxxxxxxxxx
info      - To use your own password, set the PASSWORD environment variable
info      - To disable use `--auth none`
info    - Not serving HTTPS
info    - Automatic updates are enabled
```

Neat! 🙂

## Securing the setup for remote access
So far `code-server` is only listening for local connections, but we'd like to be able to use it on the go, from a browser on the iPad. This means we have to do a little extra work to secure our setup.

`code-server` covers how to do this [in their FAQ](https://github.com/cdr/code-server/blob/master/doc/FAQ.md#how-should-i-expose-code-server-to-the-internet), but skips the specific steps. Unfortunately, due to an issue with self-signed certificates on iOS, we cannot simply use these (see [code-serfer#1122](https://github.com/cdr/code-server/issues/1122)). Instead, we will opt for [letsencrypt](https://letsencrypt.org)!

<!--
We'll set up a self-signed certificate. For the pass phrase, simply press enter to put a blank password on the key.

```bash
$ mkdir .code-server-meta
$ cd .code-server-meta
$ openssl req -x509 -nodes -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -subj "/C=NA/ST=None/L=Global/O=Company Name/OU=Org/CN=localhost"
Generating a RSA private key
...
```

Here we made a directory to hold our keys for `code-server`, and generated them using `openssl`. You can adjust the days to a number you are comfortable with, here I just went with 365, meaning I'll have to renew the certificate in a year.
-->

First we will install certbot, which will manage the certificate renewal on our server,

```bash
$ sudo apt-get install software-properties-common # for add-apt-repository
$ sudo add-apt-repository ppa:certbot/certbot
$ sudo apt install python-certbot-nginx
```

Because these certificates are managed under `certbot`, we'll need to setup a script that will move the certificates to a location we want, so that our `code-server` does not need root permissions. We'll do this with a [deploy-hook](https://certbot.eff.org/docs/using.html#renewing-certificates), which runs after each successful renewal.

Let's make a directory for the certificates. For convenience we will also export our domain name as an environment variables, to be used throughout the rest of the post (change `vscode.example.com` to your own domain),

```bash
$ mkdir .code-server-meta
$ cd .code-server-meta
$ export DOMAIN=vscode.example.com
```

Let's set up the renewal script in `/etc/letsencrypt/renewal-hooks/deploy/renewal.sh`,

```bash
$ echo 'echo '\''#!/bin/bash
echo "Letsencrypt renewal hook running..."
echo "RENEWED_DOMAINS=$RENEWED_DOMAINS"
echo "RENEWED_LINEAGE=$RENEWED_LINEAGE"

if [[ $RENEWED_LINEAGE == *"'$DOMAIN'"* ]]; then
  cat $RENEWED_LINEAGE/privkey.pem > '$HOME'/.code-server-meta/key.pem
  cat $RENEWED_LINEAGE/fullchain.pem > '$HOME'/.code-server-meta/cert.pem
  systemctl restart code-server
  echo "code-server cert and key updated and restarted"
fi'\'' > /etc/letsencrypt/renewal-hooks/deploy/renewal.sh' | sudo bash && sudo chmod +x /etc/letsencrypt/renewal-hooks/deploy/renewal.sh
```

We'll now set up our certificates by starting `certbot`. During this you will be asked for your email, and to agree to the terms of service.

```bash
$ sudo certbot certonly --standalone --preferred-challenges http -d $DOMAIN
```

This will create the certificates in `/etc/letsencrypt/live/$DOMAIN`. Check that everything works by doing a dry-run of the certificate renewal,

```bash
$ sudo certbot renew --dry-run
```

Excellent. You are now ready to launch your `code-server` instance using the keys for HTTPS,

```bash
$ code-server --cert ~/.code-server-meta/cert.pem --cert-key ~/.code-server-meta/key.pem --host 0.0.0.0
info  code-server 3.0.0
info  Server listening on http://127.0.0.1:8080
info    - Password is xxxxxxxxxxxxxxxxxxxxxx
info      - To use your own password, set the PASSWORD environment variable
info      - To disable use `--auth none`
info    - Automatic updates are enabled
```

A login screen should appear. Use the password that the server printed, and you are in! 🥳

<div class="clear two-images">
  <a href="/resources/images/visual-studio-on-ipad-welcome.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/visual-studio-on-ipad-welcome.thumbnail.png" loading="lazy" alt="Welcome screen in Visual Studio Code on iPad" title="Welcome screen in Visual Studio Code on iPad" /></a>
  <a href="/resources/images/visual-studio-on-ipad-code-file.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/visual-studio-on-ipad-code-file.thumbnail.png" loading="lazy" alt="Code file in Visual Studio Code on iPad" title="Code file in Visual Studio Code on iPad" /></a>
</div>


## Daemonizing the server
Currently we need to manually start the server every time we reboot our server. Instead of this, we'd like the `code-server` to be managed as a system service.

We'll do this by:

- Start `code-server` with a fixed password
- Setting up a script to start `code-server` in a `screen` instance
- Letting `systemd` manage the start/stop of the service

**Passphrase**

First let us set up our password for the `code-server`, so that we can login across reboots. We'll do this by dropping a simple plaintext file inside `$HOME/.code-server-meta`.

This is under the assumption that you are the only one with access to the server. It is recommended that you put a unique passphrase for this service.

```bash
$ echo "MySecretPassword" > $HOME/.code-server-meta/passphrase.txt
```

**Manage code-server in screen**

We are going to put our script to manage the `code-server` instance in `$HOME/.code-server-meta/service.sh`.

```bash
$ echo '#!/bin/bash

case "$1" in
  start)
    # Create a screen in detached mode, called "code-server"
    screen -dmS code-server bash -c '\''PASSWORD=$(cat $HOME/.code-server-meta/passphrase.txt) code-server --cert $HOME/.code-server-meta/cert.pem --cert-key ~/.code-server-meta/key.pem --host 0.0.0.0'\''
    echo "Service started."
    ;;
  status)
    result=$(screen -list | grep code-server)
    if [ $? == 0 ]; then
      echo "code-server service is ON."
    else
      echo "code-server service is OFF."
    fi
    ;;
  stop)
    # Quit the "code-server" screen
    screen -S code-server -X quit
    echo "Service stopped."
    ;;
  *)
    echo "Unknown command: $1"
    exit 1
  ;;
esac
' > $HOME/.code-server-meta/service.sh && chmod +x $HOME/.code-server-meta/service.sh
```

**Systemd service**

Finally, we are gonna put our `systemd` service in `/etc/systemd/system/code-server.service`.

```bash
$ echo "echo '[Unit]
Description=Service to run code-server
After=network.target

[Service]
Type=oneshot
User=$(whoami)
ExecStart=$HOME/.code-server-meta/service.sh start
ExecStop=$HOME/.code-server-meta/service.sh stop
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
' > /etc/systemd/system/code-server.service" | sudo bash
```

Enable the script and start the service,

```bash
$ systemctl enable code-server
$ systemctl start code-server
```

Navigate to your domain on port `8080`. Congratulations, you've now got a solid setup for editing code in your iPad browser 🎉
