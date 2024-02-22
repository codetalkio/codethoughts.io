---
title: Visual Studio Code on iPad
tags: vscode, ipad, digital nomad
date: 2020-03-20
versions:
- '`code-server` version 3.4.1'
- 'Ubuntu 18.04 (should fit with most distros)'
- 'Your own server'
- 'Your own domain'
---

With Apple increasing their focus^[[https://www.apple.com/newsroom/2020/03/apple-unveils-new-ipad-pro-with-lidar-scanner-and-trackpad-support-in-ipados/](https://www.apple.com/newsroom/2020/03/apple-unveils-new-ipad-pro-with-lidar-scanner-and-trackpad-support-in-ipados/){target="_blank" rel="noopener noreferrer"}.] on making the iPad a viable device for work, it is time to revisit using my iPad as a workstation for programming.

I rely heavily on command-line tools and language-specific tools (rust-analyser, node, ghcide, etc.) for my day-to-day programming, and my current setup features:

- [Blink](https://blink.sh){target="_blank" rel="noopener noreferrer"} with mosh to a remote server.
- Neovim and [wemux](https://github.com/zolrath/wemux){target="_blank" rel="noopener noreferrer"} on the remote server.
- [iSH](https://ish.app){target="_blank" rel="noopener noreferrer"} to play around with very simple CLI needs locally on the iPad.

On my computer, I use Visual Studio Code, and it's long been a wish to get that running somehow on my iPad. This is an attempt to make VS Code available on the iPad under the restrictions that we have to deal with.

<div></div><!--more-->

<div class="callout">
  <div class="callout-bulb">💡</div>
  This setup unfortunately doesn't eliminate the need for a server yet! We'll have to dream of that day to come.
</div>

## Enter code-server
[code-server](https://github.com/cdr/code-server){target="_blank" rel="noopener noreferrer"} enables you to run VS Code on a remote server, and access it through a browser. While not ideal, this is at least one way to get VS Code onto an iPad.

First, SSH into your server, so that we can set up `code-server`,

```bash
$ ssh user@example.com
$ curl -fsSL https://code-server.dev/install.sh | sh
```

Neat! 🙂 This was previously multiple steps, but code-server's recent addition of the quick-install script makes this painless.

## Securing the setup for remote access
So far, `code-server` is only listening for local connections, but we'd like to be able to use it on the go, from a browser on the iPad. This means we have to do a little extra work to secure our setup.

`code-server` covers how to do this [in their FAQ](https://github.com/cdr/code-server/blob/master/doc/FAQ.md#how-should-i-expose-code-server-to-the-internet){target="_blank" rel="noopener noreferrer"}, but skips the specific steps. Due to an issue with self-signed certificates on iOS, we cannot use these^[See issue [code-server#1122](https://github.com/cdr/code-server/issues/1122){target="_blank" rel="noopener noreferrer"} covering this.], so instead we will opt for [letsencrypt](https://letsencrypt.org){target="_blank" rel="noopener noreferrer"}!

<!--
We'll set up a self-signed certificate. For the passphrase, simply press enter to put a blank password on the key.

```bash
$ mkdir .code-server-meta
$ cd .code-server-meta
$ openssl req -x509 -nodes -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -subj "/C=NA/ST=None/L=Global/O=Company Name/OU=Org/CN=localhost"
Generating a RSA private key
...
```

Here we made a directory to hold our keys for `code-server`, and generated them using `openssl`. You can adjust the days to a number you are comfortable with, here I just went with 365, meaning I'll have to renew the certificate in a year.
-->

First, we will install certbot, which will manage the certificate renewal on our server,

```bash
$ sudo apt-get install software-properties-common # for add-apt-repository
$ sudo add-apt-repository ppa:certbot/certbot
$ sudo apt install certbot
```

Because these certificates are managed under `certbot`, we'll need to set up a script that will move the certificates to a location we want, so our `code-server` does not need root permissions. We'll do this with a [deploy-hook](https://certbot.eff.org/docs/using.html#renewing-certificates){target="_blank" rel="noopener noreferrer"}, which runs after each successful renewal.

Let's make a directory for the certificates. For convenience we will also export our domain name as an environment variables, to be used throughout the rest of the post (change `vscode.example.com` to your own domain and `XXXXxxxxXXXXXxxxxxxxXXXXXX` to your secret password),

```bash
$ mkdir .code-server-meta
$ cd .code-server-meta
$ export DOMAIN=vscode.example.com
$ export CODE_SERVER_PASSPHRASE=XXXXxxxxXXXXXxxxxxxxXXXXXX
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

We'll now set up our certificates by starting `certbot`. During this, you will be asked for your email and agree to the terms of service.

```bash
$ sudo certbot certonly --standalone --preferred-challenges http -d $DOMAIN
```

This will create the certificates in `/etc/letsencrypt/live/$DOMAIN`. Check that everything works by doing a dry-run of the certificate renewal,

```bash
$ sudo certbot renew --dry-run
```

Excellent! We can now configure code-server to use these certificates,

```bash
$ echo "
bind-addr: 0.0.0.0:8080
auth: password
password: $CODE_SERVER_PASSPHRASE
cert: $HOME/.code-server-meta/cert.pem
cert-key: $HOME/.code-server-meta/key.pem
" > .config/code-server/config.yml
$ systemctl --user restart code-server
```

Navigate to your domain on port `8080`. A login screen should appear. Use the password that the server printed, and you are in! 🥳

<div class="clear two-images">
  <a href="/resources/images/visual-studio-on-ipad-welcome.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/visual-studio-on-ipad-welcome.thumbnail.png" loading="lazy" alt="Welcome screen in Visual Studio Code on iPad" title="Welcome screen in Visual Studio Code on iPad" style="margin-right: 1%; width: 49%;" /></a>
  <a href="/resources/images/visual-studio-on-ipad-code-file.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/visual-studio-on-ipad-code-file.thumbnail.png" loading="lazy" alt="Code file in Visual Studio Code on iPad" title="Code file in Visual Studio Code on iPad" style="margin-left: 1%; width: 49%;" /></a>
</div>
<div class="clear"></div>

Congratulations, you've now got a stable setup for editing code in your iPad browser 🎉

### Certbot not updating automatically

Since we are running the standalone version of certbot, we'll need port 80 to be free. Make sure `sudo certbot renew --dry-run` does not complain about not being able to connect on port 80. If you experience this, check that you don't have nginx or apache occupying the port,

```bash
$ sudo systemctl status nginx
$ sudo systemctl status apache2
```

If they are, disable and stop them using `systemctl`. If this is not possible, then you can make certbot utilize either of these. DigitalOcean has some excellent guides:

- [How To Secure Nginx with Let's Encrypt on Ubuntu 18.04](https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-18-04)
- [How To Secure Apache with Let's Encrypt on Ubuntu 18.04](https://www.digitalocean.com/community/tutorials/how-to-secure-apache-with-let-s-encrypt-on-ubuntu-18-04)

## Changelog

- **20th of June, 2020**
  - Removed section on how to daemonize the setup now that `code-server` has added their own support for this
  - Switched to using `code-server`'s new install script instead of downloading and setting up assets manually
  - Updated letsencrypt instructions to utilize `code-server`'s new config.yml to contain cert paths and password
