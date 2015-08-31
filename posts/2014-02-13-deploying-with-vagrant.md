---
title: Deploying with Vagrant
---

_As of writing, the newest version of Vagrant is 1.4.3._

I recently started looking into ways that I could improve my current deployment workflow. Since my server doesn't have much RAM, I currently build the binaries locally in a Virtual Machine (VM from here on out) and then send them to the server using scp.

Although I can't do much about the server part (except buying a bigger server), I can do something about what I do locally. I set out to check what possibilities I had, and ended up looking at <a href="http://www.vagrantup.com" target="_blank" title="Vagrant" alt="Vagrant">Vagrant</a>.

While it doesn't cut hugely from the local part of my deployment hassles, it could atleast streamline it a bit, and help remove the cabal hell that I'd sometimes run into with my current setup (having multiple applications that I deploy from the same VM).

A quick overview of the article:

* [What is Vagrant?](#whatisvagrant)
* [Getting started with Vagrant](#gettingstartedwithvagrant)
* [Replicating your server](#replicatingyourserver)
* [Rolling your own Vagrant box](#rollingyourownvagrantbox)
* [Setting up the VM](#settingupthevm)
  * [Sudo access](#sudoaccess)
  * [Installing VirtualBox's Guest Additions](#installingvirtualboxsguestadditions)
  * [Installing packages](#installingpackages)
  * [Custom packages](#custompackages)
  * [SSH keys](#sshkeys)
* [Exporting the VM box](#exportingthevmbox)
* [But how do we use it?](#buthowdoweuseit)
* [Concluding thoughts](#concludingthoughts)

# What is Vagrant?
Vagrant basically manages your VMs in a manner that is local to each project you use it in (although you can use it in a more general way). As standard it uses <a href="https://www.virtualbox.org" target="_blank" title="VirtualBox" alt="VirtualBox">VirtualBox</a>, but you can also use it with other providers, such as <a href="http://www.vmware.com" target="_blank" title="VMWare" alt="VMWare">VMWare</a>. 

That said, for the purpose of being convenient to most people, I'm going to assume VirtualBox throughout this article, since that is the free alternative of the two I mentioned.

# Getting started with Vagrant
Vagrant has a nice <a href="http://docs.vagrantup.com/v2/getting-started/index.html" target="_blank" title="Getting started with Vagrant" alt="Getting started with Vagrant">getting started</a> series that you should follow if you want to understand Vagrant better (I highly recommend it, it's not that long). I'm going to try and give a quick explanation of how it works though.

Vagrant uses something called a `box`. These serve as the basis for the Vagrant setups you create, and are in essence just VMs with the bare minimum installed. To add a box you do, 

<pre class="prettyprint lang-sh">
vagrant box add precise64 http://files.vagrantup.com/precise64.box
</pre>

which will add a VM with a 64-bit version of Ubuntu 12.04 (Precise Pangolin). It will from then on be under the name `precise64`, stored on your computer. You can change the url with any other url or a local filepath, and the name with what you want your box to be refferred to as.

You can try and search around if you can find a box that matches your needs, but if you want to customize it completely, I recommend creating your own.

# Replicating your server
Before we start creating our Vagrant box, we need to know what our server looks like, so we can get the correct distro image.

On Debian, and most likely others, you can check out what version you're using with, <kbd>cat /etc/*-release</kbd>. This will output something like, 

<pre class="prettyprint lang-sh">
PRETTY_NAME="Debian GNU/Linux jessie/sid"
NAME="Debian GNU/Linux"
ID=debian
ANSI_COLOR="1;31"
HOME_URL="http://www.debian.org/"
SUPPORT_URL="http://www.debian.org/support/"
BUG_REPORT_URL="http://bugs.debian.org/"
</pre>

which tells us we're dealing with Debian Jessie (testing).

Next up, is the architecture, and we get that by running, <kbd>uname -r</kbd>, which may look like,

<pre class="prettyprint lang-sh">
3.2.0-4-amd64
</pre>

with `amd64` telling us it's 64 bit and built for amd. You should also try and match the kernel version as best you can though (the `3.2.0`).

# Rolling your own Vagrant box
First off, you need to install <a href="https://www.virtualbox.org" target="_blank" title="VirtualBox" alt="VirtualBox">VirtualBox</a>, and then go and download the installer image for the distro you want. Since my server uses the amd64 version of Debian Jessie, I'm going to go ahead and download the <a href="http://www.debian.org/devel/debian-installer/" target="_blank" title="Debian amd64 netinst CD image" alt="Debian amd64 netinst CD image">amd64 netinst CD image</a>.

After downloading it you launch it in VirtualBox and install it as you normally would a Virtual Machine. Following Vagrant's <a href="http://docs.vagrantup.com/v2/virtualbox/boxes.html" target="_blank" title="Guidelines for creating a base box" alt="guidelines for creating a base box">guidelines for creating a base box</a>, I set the following properties (with some altercations of my own):

* `40GB` dynamically resizing drive
* `2GB` RAM (they recommend 360MB, but compiling is quite memory intensive, and my laptop has 8GB of memory already)
* Audio disabled (we will only ssh into it)
* USB disabled
* NAT networking

And as recommended, I set the following values:

* hostname: `vagrant-debian-jessie` 
* domain: `vagrantup.com`
* Root password: `vagrant`
* Main account login: `vagrant`
* Main account password: `vagrant`
* Uncheck all software, to get the bare minimum

Now that you have the VM up and running, it's time to set it up as a Vagrant box.

# Setting up the VM
Vagrant assumes sudo access without password, since that allows Vagrant to do a lot of neat thigns without our intervention (installing packages, setting up network folders etc). 

### Sudo access
First we change to root, and then,

<pre class="prettyprint lang-sh">
apt-get install sudo
</pre>

followed by editing the sudoers file with <kbd>visudo</kbd>. Here we add,

<pre class="prettyprint lang-sh">
# /etc/sudoers
Defaults    env_keep="SSH_AUTH_SOCK"
%admin ALL=(ALL:ALL) ALL
%admin ALL=NOPASSWD: ALL
</pre>

which allows users in the admin group to use `sudo` without a password.

After that, we need to add the vagrant user to the admin group,

<pre class="prettyprint lang-sh">
groupadd admin
usermod -a -G admin vagrant
</pre>

### Installing VirtualBox's Guest Additions
While still logged in as root, we need to uninstall VirtualBox packages and install linux headers so we can install the tools VirtualBox uses,

<pre class="prettyprint lang-sh">
apt-get autoremove virtualbox-ose-guest-dkms virtualbox-ose-guest-utils virtualbox-ose-guest-x11
apt-get install linux-headers-$(uname -r) build-essential
</pre>
note that there might not be able to find any of the packages we tried to remove, and that is fine, but sometimes they get installed automatically depending on the distro etc.

After that, we mount the Guest Additions CD, which is located in the `Devices` menu and then something like `Insert Guest Additions CD Image...`. We then run,

<pre class="prettyprint lang-sh">
mount /dev/cdrom /media/cdrom
sh /media/cdrom/VBoxLinuxAdditions.run
</pre>

### Installing packages
The last thing we need to do as root is installing the packages that we need,

<pre class="prettyprint lang-sh">
apt-get install ruby rubygems puppet openssh-server openssh-client
</pre>

depending on what distro you use, rubygems might fail to install. On debian Jessie, gems was included in the ruby package. After this, we need to set SSH to not use DNS. This will give a small speedup when SSH'ing to the VM,

<pre class="prettyprint lang-sh">
echo 'UseDNS no' >> /etc/ssh/sshd_config
</pre>

I intentionally didn't include `chef` here, since I don't personally need it, and the install process is also a tad more bothersome than puppet's. You would just add it here if you need it though.

### Custom packages
This step is a bit more personalized, whereas the rest is for creating a general Vagrant box. Since I intend to use this box for deploying and testing my Haskell packages, it's relevant for me to install the `haskell-platform` and an updated version of `cabal` to save some time when I create the VM with <kbd>vagrant up</kbd> later on.

You can also do these steps automatically on the creation of a box, by adding a shell script with the comamnds inside the `VagrantFile` in your project, sorta like the <a href="http://docs.vagrantup.com/v2/getting-started/provisioning.html" target="_blank" title="Automatically setting up Apache" alt="Automatically setting up Apache">example with automatically setting up Apache</a>.

That said, I did the following,

<pre class="prettyprint lang-sh">
apt-get install haskell-platform
cabal update
cabal install cabal-install
</pre>

This will save me a bit of time compiling things (especially `cabal-install`) over and over again.


### SSH keys
Finally, you can now jump back to the vagrant user with `exit`, since the last bit needs to be done in the home folder of the vagrant user. We need to setup the authorized SSH keys for the vagrant user so <kbd>vagrant ssh</kbd> works easily using their <a href="https://github.com/mitchellh/vagrant/tree/master/keys/" target="_blank" title="Vagrant standard insecure key pair" alt="Vagrant standard insecure key pair">standard insecure key pair</a>,

<pre class="prettyprint lang-sh">
mkdir .ssh
wget --no-check-certificate -O .ssh/authorized_keys https://raw.github.com/mitchellh/vagrant/master/keys/vagrant.pub
chmod 700 .ssh
chmod 600 .ssh/authorized_keys
</pre>

Now we just need to cleanup the VM and shut it down,

<pre class="prettyprint lang-sh">
sudo apt-get clean
sudo shutdown -h now
</pre>

# Exporting the VM box
The final step is to export the VM as a vagrant box and add it. Vagrant neatly picks up the VMs that VirtualBox manages (assuming you use the default location), so you can simply do,

<pre class="prettyprint lang-sh">
vagrant package --base Jessie64
</pre>

where `Jessie64` is the name I gave the VM inside VirtualBox. It will create a file named `package.box` at the location you ran the command.

Now you can add the box to Vagrant just like any other box,

<pre class="prettyprint lang-sh"> 
vagrant box add jessie64 ~/package.box
</pre>

# But how do we use it?
I'm going to give a little example of how I would set up and use Vagrant in one of my projects. 

First I create the VagrantFile inside my project, using the box I just created,

<pre class="prettyprint lang-sh">
vagrant init jessie64
</pre>

After this, I create a small script that serves to setup my project with the packages that it needs. The content of the script should be self-explanatory. I name this `bootstrap.sh` and it contains the following,


<pre class="prettyprint lang-sh">
#!/usr/bin/env bash

cd /vagrant

/home/vagrant/.cabal/bin/cabal update && /home/vagrant/.cabal/bin/cabal sandbox init && /home/vagrant/.cabal/bin/cabal install
</pre>

The `/vagrant` folder is a folder that Vagrant keeps in sync with the host filesystem. It is the folder that Vagrant is running in, which in this case is my project folder.

I then add a line,

<pre class="prettyprint lang-rb">
config.vm.provision :shell, :path => "bootstrap.sh"
</pre>

in my `VagrantFile` inside the `Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|` section.

After this my project I can do,

<pre class="prettyprint lang-sh">
vagrant up
</pre>

and my project will get completely bootstrapped and ready for me to compile it and deploy. This usually means doing <kbd>vagrant ssh</kbd> and then two simple commands,

<pre class="prettyprint lang-sh">
cd /vagrant
yesod keter
</pre>

with this, I've now build my binary, packaged it up and sent it to my server.


If there is some trouble with some packages I can simply do,

<pre class="prettyprint lang-sh">
vagrant destroy
</pre>

This will destroy the VM in my project (it still keeps the box we created before, don't worry). You can change <kbd>destroy</kbd> with <kbd>halt</kbd> to shut down the VM or <kbd>suspend</kbd> to simply suspend it. There is some information about the pros and cons to the three options <a href="http://docs.vagrantup.com/v2/getting-started/teardown.html" target="_blank" title="Vagrant documentation on teardown" alt="Vagrant documentation on teardown">in the Vagrant documentation</a>.

# Concluding thoughts
Vagrant has greatly made me more comfortable messing around with my local VMs, since it's so easy to just destroy one and spin up an exact replica.

It also allows me to easily sandbox my projects completely by having VagrantFile's in each of them, with exactly what they need. Since Vagrant syncs the project folder, I can make changes using my preferred tools and the VM keeps the folders in sync.

As to what comes after this; I'm looking into puppet at the moment, since I would like to make setting up my server completely automatic. This will be a future article for itself though.
