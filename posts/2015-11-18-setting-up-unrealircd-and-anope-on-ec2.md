---
title: Setting up UnrealIRCd and Anope IRC Services on EC2
tags: irc, aws, unrealircd, anope
---

_Versions used:_

* _UnrealIRCd 4.0.0-rc3_
* _Anope 2.0.2_

Having recently discovered <a href="http://sameroom.io" target="_blank">sameroom.io</a> I wanted to update the codetalk IRC server to be compliant with their
authentication method. This basically just meant enabling `SASL` support, but
while I was tinkering with stuff anyways, I thought I might as well streamline
the setup process for the IRC server. In short, everything is fully automated and
set up on AWS using `EC2` and `S3`.

This will go through the process of doing exactly that, by talking about:

* <a href="#sasl-support">enabling SASL support</a>
* <a href="#installing-unrealircd">installing `UnrealIRCd`</a>
* <a href="#installing-anope-irc-services">installing `Anope`</a>
* <a href="#automating-launch-of-ec2-and-install">a little about automating the launch of the `EC2`</a> instance that the IRC server will run on, using the `user-data` field as input for a setup
script.
* <a href="#github-repo-ready-to-fork">all the important files gathered in a github repo</a>

If you just want to throw yourself in the deep end, you can fork/clone the github
repository, alter the configuration and script variables to fit your need and
quickly be on with it. I recommend though that you skim through most of this to
get an overview of what is happening.


## SASL support
SASL (Simple Authentication and Security Layer) is a framework for
authentication and data security. For all intents and purposes we won't bother
with how it works specifically, since we are only interested in getting it
running.

UnrealIRCd supports authenticating with SASL using a remote service that
supports SASL. Here enters Anope which is commonly used for handling nickname
registration and other IRC services. Since `1.9.x` Anope has supported SASL. To
enable it, it needs to be compiled with the sasl modules (which it should be by
default).

The configuration for SASL is two part, a simple block for UnrealIRCd and
a block for Anope.

__UnrealIRCd__: The following links the Anope services to the IRC server and sets the
SASL server to the services server.

<?prettify?>
```
link services.myircserver.org {
    incoming {
        mask *;
    };
    outgoing {
        bind-ip  *;
        hostname 127.0.0.1;
        port     6697;
        options { ssl; autoconnect; };
    };
    password "MySecretPassword!";
    class    servers;
};

ulines {
	services.myircserver.org;
};
/* Enable SASL support */
set {
    sasl-server "services.myircserver.org";
};
```


__Anope__: Enabling SASL is fairly simple, and just requires that the module is
loaded. The rest is handled by Anope.

<?prettify?>
```
/* Load the SASL module m_sasl */
module { name = "m_sasl" }
```

If you are using the sample config in the [github repo](#github-repo-ready-to-fork), then SASL is already included.


## Installing UnrealIRCd
Since we want to automate the setup and installation, we need to install
UnrealIRCd in a way that requires absolutely no user input. First off though,
UnrealIRCd depends on some packages, and these will need to be installed.`

Since we are building UnrealIRCd from source, we will need some build tools.
These can be found in the `Development Tools` group. Other than that, there are
some curl, ssl and other libraries that needs to be installed.

<?prettify?>
```
yum -yq groupinstall "Development Tools" \
&& yum -yq install \
        curl \
        libcurl-devel \
        openssl-devel \
        openssl zlib \
        zlib-devel \
        ntp \
        c-ares \
```

Now we are able to compile UnrealIRCd from source. We will do all of this in
one giant step:

<?prettify?>
```
# UnrealIRCd version
UNREAL_VERSION="unrealircd-4.0.0-rc3"

curl https://www.unrealircd.org/downloads/$UNREAL_VERSION.tar.gz | tar xz \
&& cd $UNREAL_VERSION \
&& ./configure \
    --enable-ssl \
    --with-showlistmodes \
    --with-shunnotices \
    --with-confdir=/etc/unrealircd/config \
    --with-cachedir=/etc/unrealircd/cache \
    --with-scriptdir=/etc/unrealircd/unreal \
    --with-tmpdir=/etc/unrealircd/tmp \
    --with-modulesdir=/etc/unrealircd/modules \
    --with-logdir=/etc/unrealircd/log \
    --with-docdir=/etc/unrealircd/doc \
    --with-datadir=/etc/unrealircd/data \
    --with-pidfile=/etc/unrealircd/pid \
    --with-bindir=/usr/bin/unrealircd \
    --with-permissions=0600 \
    --enable-dynamic-linking \
&& make \
&& make install
```

This will install UnrealIRCd into `/etc/unrealircd` with SSL enabled. Since we
have SSL enabled, we will also need an SSL certificate! Luckily, this can also
be done through without any user input. It does require some information
though, so you should substitute the variables with your information.

<?prettify?>
```
# SSL certificate information
# The two-letter ISO abbreviation for your country
SSL_CERTIFICATE_COUNTRY="DK"
# The state or province where your organization is legally located
SSL_CERTIFICATE_STATE="Copenhagen"
# The city where your organization is legally located
SSL_CERTIFICATE_LOCATION="Copenhagen"
# The exact legal name of your organization
SSL_CERTIFICATE_ORGANIZATION="MyOrganization"
# Section of the organization
SSL_CERTIFICATE_ORGANIZATION_UNIT="IT"
# The fully qualified domain name for your web server
SSL_CERTIFICATE_COMMON_NAME="irc.myserver.org"
# Days the certificate is valid for
SSL_CERTIFICATE_DAYS=20000

mkdir -p /etc/unrealircd \
&& openssl req \
    -x509 \
    -newkey rsa:2048 \
    -keyout server.key.pem \
    -out server.cert.pem \
    -days $SSL_CERTIFICATE_DAYS \
    -nodes \
    -subj "/C=$SSL_CERTIFICATE_COUNTRY/ST=$SSL_CERTIFICATE_STATE/L=$SSL_CERTIFICATE_LOCATION/O=$SSL_CERTIFICATE_ORGANIZATION/OU=$SSL_CERTIFICATE_ORGANIZATION_UNIT/CN=$SSL_CERTIFICATE_COMMON_NAME" \
&& mv server.cert.pem /etc/unrealircd/config/ssl/ \
&& mv server.key.pem /etc/unrealircd/config/ssl/
```

Now you can put your configuration files into `/etc/unrealircd/config`. You can
read more in the automating section about automatically including the configs.


## Installing  Anope IRC services
Again the goal is to install without any human interaction needed. This step
will assume that UnrealIRCd has been installed first, since it needs some of
the tools (namely the `Development Tools` packages).

Anope uses (or at least we use it here) cmake to build. This means we have to
install cmake before doing anything else.

<?prettify?>
```
yum -y install cmake
```

Now we can compile Anope IRC services from source. We will fetch it and compile
it in one step:

<?prettify?>
```
# Anope version
ANOPE_VERSION="2.0.2"

curl -L https://github.com/anope/anope/releases/download/$ANOPE_VERSION/anope-$ANOPE_VRSION-source.tar.gz | tar xz \
&& cd anope-$ANOPE_VERSION-source \
&& mv modules/extra/m_ssl_openssl.cpp modules/ \
&& mv modules/extra/m_sasl_dh-aes.cpp modules/ \
&& mkdir build \
&& cd build \
&& cmake \
    -DINSTDIR:STRING=/etc/anope \
    -DDEFUMASK:STRING=077  \
    -DCMAKE_BUILD_TYPE:STRING=RELEASE \
    -DUSE_RUN_CC_PL:BOOLEAN=ON \
    -DUSE_PCH:BOOLEAN=ON .. \
&& make \
&& make install
```

Depending on your SSL config you might also need to generate some certificates
here. These are automatically generated in the full script included in the [github repo](#github-repo-ready-to-fork), along with a sample configuration setup.

It is probably worth noting that two extra modules were included in the Anope
build, namely `m_ssl_openssl` which enables SSL and `m_sasl_dh-aes` which
enables AES on SASL. You can take a look at the extra modules in the
`modules/extra` folder in the Anope source files.


## Automating launch of EC2 and install
There are a couple of things that need to be set up first. For starters, we
need to create a security group for the EC2 instance. You can do this in _AWS
Console -> EC2 -> Security Groups -> Create Security Group_. If you don't know
what you want to open here, open the two TCP ports `6667` and `6697` to
anywhere, and the SSH port `22` to anywhere also. The last one is optional, but
it is quite nice to be able to check out the logs if anything goes wrong.

After this we need to create an IAM role for the instance, so that it can fetch
configuration files and install scripts from S3. You can do this in _AWS Console
-> Identify & Access Management -> Roles -> Create New Role_ and then name the
role. You then need to attach a policy to it. This can either be full S3
access, or a more limited policy. For more on the latter see the post about
generating [S3 bucket specific policies](/posts/2015-11-18-s3-bucket-specific-policy.html).


__Now to the fun part!__ This will assume that you are using the install
scripts found in the [github repo](#github-repo-ready-to-fork), and that you
have uploaded them to S3. In the repo there is a script to quickly upload the
`install/install-anope.sh` and `install/install-unrealircd.sh` scripts, along
with tar/gzipping the config files and uploading them. The script is aptly
named `upload-to-s3.sh`.

First off, we will create an init script, which the instance will run on the
first launch. This will take care of installing everything and moving the files
into place, using the install scripts mentioned earlier. By having a IAM role
with S3 read access attached to it, we can download objects from the S3 bucket
directly.

<?prettify?>
```
#!/bin/bash
# Bucket location
export AWS_S3_BUCKET="YourBucket/install"
export AWS_DEFAULT_REGION=eu-central-1

# Download the files from S3
echo "Downloading install files" >> /home/ec2-user/log.txt
aws s3 cp --region $AWS_DEFAULT_REGION s3://$AWS_S3_BUCKET/install/install-unrealircd.sh /home/ec2-user/install-unrealircd.sh >> /home/ec2-user/log.txt
aws s3 cp --region $AWS_DEFAULT_REGION s3://$AWS_S3_BUCKET/install/install-anope.sh /home/ec2-user/install-anope.sh >> /home/ec2-user/log.txt

# Make the scripts executable
echo "Making scripts executable" >> /home/ec2-user/log.txt
chmod +x /home/ec2-user/install-unrealircd.sh >> /home/ec2-user/log.txt
chmod +x /home/ec2-user/install-anope.sh >> /home/ec2-user/log.txt

# Installing UnrealIRCd
echo "Starting install of UnrealIRCd (check log-unrealircd.txt)" >> /home/ec2-user/log.txt
touch /home/ec2-user/log-unrealircd.txt
/home/ec2-user/install-unrealircd.sh >> /home/ec2-user/log-unrealircd.txt

# Installing Anope
echo "Starting install of Anope (check log-anope.txt)" >> /home/ec2-user/log.txt
touch /home/ec2-user/log-anope.txt
/home/ec2-user/install-anope.sh >> /home/ec2-user/log-anope.txt
```

The above fetches the scripts down, executes them which in turn installs
UnrealIRCd and Anope.

While launching stuff from the console is indeed very fun...the first couple of
times, it quickly gets tedious. Therefore we will utilize the AWS API, to
create an EC2 instance, tag it and associate an elastic IP to it.

<?prettify?>
```
#!/bin/bash

# AWS user credentials
export AWS_ACCESS_KEY_ID=MyAccessKey
export AWS_SECRET_ACCESS_KEY=MySecretKey
export AWS_DEFAULT_REGION=eu-central-1

# EC2 instance details
NAME_TAG="irc.myserver.org"
IMAGE_ID="ami-bc5b48d0" # Amazon Linux AMI 2015.09.1 (HVM), SSD Volume Type
SNAPSHOT_ID="snap-f1a95375" # That snapshot depends on the AMI above
INSTANCE_TYPE="t2.micro"
KEY_NAME="irc-server"
SECURITY_GROUP="IRC"
IAM_ROLE="irc.codetalk.io"
ELASTIC_IP=168.1.1.1

# Launch an EC2 instance
echo "> Launching the EC2 instance..."
INSTANCE_ID=$( aws ec2 run-instances \
    --image-id $IMAGE_ID \
    --instance-type $INSTANCE_TYPE \
    --key-name $KEY_NAME \
    --security-groups $SECURITY_GROUP \
    --iam-instance-profile Name=$IAM_ROLE \
    --block-device-mapping DeviceName=/dev/xvda,Ebs="{SnapshotId=$SNAPSHOT_ID,VolumeSize=30,DeleteOnTermination=true,VolumeType=gp2}" \
    --user-data file://initec2.sh \
    | jq --raw-output '.Instances[0].InstanceId' )
echo "> Instance $INSTANCE_ID is launching"

# Add name, environment and company tags to the instance
echo "> Adding tags to the instance $INSTANCE_ID"
aws ec2 create-tags \
    --resources $INSTANCE_ID \
    --tags "[
        {\"Key\": \"Name\", \"Value\": \"$NAME_TAG\"}
    ]"

# Waiting for the instance to be running
echo "> Waiting until $INSTANCE_ID is running..."
aws ec2 wait instance-running --instance-ids $INSTANCE_ID
echo "> Instance is up"

# Associate an elastic IP with the instance
echo "> Associating the IP $ELASTIC_IP with instance $INSTANCE_ID"
ASSOC_ID=$( aws ec2 associate-address --instance-id $INSTANCE_ID --public-ip $ELASTIC_IP )
echo "> Done!"
```

The script should mostly be self-explanatory. The important parts are under the
`# EC2 instance details` comment. Here are the values that you should configure
to match what you need. The AMI ID can be found in the AMI store (you can start
launching an instance and stop after the first screen).


## Github repo, ready to fork!
All of the scripts and configuration files to set it all up can be found in
this [github repo](https://github.com/codetalkio/AWS-IRC-Setup). You'll want to
change the configuration files in the `config` folder to fit your server
details.

Furthermore you need to fit the credentials and server details to your own, in
the `initec2.sh`, `launch-ec2-instance.sh` and `upload-to-s3.sh` scripts.
Hopefully it should be evident from the naming of the variables, what it is
they expect.
