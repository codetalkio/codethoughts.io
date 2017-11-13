---
title: S3 bucket specific policy
tags: aws
---

I have recently started caring a bit more about security on my AWS
applications, and to this end Identity & Access Management (IAM) users are a great way to limit access to
a need-to-use-only basis.

Recently I set up my IRC server to download its configuration and install files
from an S3 bucket. This meant that it needed to have read access to a specific
bucket, and for this an IAM role was created.

There are two ways to generate policies:

* <a href="#using-the-policy-generator">Using the policy generator</a>
* <a href="#manually-creating-a-policy">Manually creating a policy</a>

I will generally advise to either use the generator completely or at least use
it for the basis of the policy you want to create.


## Using the policy generator
To access the generator go into IAM -> Policies -> Create Policy -> Policy Generator.

<div class="clear two-images">
  <a href="/resources/images/S3-create-policy.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/S3-create-policy.thumbnail.png" alt="S3 Create Policy" title="S3 Create Policy" /></a>
  <a href="/resources/images/S3-policy-generator.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/S3-policy-generator.thumbnail.png" alt="S3 Select Policy Generator" title="S3 Select Policy Generator" /></a>
</div>

You will then be able to create the policy. To do this, first set the AWS
service to `Amazon S3` and then choose the actions that you want to support.
Here I've chosen `GetObject` as I just want to allow the read of an object in
the bucket, and nothing more.

Finally, you need to set the `ARN` of your bucket. This will be something like
`arn:aws:s3:::YourBucketName/*` to allow access to all elements in your S3
bucket named `YourBucketName`. It'll look a little something like in the left figure below.

<div class="clear two-images">
  <a href="/resources/images/S3-edit-permissions.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/S3-edit-permissions.thumbnail.png" alt="S3 Edit Permissions" title="S3 Edit Permissions" /></a>
  <a href="/resources/images/S3-review-policy.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/S3-review-policy.thumbnail.png" alt="S3 Review Policy" title="S3
Review Policy" /></a>
</div>

Finally add the statement, and you can click next which should present you
with something like the right image above.


## Manually creating a policy
You can also skip the generator and create your own policy directly. This will
get you into a window looking much like the 'Review Policy' image in the
earlier section.

Here you just need to input a name, description and the policy document itself.
You can either use the generator as a basis for this document or craft your
own. The following is the one generated from the policy generator which can be
directly used here.

<div class="snippet-title">S3 bucket policy</div>
```javascript
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "Stmt1447546160000",
            "Effect": "Allow",
            "Action": [
                "s3:GetObject"
            ],
            "Resource": [
                "arn:aws:s3:::YourBucketName/*"
            ]
        }
    ]
}
```

Hopefully this will get you going and buff up your security on your AWS setup.
