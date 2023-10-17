---
title: "The Stack Part 1: Setting up your AWS Account Structure"
tags: aws, cloud, infrastructure
---

In [the last post](/posts/2023-01-29-the-stack.html) we went over the overall goals of "The Stack" and what we will be building. In this post we'll be setting up our AWS Account structure. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

As a reminder, here is the structure we are aiming for:

- **Control Tower**: This is your central place to control access and policies for all accounts in your organization
- **Production Multi-tenant**: Your primary production account for multi-tenant setup, and most likely were the majority of users will be
- **Production Single-tenant**: While desirable to avoid the operation overhead for single-tenant setups, its good to think in this from the get-go
- **Integration Test**: This will be the account that IaC deployments get tested on to ensure rollout works
- **Preview**: This will be used to spin up Preview Environments later on
- **Individual Developer**: Individual developer accounts to allow easy testing of IaC testing and exploration
- **Monitoring**: Centralize monitoring and observability into one account, allowing access to insights without access to sensitive logs or infrastructure from the other accounts
- **Logs**: Centralized storage of logs, which may require different access considerations than metrics and traces

<div></div><!--more-->

Or, for the visual learners:

<pre class="mermaid">
graph TD
  subgraph ControlTower[AWS: Control Tower]
    AuditLog[Audit Log]
    GuardRails[Guard Rails]
  end
  ControlTower-->AWSProdMultiTenantAccount
  ControlTower-->AWSProdSingleTenantAccount
  ControlTower-->AWSIntegrationTestAccount
  ControlTower-->AWSPreviewAccount
  ControlTower-->AWSIndividualDeveloperAccount
  ControlTower-->AWSMonitoringAccount
  ControlTower-->AWSLogsAccount

  subgraph AWSProdMultiTenantAccount[AWS: Production Multi-tenant]
    AccountFillerProdMultiTenant[...]
  end

  subgraph AWSProdSingleTenantAccount[AWS: Production Single-tenant]
    AccountFillerProdSingleTenant[...]
  end

  subgraph AWSIntegrationTestAccount[AWS: Integration Test]
    AccountFillerIntegrationTest[...]
  end

  subgraph AWSPreviewAccount[AWS: Preview]
    AccountFillerPreview[...]
  end

  subgraph AWSIndividualDeveloperAccount[AWS: Individual Developer]
    AccountFillerIndividualDeveloper[...]
  end

  subgraph AWSMonitoringAccount[AWS: Monitoring]
    direction LR
    CloudWatchDashboards[CloudWatch Dashboards]
    CloudWatchMetrics[CloudWatch Metrics/Alarms]
    XRay[XRay Analytics]
  end

  subgraph AWSLogsAccount[AWS: Logs]
    CloudWatchLogs[CloudWatch Logs]
  end

  classDef container stroke:#333,stroke-width:2px,fill:transparent,padding:8px
  class ControlTower,AWSProdMultiTenantAccount,AWSProdSingleTenantAccount,AWSIntegrationTestAccount,AWSPreviewAccount,AWSIndividualDeveloperAccount,AWSMonitoringAccount,AWSLogsAccount container;
</pre>

Let's jump into it, you can see the different sections here:

- [Set up Control Tower](#set-up-control-tower)
    - [Step 1](#step-1)
    - [Step 2](#step-2)
    - [Step 3](#step-3)
    - [Step 4](#step-4)
    - [Step 5](#step-5)
    - [Cleanup](#cleanup)
- [Adjusting Account Factory defaults](#adjusting-account-factory-defaults)
- [Set up our Development Organizational Unit](#set-up-our-development-organizational-unit)
- [Set up our AWS Accounts](#set-up-our-aws-accounts)
- [Next Steps](#next-steps)

## Set up Control Tower

AWS has an excellent [Getting Started Guide](https://docs.aws.amazon.com/controltower/latest/userguide/quick-start.html) which goes through setting up a new Control Tower at a high-level. We'll do a few adjustments to the defaults to make it fit our needs.

Fist off, you will need an existing AWS Account. This is the one we are turning into our Control Tower, or also called our "Landing Zone". If you don't already have an account ready to use, then [go setup your AWS Account first](https://aws.amazon.com/).

Now that we are ready, a high-level overview of the steps we will be taking are:
1. Sign in to the AWS management console with your root user.
3. Navigate to the "Control Tower" console [here](https://console.aws.amazon.com/controltower).
4. Change to the region you want your base to be in (e.g. for me I prefer eu-west-1).
5. Click "Set up landing zone" to start the process.
6. Set up your landing zone, following the detailed steps below

#### Step 1

The first screen you'll meet wants you to review various infomration and pricing as well as choose a few defaults. We are going to change some of the values:

- **Region deny setting**: Choose `Enabled` for this. We want to make sure that Control Tower is governing our accounts and resources.
- We will need `us-east-1` for certain "global" resources, so we'll add that to the list of allowed regions along with your desired region.

#### Step 2

Now we need to create our Organizational Units (OUs). We want both a Foundation and an Additional OU, but we will rename them a bit to make more sense for our use-case:

- **Foundation OU**: We'll call this `Compliance` since it contains our Logs as well as our Audit accounts.
- **Additonal OU**: This is where we will put our Production workloads into, so we'll call this `Production`.

#### Step 3

The Foundation OU, **Compliance**, creates two accounts for us. We to set up emails for these accounts, but we will keep the names. I recommend pointing this to an administrator email and using `+` to allow the same email to be used for multiple accounts:

- **Log Archive**: `administrator+log-archive@example.com` (adjust to an account and domain you control)
- **Audit**: `administrator+audit@example.com` (adjust to an account and domain you control)

#### Step 4

Now that we have a location for our logs, we can configure CloudTrail as well as log retention. You can adjust to your needs, but I recommend the following:

- **CloudTrail**: `Enabled`
- **Amazon S3 bucket retention for logging**: `1 years`
- **Amazon S3 bucket retention for access logging**: `10 years`
- **KMS Encryption**: `Enable and custome encryption settings`

When you check the box to enable KMS encryption you'll be asked for a key to use. Click "Create a KMS key" which will take us into the KMS Console:

Key configuration

- **Symmetric**
- **Encrypt and decrypt**

Key labels

- **KMS Key Alias**: `control-tower-cloudtrail`
- **Description**: `KMS key used for CloudTrail logs stored by Control Tower`
- **Tags**:
  - Key: `billing`, Value: `cloudtrail-control-tower`
  - Key: `billing-group`, Value: `control-tower`

We'll immediately start our good habit of adding billing tags whereever we can, which greatly simplifies diving into cloud expenses.

<details>
<summary>👈 Open the toggle for an example</summary>

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-1-kms-step-1.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-1-kms-step-1.thumbnail.png" loading="lazy" alt="KMS Configuration" title="KMS Configuration" width="70%" /></a>
</div>

</details>

Skip through Step 3, Step 4, and click "Finish" on the review step.

Once the key is created we'll immediately edit it now that it has gotten a Key ID. If we don't we'll run into this nice error later on:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-1-kms-error.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-1-kms-error.thumbnail.png" loading="lazy" alt="KMS error" title="KMS error" width="70%" /></a>
</div>

To avoid this:

1. Click on your newly created key.
2. Note down the Key ID (e.g. `12345678-1234-1234-1234-123456789012`).
3. Note down your AWS Account ID (e.g. `123456789012`).
4. Click "Edit" and switch to the JSON policy editor.
5. Add the following policies, with values replaced, at the end of the `Statement` list.


Replace `AWS_REGION`, `AWS_ACCOUNT_ID`, and `KMS_KEY_ID` and insert the following:

```json
{
    "Sid": "Allow Config to use KMS for encryption",
    "Effect": "Allow",
    "Principal": {
        "Service": "config.amazonaws.com"
    },
    "Action": [
        "kms:Decrypt",
        "kms:GenerateDataKey"
    ],
    "Resource": "arn:aws:kms:AWS_REGION:AWS_ACCOUNT_ID:key/KMS_KEY_ID"
},
{
    "Sid": "Allow CloudTrail to use KMS for encryption",
    "Effect": "Allow",
    "Principal": {
        "Service": "cloudtrail.amazonaws.com"
    },
    "Action": [
        "kms:GenerateDataKey*",
        "kms:Decrypt"
    ],
    "Resource": "arn:aws:kms:AWS_REGION:AWS_ACCOUNT_ID:key/KMS_KEY_ID",
    "Condition": {
        "StringEquals": {
            "aws:SourceArn": "arn:aws:cloudtrail:AWS_REGION:AWS_ACCOUNT_ID:trail/aws-controltower-BaselineCloudTrail"
        },
        "StringLike": {
            "kms:EncryptionContext:aws:cloudtrail:arn": "arn:aws:cloudtrail:*:AWS_ACCOUNT_ID:trail/*"
        }
    }
}
```

This ensures that both Config and CloudTrail can use the key for encryption and decryption.

#### Step 5

Review and confirm the setup.

#### Cleanup

Finally, we'll also cleanup the VPCs that were created in our Control Tower:

1. Go to the [AWS Console -> VPC](https://console.aws.amazon.com/vpc/).
2. Click on **Your VPCs** in the menu on the left, and click on your Control Tower VPC.
3. Choose **Actions** and then choose **Delete VPC** as well as confirming the choice.

<div class="callout">
  <div class="callout-bulb">💡</div>
  If you missed the KMS policy adjustment in Step 4 and the creation fails, you can go into CloudFormation and delete the stack called `AWSControlTowerBP-BASELINE-CLOUDTRAIL-MASTER`. Once that is done, you can go back to the Control Tower console and click "Retry" to try again.
</div>

## Adjusting Account Factory defaults

By default the Account Factory will be creating VPCs and Subnets in newly provisioned accounts. We don't want this as we are focusing on serverless, so we'll disable this.

1. Go to the [AWS Console -> Control Tower](https://console.aws.amazon.com/controltower/).
2. Go into **Account Factory**.
3. Click **Edit** on the **Network configuration** card.
4. Set the **Maximum number of private subnets** to `0`
5. Uncheck any checkboxes in the **Regions for VPC creation** list.
6. Click **Save**.

## Set up our Development Organizational Unit

We've only set up two OUs so far, `Compliance` and `Production`, but we have one more we'd like to use. Jump into your Organization overview in Control Tower:

1. Go to the [AWS Console -> Control Tower](https://console.aws.amazon.com/controltower/).
2. Go into **Organization**.
3. Click **Create resources** and choose **Create organizational unit**.
4. Call it `Development` and set the Parent OU to `Root`.

This will take a bit of time, and we cannot create accounts during this.

## Set up our AWS Accounts

For this step it's important that you are not logged in as the Root user anymore. Instead, jump to your new User portal URL which AWS has set up for us.

Find the User portal URL:

1. Go to the [AWS Console -> Control Tower](https://console.aws.amazon.com/controltower/).
2. Go into **Users and access**.
3. Copy the **User portal URL** from the **Federated access management** card.
4. Bookmark this URL, you're gonna need it a lot.

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-1-user-portal-url.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-1-user-portal-url.thumbnail.png" loading="lazy" alt="User portal URL" title="User portal URL" width="65%" /></a>
</div>

Once you're logged into the Control Tower account using the portal, jump into the Account Factory again:

1. Go to the [AWS Console -> Control Tower](https://console.aws.amazon.com/controltower/).
2. Go into **Account Factory**.
3. Click **Create account**.

We'll be creating the following accounts:

- `Integration Test` in our Development OU
- `Preview` in our Development OU
- `Production Multi-tenant` in our Production OU
- `Production Single-tenant` in our Production OU
- `Monitoring` in our Production OU
- `Logs` in our Production OU

For each account, in the **Create account** process, fill in:

- **Account email**: An unused email for the account, e.g. `administrator+integration@example.com`
- **Display name**: The name of the account, e.g. `Integration Test`
- **IAM Identity Center user email**: The email of the user that will be the administrator of the account, e.g. `administrator+integration@example.com`
- **IAM Identity Center user first name**: The first name of the user that will be the administrator of the account, e.g. `Integration`
- **IAM Identity Center user last name**: The last name of the user that will be the administrator of the account, e.g. `Test`

Pick the appropriate Organizational unit according to the list above, and click **Create account**.

You should end up with an overall structure like the following (Preview environment missing, but should be in the Development OU):

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-1-account-overview.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-1-account-overview.thumbnail.png" loading="lazy" alt="Account overview" title="Account overview" width="40%" /></a>
</div>

## Next Steps

Next up, we will be looking at how we can set up and automate our deployments to these environments so that once we start building it will update automatically. Follow along in [Part 2 of the series](/posts/2023-01-29-the-stack-part-2.html).
