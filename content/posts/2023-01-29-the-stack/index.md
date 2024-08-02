+++
title = "\"The Stack\": Everything you'll need"
date = 2023-01-29

[taxonomies]
tags = ["aws", "cloud", "infrastructure", "cdk", "rust"]

[extra]
mermaidjs = "true"
+++

This will be a series of blog posts where we will build up the perfect infrastructure setup for the majority of usecase, aka "The Stack". We'll be building everything on top of AWS.

Before diving in, let's first establish some goals:

- **Performant**: Latency and performance is important as this will serve end-users.
- **Low cost**: We want our base cost to be low, and our costs to scale well with high traffic. Ideally, it should cost nothing if no users are using it.
- **Low operational overhead**: It's >=2023, nobody wants to nurse servers or services anymore, things should scale up and down without intervention or oversight.
- **High flexibility**: Everything should be built with the foresight of future scalability, both organizationally, code-wise, and in the way things fit together.
- **Modular**: Pieces of the infrastructure should be opt-out, e.g. if you don't need Pub/Sub, it can be skipped.


Obviously, this is my personal opinion on it, but I'll be sharing the thinking behind each of the choices as we go along.

Some technology choices upfront:

- Everything will be infrastructure as code using [AWS CDK](https://github.com/aws/aws-cdk).
- We'll be using [Rust](https://www.rust-lang.org) throughout for each service, as it allows us to squeeze out the most performance while still giving us a nice developer experience.
- [Federated GraphQL](https://www.apollographql.com/docs/federation/) will be our way of facilitating microservices.

<div></div><!-- more -->

Here's an overview before we get into the details:

{{ toc() }}

## What will we be covering?

{% aside() %}
  You can see a sneak peak of the final setup below.
{% end %}

All of this will be covered in parts:

- [Part 0: The introduction and goals (this post)](#what-will-we-be-covering)
  - The goals of "The Stack" and architecture overview
- [Part 1: Setting up your AWS Account Structure](@/posts/2023-10-07-the-stack-part-1/index.md)
  - Setting up Control Tower and all of our AWS Accounts
- [Part 2: Automating Deployments via CI](@/posts/2023-10-08-the-stack-part-2/index.md)
  - Bootstrapping CDK and deploying to all accounts via CI
- [Part 3: Creating our Frontend](@/posts/2023-10-16-the-stack-part-3/index.md)
  - Creating an SPA and deploying it to S3 + CloudFront
- Part 4: A Federated GraphQL API
  - Federated GraphQL in Lambda with three subgraphs
- Part 5: Adding Databases (DynamoDB)
  - Make our subgraphs use DynamoDB for storage
- Part 6: Using our API from the Frontend
  - Connecting our Frontend to the API and setting up our GraphQL clients
- Part 7: Preview Environments in CI
  - Spin up environments in Pull Requests using GitHub Actions
- Part 8: Asynchronous work and processing
  - Queuing up work with SQS and decoupling services via Pub/Sub using EventBridge
- Part 9: Notifications and emails
  - Sending emails and Push Notifications
- Part 10: Monitoring, traces, and debugging
  - XRay traces and CloudWatch Dashboards
- Part 11 (Bonus): Websocket support for GraphQL
  - Support GraphQL subscriptions via API Gateway's websocket support
- Part 12 (Bonus): Video transcoding and image resizing
  - Transcode video files with MediaConvert and resize images on-the-fly
- Part 13 (Bonus): Mobile App
  - Package the Frontend as a Mobile App
- Part 14 (Bonus): Stripe integration
  - Integrating Stripe for payments and billing
- Part 15 (Bonus): Billing breakdown
  - Forming an overview of service costs via Billing Tags and the Cost Explorer

## Account Structure and Governance

Reorganizing your AWS Account structure is a pain, so let's see if we can get this right from the beginning. There are a few things that direct our choices here:

- Isolation of environments to avoid sharing soft and hard limits between environments (e.g. Lambda concurrency limits, etc)
  - We will be putting each environment in their own AWS Account
- Security, audit, and control to ease GDPR requirements
  - We'll use AWS Control Tower to give us full control of sub-accounts in our Organization

Let's first sketch out the Account Governance structure, before diving into the view of each individual AWS account:

- **Control Tower**: This is your central place to control access and policies for all accounts in your organization
- **Production Multi-tenant**: Your primary production account for multi-tenant setup, and most likely were the majority of users will be
- **Production Single-tenant**: While desirable to avoid the operation overhead for single-tenant setups, its good to think in this from the get-go
- **Integration Test**: This will be the account that IaC deployments get tested on to ensure rollout works
- **Preview**: This will be used to spin up Preview Environments later on
- **Individual Developer**: Individual developer accounts to allow easy testing of IaC testing and exploration
- **Monitoring**: Centralize monitoring and observability into one account, allowing access to insights without access to sensitive logs or infrastructure from the other accounts
- **Logs**: Centralized storage of logs, which may require different access considerations than metrics and traces

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

## Service Infrastructure

Each of the infrastructure accounts (Production, Integration, Developer) all hold the same services and follow the same setup. The infrastructure we will make might seem complex at first, and it is, but as we go through each piece everything will start to make sense.

The diagram gets quite large, so we will split it up into three parts:

- Client to Frontend
- Client to API
- Asynchronous Work and Media


### Client to Frontend

Let's focus first on the Client to Frontend paths:

- **Public Frontend**: A simple hosting of static files in S3 with CloudFront as the CDN in front of it.
- **Internal Frontend**: Hosting of internal applications, secured behind Cognito, and otherwise similar to the Public Frontend.


<pre class="mermaid">
graph TD
  Client
  Route53

  Client-->Route53
  Route53-->FrontendCloudFront
  Route53-->InternalCloudFront
  Route53-->CertificateACM

  Frontend-->API
  Internal-->API

  subgraph Certificate[ACM: Certificate]
    CertificateACM
  end

  subgraph Frontend[Frontend: Public]
    FrontendCloudFront[CloudFront]
    FrontendS3App[S3: Static UI Files]

    FrontendCloudFront-->FrontendS3App
  end

  subgraph Internal[Frontend: Internal]
    InternalCloudFront[CloudFront]
    InternalCognito[Cognito]
    InternalS3App[S3: Static UI Files]

    InternalCloudFront-->InternalCognito
    InternalCloudFront-->InternalS3App
  end

  subgraph API
    APIFiller[...]
  end

  classDef container stroke:#333,stroke-width:2px,fill:transparent,padding:8px
  class Certificate,Frontend,Internal,API,Media,Database,Notification,Async,Monitoring container;
</pre>


### Client to API

As we can see, the two Frontends need something to talk to, let's check out the APIs:

- **API**: A Federated GraphQL setup, served using Lambda with API Gateway exposing it to the internet. WAF is added for security and protection, and CloudFront for possibility of cheaper egress pricing via commitment to a certain volume.
- **Database**: DynamoDB tables for storing data.
- **Monitoring**: XRay traces and CloudWatch metrics/alarms.


<pre class="mermaid">
graph TD
  Client
  Route53

  Client-->Route53
  Route53-->APICloudFront

  subgraph API
    APICloudFront[CloudFront]
    APIWAF[WAF]
    APIAPIGateway[API Gateway]
    APILambdaAuthentication[Lambda: Custom Authorizer]
    APILambdaRouter[Lambda: GraphQL Supergraph<br>Apollo Router]
    APILambdaServiceReviews[Lambda: GraphQL Subgraph<br>Reviews Service]
    APILambdaServiceUsers[Lambda: GraphQL Subgraph<br>Users Service]
    APILambdaServiceProducts[Lambda: GraphQL Subgraph <br>Products Service]

    APICloudFront-->APIWAF-->APIAPIGateway

    APIAPIGateway--Cached-->APILambdaAuthentication

    APIAPIGateway-->APILambdaRouter
    APILambdaRouter-->APILambdaServiceReviews
    APILambdaRouter-->APILambdaServiceUsers
    APILambdaRouter-->APILambdaServiceProducts
  end

  subgraph Database
    DatabaseDynamoDB[DynamoDB]
  end

  %% APILambdaAuthentication-->Database
  APILambdaServiceReviews-->Database
  APILambdaServiceUsers-->Database
  APILambdaServiceProducts-->Database

  subgraph Monitoring[Monitoring]
    MonitoringXray[Xray]
    MonitoringCloudWatch[CloudWatch Metrics/Alarms]
  end

  API-->Monitoring

  classDef container stroke:#333,stroke-width:2px,fill:transparent,padding:8px
  class Certificate,Frontend,Internal,API,Media,Database,Notification,Async,Monitoring container;
</pre>

### Asynchronous Work and Media

And finally we can see the Media and Async work (the Database and some APIs reappear here as well):

- **Media**: S3 buckets for images and videos as well as MediaConvert for transcording video for wider device support.
- **Async**: SQS for asynchronous work via a queue and EventBridge for a Pub/Sub style architecture. An initial Analytics Lambda service is set up as the consumer of the Pub/Sub events.
- **Notification**: SES for emails and SNS for mobile push notifications.Z

<pre class="mermaid">
graph TD
  Client
  Route53

  Client-->Route53
  Route53-->APICloudFront

  subgraph API
    APILambdaServiceReviews[Lambda: GraphQL Subgraph<br>Reviews Service]
    APILambdaServiceProducts[Lambda: GraphQL Subgraph <br>Products Service]
  end

  subgraph Media
    MediaConvert[MediaConvert]
    MediaS3[S3: Media Files<br>Image Bucket + Video Bucket]
  end

  %% FrontendCloudFront-->MediaS3
  APILambdaServiceProducts--Create Signed URL-->MediaS3
  Client--Upload via Signed URL-->MediaS3
  APILambdaServiceProducts--Create Job-->MediaConvert

  subgraph Database
    DatabaseDynamoDB[DynamoDB]
  end

  subgraph Notification[Notification]
    NotificationSES[SES: Emails]
    NotificationSNS[SNS: Mobile Notification]
  end

  subgraph Async[Async Work]
    AsyncSQS[SQS]
    AsyncEventBridge[Event Bridge: Pub/Sub]
    AsyncLambdaAnalytics[Lambda: Analytics]
    AsyncLambdaNotification[Lambda: Notification]

    AsyncEventBridge-->AsyncLambdaAnalytics
    AsyncSQS-->AsyncLambdaNotification
  end

  DatabaseDynamoDB--Streams-->AsyncEventBridge
  AsyncLambdaAnalytics-->Database
  APILambdaServiceReviews-->AsyncSQS
  AsyncLambdaNotification-->Notification

  classDef container stroke:#333,stroke-width:2px,fill:transparent,padding:8px
  class Certificate,Frontend,Internal,API,Media,Database,Notification,Async,Monitoring container;
</pre>


### One diagram to rule them all

If we combine all the individual diagrams, we get:

<pre class="mermaid">
graph TD
  Client
  Route53

  Client-->Route53
  Route53-->FrontendCloudFront
  Route53-->InternalCloudFront
  Route53-->APICloudFront
  Route53-->CertificateACM

  subgraph Certificate[ACM: Certificate]
    CertificateACM
  end

  subgraph Frontend[Frontend: Public]
    FrontendCloudFront[CloudFront]
    FrontendS3App[S3: Static UI Files]

    FrontendCloudFront-->FrontendS3App
  end

  subgraph Internal[Frontend: Internal]
    InternalCloudFront[CloudFront]
    InternalCognito[Cognito]
    InternalS3App[S3: Static UI Files]

    InternalCloudFront-->InternalCognito
    InternalCloudFront-->InternalS3App
  end

  subgraph API
    APICloudFront[CloudFront]
    APIWAF[WAF]
    APIAPIGateway[API Gateway]
    APILambdaAuthentication[Lambda: Custom Authorizer]
    APILambdaRouter[Lambda: GraphQL Supergraph<br>Apollo Router]
    APILambdaServiceTodo[Lambda: GraphQL Subgraph<br>Todo Service]
    APILambdaServiceMedia[Lambda: GraphQL Subgraph <br>Media Service]

    APICloudFront-->APIWAF-->APIAPIGateway

    APIAPIGateway--Cached-->APILambdaAuthentication

    APIAPIGateway-->APILambdaRouter
    APILambdaRouter-->APILambdaServiceTodo
    APILambdaRouter-->APILambdaServiceMedia
  end

  subgraph Media
    MediaConvert[MediaConvert]
    MediaS3[S3: Media Files<br>Image Bucket + Video Bucket]
  end

  %% FrontendCloudFront-->MediaS3
  APILambdaServiceMedia--Create Signed URL-->MediaS3
  Client--Upload via Signed URL-->MediaS3
  APILambdaServiceMedia--Create Job-->MediaConvert

  subgraph Database
    DatabaseDynamoDB[DynamoDB]
  end

  %% APILambdaAuthentication-->Database
  APILambdaServiceTodo-->Database
  APILambdaServiceMedia-->Database

  subgraph Notification[Notification]
    NotificationSES[SES: Emails]
    NotificationSNS[SNS: Mobile Notification]
  end

  subgraph Async[Async Work]
    AsyncSQS[SQS]
    AsyncEventBridge[Event Bridge: Pub/Sub]
    AsyncLambdaAnalytics[Lambda: Analytics]
    AsyncLambdaNotification[Lambda: Notification]

    AsyncEventBridge-->AsyncLambdaAnalytics
    AsyncSQS-->AsyncLambdaNotification
  end

  DatabaseDynamoDB--Streams-->AsyncEventBridge
  AsyncLambdaAnalytics-->Database
  APILambdaServiceTodo-->AsyncSQS
  AsyncLambdaNotification-->Notification

  subgraph Monitoring[Monitoring]
    MonitoringXray[Xray]
    MonitoringCloudWatch[CloudWatch Metrics/Alarms]
  end

  API-->Monitoring

  classDef container stroke:#333,stroke-width:2px,fill:transparent,padding:8px
  class Certificate,Frontend,Internal,API,Media,Database,Notification,Async,Monitoring container;
</pre>


## Next Steps

Next up is to start building! Follow along in [Part 1 of the series here](@/posts/2023-10-07-the-stack-part-1/index.md).

{{ medium_comments(post="the-stack-everything-youll-need-6b3df3046904") }}
