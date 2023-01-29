---
title: "The Stack": Everything you'll need
tags: aws, cloud, infrastructure
---

# Governance and setup
Goals: Low cost, high flexibility, modular approach to infrastructure pieces.

- Mermaid diagram
- Control Tower and audit trail
- Billing alerts and tags

Let's first sketch out the Account Governance structure, before diving into the view of each individual AWS account:

- **Control Tower**: This is your central place to control access and policies for all accounts in your organization
- **Production Multi-tenant**: Your primary production account for multi-tenant setup, and most likely were the majority of users will be
- **Production Single-tenant**: While desirable to avoid the operation overhead for single-tenant setups, its good to think in this from the get-go
- **Integration Test**: This will be the account that IaC deployments get tested on to ensure rollout works
- **Individual Developer**: Individual developer accounts to allow easy testing of IaC testing and exploration
- **Monitoring**: Centralize monitoring and observability into one account, allowing access to insights without access to sensitive logs or infrastructure from the other accounts
- **Logs**: Centralized storage of logs, which may require different access considerations than metrics and traces

```mermaid
graph TD
  subgraph ControlTower[AWS: Control Tower]
    AuditLog[Audit Log]
    GuardRails[Guard Rails]
  end
  ControlTower-->AWSProdMultiTenantAccount
  ControlTower-->AWSProdSingleTenantAccount
  ControlTower-->AWSIntegrationTestAccount
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
```

Next up, let's zoom in a bit on the various pieces our infrastructure consists of:

```mermaid
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
```

# Deployment and CI
- AWS CDK
    - Speed up https://pgrzesik.com/posts/speed-up-cdk-deploments/
    - CDK Watch https://aws.amazon.com/blogs/developer/increasing-development-speed-with-cdk-watch/
- Deployments
    - Canary environment
    - Dev environments?
    - Canary deployment and rollback
    - Blue/green?
- CI
    - Build
    - Deploy to integration
    - Check
    - Deploy canary to production
    - Check canary specifically?

# Database
- DynamoDB
- Aurora Serverless v2?
    - Not fully serverless

# Frontend: Web and Mobile
- S3 + CloudFront
  - Tiny Leptos todo app as example
- Mobile build
  - PWA?
    - https://web.dev/learn/pwa/getting-started/
    - https://firt.dev/notes/pwa-ios/
    - http://pwabuilder.com
    - Notifications are coming https://webkit.org/blog/12945/meet-web-push/
    - https://medium.com/@firt/whats-new-on-ios-12-2-for-progressive-web-apps-75c348f8e945
    - Doesn't really seem ideal
  - Capacitor?
    - https://capacitorjs.com

# Backend: API and Services
- API Gateway + Lambda
    - CloudFront in front for better traffic price
    - Federated schema + two services
    - Authentication cached via API Gateway
    - Pin Router Lambda to versions of Subgraphs for safe deployments
    - Lambda Pricing
      - $0.20 per 1M requests
      - $0.0000133334 for every GB-second (1024 MB Memory = $0.0000000167 per 1ms)
    - API Gateway Pricing
      - HTTP APIs: $1.11/million
      - REST APIs: $3.50/million
- WAF
  - https://www.wellarchitectedlabs.com/security/300_labs/300_multilayered_api_security_with_cognito_and_waf/3_prevent_requests_from_accessing_api_directly/
- Websockets via API Gateway
  - $1.14 per million messages
  - $0.285 per million connection minutes

# Queues and Pub/Sub
- SQS
  - From 1 Million to 100 Billion Requests/Month: $0.40/million
- SNS
  - Mobile Push Notifications: $0.50 per 1 million requests
  - Email/Email-JSON: $2.00 per 100,000 notifications
- SES
  - $0.10/1000 emails
- Pub/sub setup, SNS vs EventBridge Pipes
  - https://www.altostra.com/blog/aws-pub-sub
  - https://aws.amazon.com/pub-sub-messaging/
  - DynamoDB Streams into EventBridge
    - https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-pipes-dynamodb.html
    - https://aws.amazon.com/eventbridge/pipes/
    - $0.40/million requests entering a Pipe
- ETL setup? Fargate? Analytics

# File and Media processing
- AWS Elemental MediaConvert
  - https://docs.aws.amazon.com/AmazonS3/latest/userguide/tutorial-s3-batchops-lambda-mediaconvert-video.html
- S3 Intelligent Tiering (objects larger than 1MB)
- Image resizing via Lambd@Edge
  - https://aws.amazon.com/blogs/networking-and-content-delivery/resizing-images-with-amazon-cloudfront-lambdaedge-aws-cdn-blog/

# Internal Applications
- Access control? SSO? Cognito?
  - https://aws.amazon.com/blogs/networking-and-content-delivery/authorizationedge-using-cookies-protect-your-amazon-cloudfront-content-from-being-downloaded-by-unauthenticated-users/

# Monitoring and observability
- Xray
  - Cross-account
    - https://docs.aws.amazon.com/xray/latest/devguide/xray-console-crossaccount.html
    - https://aws.amazon.com/about-aws/whats-new/2022/11/amazon-cloudwatch-cross-account-observability-multiple-aws-accounts/
- CloudWatch Dashboards

# Local development
- Stitching all the pieces together
- Frontend against deployed backend
- Frontend against local backend
- Backend against deployed database
- Backend against local database
- SQS locally?
- Pub/Sub locally?

# Pull Request development
- Preview Environments for PRs
- Run local Frontend + Backend against Deployed?
- SQS and Pub/Sub?

# Support
- Amazon Connect?
  - https://docs.aws.amazon.com/connect/latest/adminguide/tutorial1-create-helpdesk.html
  - Unlikely to be an ideal solution compared to e.g. Intercom
