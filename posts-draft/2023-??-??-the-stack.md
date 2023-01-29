---
title: "The Stack": Everything you'll need
tags: aws, cloud, infrastructure
---

# Governance and setup
Goals: Low cost, high flexibility, modular approach to infrastructure pieces.

- Mermaid diagram
- Control Tower and audit trail
- Billing alerts and tags

# Deployment and CI
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
- WAF

# Queues and Pub/Sub
- SQS
- SES + SNS
- Pub/sub setup, SNS vs EventBridge
  - https://www.altostra.com/blog/aws-pub-sub
  - https://aws.amazon.com/pub-sub-messaging/
- ETL setup? Fargate? Analytics

# Internal Applications
- Access control? SSO? Cognito?

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

# Monitoring and observability
- Xray
- CloudWatch Dashboards
