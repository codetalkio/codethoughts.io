---
title: "The Stack Part 2: Automating Deployments via CI"
tags: aws, cloud, infrastructure, cdk
---

In [the last post](/posts/2023-01-29-the-stack-part-1.html) we created our Control Tower structure with all of our AWS Accounts in it. In this post we will be automating our deployment process for each of these environments. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

This will include:
- Workflow for bootstrapping our AWS Accounts for CDK ([see here](https://github.com/codetalkio/the-stack/blob/part-2-automatic-deployments/.github/workflows/cd-bootstrap.yml))
- Workflow for deploying our CDK stacks, including synthesizing and testing before ([see here](https://github.com/codetalkio/the-stack/blob/part-2-automatic-deployments/.github/workflows/cd-deploy.yml))

But first we must prepare our GitHub environments, setting it up with AWS credentials and configuring it with the environments we want to deploy to.

<div></div><!--more-->

# AWS: Set up our Credentials

For now, we will focus on the following of our accounts as deployment targets:
- Integration Test
- Production Single-tenant
- Production Multi-tenant

For each of these we will need to set up IAM credentials that we can use in GitHub Actions to deploy our resources into each of these accounts.

<div class="callout">
  <div class="callout-bulb">💡</div>
  GitHub also supports authenticating to AWS via OpenID, but this is much more complicated to set up. Check out their docs on that [here](https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/configuring-openid-connect-in-amazon-web-services).
</div>

Let's get set up. First we'll define a group for the user to go into, create the user, and then create the access keys for the user:

1. Go to the [AWS Console -> IAM](https://console.aws.amazon.com/iam/).
2. Go into **User groups**.
3. Create a new group called `ci-github-deployment`.
4. Give it the `AdministratorAccess` policy for now.
5. Go into **Users**.
6. Create a new user called `ci-github-deployment` without console access.
7. Add it to the `ci-github-deployment` group.

Finally, we need to create the access keys for the user:

1. Go into the newly created user.
2. Go into **Security credentials** and create a new access key.
3. Choose Command Line Interface (CLI) and click the checkbox to confirm.
4. Set the **Description tag value** to `Automatic deployments from GitHub Actions` or something appropriate.
5. Note down your Access and Secret Key somewhere safe (we'll need it later)

Repeat this process for `Integration Test`, `Production Single-tenant`, and `Production Multi-tenant`.


# GitHub: Setting up Environments
For our GitHub Actions workflows to work, we need to set up our `Environment`s configure a couple of `Environment` variables and secrets.

1. Go to your repository Settings -> Environments
2. Create your environments e.g. `Integration Test`
3. [Recommended] Restrict deployments to the `main` branch
4. Set up the secrets for
   1. `AWS_ACCESS_KEY_ID`
   2. `AWS_SECRET_ACCESS_KEY`
5. Set up the variables for
   1. `AWS_ACCOUNT_ID`
   2. `AWS_REGION`
   3. `DOMAIN` (where your app will live, e.g. `app.example.com`. `integration.example.com`, and `single.example.com`)

Repeat those steps with the relevant values for each of the environments `Integration Test`, `Production Single`, `Production Multi`.

Your `Environment` overview will look like this:

<a href="/resources/images/the-stack-part-2-environment-overview.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-environment-overview.thumbnail.png" loading="lazy" alt="Overview of Environments" title="Overview of Environments" width="65%" /></a>


And each environment will roughly look like this:

<a href="/resources/images/the-stack-part-2-environment-configuration.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-environment-configuration.thumbnail.png" loading="lazy" alt="Configuration, secrets, and variables of an environment" title="Configuration, secrets, and variables of an environment" width="65%" /></a>


# CDK: Infrastructure as Code
[CDK](https://github.com/aws/aws-cdk) is our tool of choice for Infrastructure as Code. We'll start from the default template and adjust it to use [Bun](https://bun.sh/) which simplifies the process of running CDK commands while using TypeScript.

Instead of setting this up from scratch, start from the template for this step in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments). We'll go through what this contains in the next two sections.

## Explanation: CDK Stack

We structure our CDK stack as follows:
- `deployment/`: The root folder for all things CDK
  - `bin/`: The entry point for all our stacks
    - `deployment.ts`: Our "executable" that CDK run run (defined in `cdk.json`)
    - `helpers.ts`: Helper functions to make our CDK code more readable and safe to run
  - `lib/`: The actual logic of our CDK stacks
    - `base/`: Our base layer containing all the resources that are shared across all stacks
      - `stack.ts`: Gathers all of our base stacks into one stack
      - `domain.ts`: Sets up our Hosted Zone and ACM certificates

This might seem like overkill right now, but will benefit us quite quickly as we start adding more stacks to our project.

In `deployment.ts` you'll see the root of our CDK stack:

```typescript
// ...imports
const app = new cdk.App();

/**
 * Define our base stack that provisions the infrastructure for our application, such
 * as domain names, certificates, and other resources that are shared across all.
 */
const baseStackName = "Base";
if (matchesStack(app, baseStackName)) {
  new BaseStack(app, baseStackName, {
    env: {
      account: process.env.CDK_DEFAULT_ACCOUNT || process.env.AWS_ACCOUNT_ID,
      region: process.env.CDK_DEFAULT_REGION || process.env.AWS_REGION,
    },
    domain: validateEnv("DOMAIN", baseStackName),
  });
}
```

We've set up some conveniences to easily run a single stack, via `matchesStack`, and to validate our environment variables, via `validateEnv`.

Our `BaseStack` is then defined in `lib/base/stack.ts`, and more or less just pieces together the types and the sub-stacks in the `base/` directory:

```typescript
// ...imports
interface StackProps extends cdk.StackProps, domain.StackProps {}

export class Stack extends cdk.Stack {
  constructor(scope: Construct, id: string, props: StackProps) {
    super(scope, id, props);

    // Set up our domain stack.
    new domain.Stack(this, "Domain", props);
  }
}
```

And finally, to the meat of things, our `lib/base/domain.ts` is currently where the magic happens:

```typescript
// ...imports
export interface StackProps extends cdk.StackProps {
  /**
   * The domain name the application is hosted under.
   */
  readonly domain: string;
}

/**
 * Set up a Hosted Zone to manage our domain names.
 *
 * https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_route53.HostedZone.html
 */
export class Stack extends cdk.Stack {
  constructor(scope: Construct, id: string, props: StackProps) {
    super(scope, id, props);

    // Set up the hosted zone.
    const hostedZone = new route53.HostedZone(this, "HostedZone", {
      zoneName: props.domain,
    });

    // Set up an ACM certificate for the domain, and validate it using DNS.
    new acm.Certificate(this, "Certificate", {
      domainName: props.domain,
      validation: acm.CertificateValidation.fromDns(hostedZone),
    });
  }
}
```

This sets up a Hosted Zone and an ACM certificate for our domain, and configures it to validate the Certificate via DNS validation.

## Explanation: Automated Deployments via GitHub Actions

We have two workflows to deploy things, they share much of the same logic, so let's focus on the commonalities first.

Both of them do a few things:
- Sets up a trigger on `workflow_dispatch` so that we can manually trigger the workflow.
- Set up a matrix of environments to deploy to.
- Configures and `environment` and a `concurrency` group. The `concurrency` group is important, since we don't want to deploy to the same environment at the same time.
- Installs `bun` and our dependencies.

All of this looks like this, taken from the `cd-bootstrap` workflow:

```yaml
name: "Deployment: Bootstrap"

on:
  workflow_dispatch:

defaults:
  run:
    shell: bash # Set the default shell to bash.

jobs:
  bootstrap:
    name: bootstrap
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        # Add new environments to this list to run bootstrap on them when manually triggered.
        environment:
          - "Integration Test"
          - "Production Single-tenant"
          - "Production Multi-tenant"
    environment: ${{ matrix.environment }}
    # Limit to only one concurrent deployment per environment.
    concurrency:
      group: ${{ matrix.environment }}
      cancel-in-progress: false
    steps:
      - uses: actions/checkout@v3
      - uses: oven-sh/setup-bun@v1

      - name: Install dependencies
        working-directory: ./deployment
        run: bun install
```

We now diverge (beyond the naming in the above example) for each workflow. The bootstrap flow is quite simple, it just runs `bun run cdk bootstrap` for each environment:

```yaml
      - name: Bootstrap account
        working-directory: ./deployment
        env:
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
        run: bun run cdk bootstrap
```

Our deployment workflow does a bit more, we also synthesize our stacks and run any tests we have:

```yaml
      - name: Synthesize the whole stack
        working-directory: ./deployment
        env:
          DOMAIN: ${{ vars.DOMAIN }}
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
        run: bun run cdk synth --all

      - name: Run tests
        working-directory: ./deployment
        env:
          DOMAIN: ${{ vars.DOMAIN }}
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
        run: bun test

      - name: Deploy to ${{ matrix.environment }}
        working-directory: ./deployment
        env:
          DOMAIN: ${{ vars.DOMAIN }}
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
        run: bun run cdk deploy --all --require-approval never
```

The synth and test steps ensure we have a minimum of sanity checking in place.

## Trigger the Workflows

Push your project to GitHub. You now have access to the workflows and can trigger them manually.

- Trigger the `Deployment: Bootstrap` workflow first, to set up CDK on all accounts.

Before we initiate the deployment, it's recommended to be logged into your Domain Registrar that controls the DNS of your domain, so that you can quickly update your name servers to point to the Hosted Zone that we will be creating. This is necessary to DNS validate our ACM certificates.

Our process will go:
1. Open the DNS settings of your domain registrar
2. Trigger the `Deployment: Deploy to AWS` workflow to start the deployments
3. Log into the target AWS Account and go to the [AWS Console -> Route 53](https://console.aws.amazon.com/route53/) and select **Hosted Zones**
4. Wait for the Hosted Zone to be created, refresh the list, go into the Hosted Zone and copy the name servers, which will look something like this:
    ```
    ns-1234.awsdns-99.co.uk.
    ns-332.awsdns-21.net.
    ns-6821.awsdns-01.org.
    ns-412.awsdns-01.com.
    ```
5. Update the name servers in your domain registrar to point your chosen domain to the Name Servers
6. Repeat step 3., 4., and 5. for each environment

This can easily take 5-15 minutes to complete the first time, depending on how quick you are at setting up your Name Servers.

You can go and see the generated CloudFormation stacks in the [AWS Console -> CloudFormation](https://console.aws.amazon.com/cloudformation/) which will look something like this:

<a href="/resources/images/the-stack-part-2-cloudformation.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-cloudformation.thumbnail.png" loading="lazy" alt="Cloudformation stacks" title="Cloudformation stacks" width="100%" /></a>

We've now set up the foundation for all of our future deployments of applications and services 🥳

## Manual alternative: Setting up CDK

Once you're clone the repo, set up bun:

```bash
$ curl -fsSL https://bun.sh/install | bash
```

Then we can install all of our dependencies for our CDK stack:

```bash
$ cd deployment
$ bun install
```

## Manual alternative: Bootstrapping our Accounts
We’ll be setting up CDK on each of our accounts, so that we can start using it for deployments.

And now we can bootstrap our environment. We'll assume that you have already switched your CLI environment to point to the AWS account that you want to bootstrap:

```bash
# Assuming we are still in the deployment folder
$ bun run cdk bootstrap
```

This is essentially what the [cd-bootstrap](/.github/workflows/cd-bootstrap.yml) workflow does for you, across all the environments you've specified (you can adjust the list in the build matrix).

## Manual alternative: Deployments

Now that we have bootstrapped our accounts, we can deploy our CDK stacks.

Similar to using the Workflow: Before we initiate the deployment, it's recommended to be logged into your Domain Registrar that controls the DNS of your domain, so that you can quickly update your name servers to point to the Hosted Zone that we will be creating. This is necessary to DNS validate our ACM certificates.

Our process will go:
1. Open the DNS settings of your domain registrar
2. Log into the target AWS Account and go to Route 53 -> Hosted Zones
3. Start the deployment
4. Wait for the Hosted Zone to be created, refresh the list, go into the Hosted Zone and copy the name servers
5. Update the name servers in your domain registrar to point your chosen domain to the Name Servers

Assuming you are ready for step 3., we can start the deployment. We'll assume that you are still in the deployment folder and that you have switched your CLI environment to point to the AWS account that you want to deploy to:

```bash
$ DOMAIN="app.example.com" bun run cdk deploy 'Base'
```

The `DOMAIN` environment variable is required here, since we need to know what domain we should use for the Hosted Zone.


# Next Steps

Next up is to start building! Follow along in Part 1 of the series (will be posted soon).
