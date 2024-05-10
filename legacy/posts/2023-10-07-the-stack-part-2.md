---
title: "The Stack Part 2: Automating Deployments via CI"
tags: aws, cloud, infrastructure, cdk, ci
date: 2023-10-07
---

In [the last post](/posts/2023-10-07-the-stack-part-1.html) we created our Control Tower structure with all of our AWS Accounts in it. In this post we will be automating our deployment process for each of these environments. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

At the end of this post we will have:

- A workflow for bootstrapping our AWS Accounts for CDK ([see here](https://github.com/codetalkio/the-stack/blob/part-2-automatic-deployments/.github/workflows/cd-bootstrap.yml)).
- A workflow for deploying our CDK stacks, including synthesizing and testing before ([see here](https://github.com/codetalkio/the-stack/blob/part-2-automatic-deployments/.github/workflows/cd-deploy.yml)).
- Set up automatic staggered deployments when changes are merged to our `main` branch.
- And fallback to manual deployments if we need to.

If you want to jump straight to the code, you can find it in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments) which links directly to the Part 2 branch.

Otherwise, let's jump in!

<div></div><!--more-->

- [AWS: Seting up Credentials](#aws-seting-up-credentials)
- [GitHub: Setting up Environments](#github-setting-up-environments)
- [CDK: Infrastructure as Code](#cdk-infrastructure-as-code)
- [Automated Deployments via GitHub Actions](#automated-deployments-via-github-actions)
  - [Boostrap Workflow](#boostrap-workflow)
  - [Deployment Workflow](#deployment-workflow)
  - [Trigger the Workflows](#trigger-the-workflows)
- [Manual alternative: Bootstrapping our Accounts](#manual-alternative-bootstrapping-our-accounts)
- [Manual alternative: Deployments](#manual-alternative-deployments)
- [Bonus: Justfile and just](#bonus-justfile-and-just)
- [Next Steps](#next-steps)


## AWS: Seting up Credentials

We will need to set up credentials for our GitHub Actions to be able to deploy to our AWS Accounts. For now, we will focus on the following of our accounts as deployment targets:

- Integration Test
- Production Single-tenant
- Production Multi-tenant

For each of these we will need to set up IAM credentials that we can use in GitHub Actions to deploy our resources into each of these accounts.

<div class="callout">
  <div class="callout-bulb">💡</div>
  GitHub also supports authenticating to AWS via OpenID. Check out their docs on that [here](https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/configuring-openid-connect-in-amazon-web-services).
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


## GitHub: Setting up Environments
For our GitHub Actions workflows to work, we need to set up our `Environment`s configure a couple of `Environment` variables and secrets.

1. Go to your repository Settings -> Environments
2. Create your environments e.g. `Integration Test`
3. [Recommended] Restrict deployments to the `main` branch
4. Set up the secrets for
    - `AWS_ACCESS_KEY_ID`
    - `AWS_SECRET_ACCESS_KEY`
1. Set up the variables for
    -  `AWS_ACCOUNT_ID`
    -  `AWS_REGION`
    -  `DOMAIN` (where your app will live, e.g. `app.example.com`. `integration.example.com`, and `single.example.com`)

Repeat those steps with the relevant values for each of the environments `Integration Test`, `Production Single`, `Production Multi`.

Your `Environment` overview will look like this:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-environment-overview.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-environment-overview.png" loading="lazy" alt="Overview of Environments" title="Overview of Environments" width="75%" /></a>
</div>

And each environment will roughly look like this:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-environment-configuration.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-environment-configuration.png" loading="lazy" alt="Configuration, secrets, and variables of an environment" title="Configuration, secrets, and variables of an environment" width="75%" /></a>
</div>

## CDK: Infrastructure as Code

[CDK](https://github.com/aws/aws-cdk) is our tool of choice for Infrastructure as Code. We'll start from the default template and adjust it to use [Bun](https://bun.sh/) which simplifies the process of running CDK commands while using TypeScript.

Instead of setting this up from scratch, start from the template for this step in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments). We'll go through what this contains in the next two sections.

Our overall aim is to structure our CDK into three main groupings:

- `Global`: "Global" (often `us-east-1`) specific things such as ACM Certificates for CloudFront, and we'll also put Hosted Zones here
- `Cloud`: Region specific infrequently changing things such as VPC, Region-specific Certificates, etc
- `Platform`: DynamoDB, Cache, SQS
- `Services`: Lambdas, API Gateway, etc

This split is based on the frequency something changes, and allows us to deploy freqeuently changing stacks without having to also look at things that very rarely change. In this part of the series we will set up the `Global` stack.

As you can see in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments), we structure our CDK stack as follows:

- `deployment/`: The root folder for all things CDK
  - `bin/`: The entry point for all our stacks
    - `deployment.ts`: Our "executable" that CDK run run (defined in `cdk.json`)
    - `helpers.ts`: Helper functions to make our CDK code more readable and safe to run
  - `lib/`: The actual logic of our CDK stacks
    - `global/`: Our 'Global' layer containing all the resources that are shared across all stacks
      - `stack.ts`: Gathers all of our 'Global' stacks into one stack
      - `domain.ts`: Sets up our Hosted Zone and ACM certificates

This might seem like overkill right now, but will benefit us quite quickly as we start adding more stacks to our project.

In `deployment.ts` you'll see the root of our CDK stack. This is where we will define the three layers we mentioned earlier, `Global`, `Cloud`, `Platform`, and `Services`. In CDK terminilogy these are called `Stack`s.

For now, we will only define the `Global` layer:

```typescript
// ...imports
const app = new cdk.App();

/**
 * Define our 'Global' stack that provisions the infrastructure for our application, such
 * as domain names, certificates, and other resources that are shared across all regions.
 *
 * ```bash
 * bun run cdk deploy --concurrency 6 'Global/**'
 * ```
 */
const globalStackName = "Global";
if (matchesStack(app, globalStackName)) {
  // Some of our global resources need to live in us-east-1 (e.g. CloudFront certificates),
  // so we set that as the region for all global resources.
  new GlobalStack(app, globalStackName, {
    env: {
      account: process.env.AWS_ACCOUNT_ID || process.env.CDK_DEFAULT_ACCOUNT,
      region: "us-east-1",
    },
    domain: validateEnv("DOMAIN", globalStackName)
  });
}
```

We've set up some conveniences to easily run a single stack, via `matchesStack`, and to validate our environment variables, via `validateEnv`.

Our `GlobalStack` is then defined in `lib/global/stack.ts`, and more or less just pieces together the types and the sub-stacks in the `global/` directory.

The interesting bit here is the call to `new domain.Stack` which is what actually kicks off the provisioning of resources, which are defined inside the `lib/global/domain.ts` file on layer deeper:

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

And finally we get to the interesting part of it all in `lib/global/domain.ts`. This is the first place we are actually defining resources that will be deployed to AWS, by calling the CDK `Construct`s that are available to us. `Construct` is the CDK terminology for the actual resources we create, i.e. our building blocks.

We create our Hosted Zone via `new route53.HostedZone` and our ACM certificate via `new acm.Certificate`. You can find out more about each of these in the CDK docs:

- [Route 53](https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_route53.HostedZone.html)
- [ACM](https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_certificatemanager-readme.html)

Let's get our resources defined:

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

    // Set up an ACM certificate for the domain + subdomains, and validate it using DNS.
    new acm.Certificate(this, "Certificate", {
      domainName: props.domain,
      subjectAlternativeNames: [`*.${props.domain}`],
      validation: acm.CertificateValidation.fromDns(hostedZone),
    });
  }
}
```

This sets up a Hosted Zone and an ACM certificate for our domain, and configures it to validate the Certificate via DNS validation.

## Automated Deployments via GitHub Actions

As you can see in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments), we have two workflows to deploy things. They share much of the same logic, so let's focus on the commonalities first.

Both of them do a few things:

- Sets up a trigger on `workflow_dispatch` so that we can manually trigger the workflow.
- Set up a matrix of environments to deploy to.
- Configures and `environment` and a `concurrency` group. The `concurrency` group is important, since we don't want to deploy to the same environment at the same time.
- Installs `bun` and our dependencies.

The GitHub Actions YAML might feel a bit verbose, so let's break it down a bit. We'll first focus on `cd-bootstrap` which Bootstraps AWS Accounts for CDK.

### Boostrap Workflow

We first define our name and the trigger. Because we only want this to be triggered manually (bootstrapping just needs to run once) we can use `workflow_dispatch` which allows us to trigger it from the GitHub Actions UI ([docs here](https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#workflow_dispatch)):

```yaml
name: "Deployment: Bootstrap"

on:
  workflow_dispatch:
# ...
```

With that in place, let's take a look at the logic we are running.

A neat way to "do the same thing" over a set of different things is to utilize the `matrix` feature of GitHub Actions ([docs here](https://docs.github.com/en/actions/using-jobs/using-a-matrix-for-your-jobs)). We can define a list of `environments`s ([docs here](https://docs.github.com/en/actions/deployment/targeting-different-environments/using-environments-for-deployment)) to run our workflow on, and then use that list to run the same steps for each environment.

This is what the `strategy` section does, and it then feeds this into the `environment` which tells GitHub Actions which environment variables and secrets are available, as well as automatically tracks our deployments in the GitHub UI:

```yaml
# ...
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
    # ...
```

Now, what would happen if we ran this multiple times in parallel on the same environment? Probably not something we'd like to find out.

To prevent this, we can tell GitHub to only allow one job to run at a time, given a group identifier. We do this by adding a `concurrency` control to our workflow ([docs here](https://docs.github.com/en/actions/using-jobs/using-concurrency)):

```yaml
    # ...
    # Limit to only one concurrent deployment per environment.
    concurrency:
      group: ${{ matrix.environment }}
      cancel-in-progress: false
    # ...
```

And finally, we get to the actual steps of logic we are performing. First we'll checkout our code, set up bun, and then use bun to install our dependencies:

```yaml
    # ...
    steps:
      - uses: actions/checkout@v3
      - uses: oven-sh/setup-bun@v1

      - name: Install dependencies
        working-directory: ./deployment
        run: bun install
      # ...
```

Now we're ready to bootstrap! We use the variables and secrets we defined previously. Since we told GitHub which environment we are running in, it will automatically know where to pull this from. This saves us the headache of weird hacks such as `AWS_ACCESS_KEY_ID_FOR_INTEGRATION_TEST` or something similar.

We pull in what we need, and then run the `cdk bootstrap` command via bun:

```yaml
      # ...
      - name: Bootstrap account
        working-directory: ./deployment
        env:
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
        run: bun run cdk bootstrap
```

### Deployment Workflow

Our deployment flow gets a bit more complicated. We're building for the future here, so we want to put in place a few checks to make sure we don't accidentally deploy something broken. Or, if we do, we can at least stop early before it affects our users.

We will be building up the following flow, illustrated in the diagram below:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-deployment-flow.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-deployment-flow.png" loading="lazy" alt="Deployment flow" title="Deployment flow" width="100%" /></a>
</div>

This is what is called a "staggered deployment", but across our environments:

1. We roll out to our `Integration Test` environment in **Stage 1**.
   - We validate that our IaC actually works.
2. Once the deployment is done, we perform checks against it to validate the health of the deployment (e.g. End-to-End tests, smoke tests, etc)
   - We validate that our application actually works with our changes to our infrastructure.
4. If everything looks good, we proceed to **Stage 2** which deploys both our `Production Single-tenant` and `Production Multi-tenant` environments in parallel.
5. We do a final check that all is good, and then we're done!

This helps us build confidence that our deployments work, since our aim is to deploy changes immediately as they are merged into our `main` branch.

**Part 1: Structuring the deployment flow**

Let's take a look at how we do this. First, we'll set up our triggers. We want to both allow manually triggering a deployment, again via `workflow_dispatch`, but we also want to immediately deploy when changes are merged to our `main` branch:

```yaml
name: "Deployment: Deploy to AWS"

on:
  workflow_dispatch:
  push:
    branches:
      - main
# ...
```

All good and well, so let's start defining our jobs. Ignore the `uses` for now, that's a reuseable workflow which we'll get back to later in **Part 2** of this section:

```yaml
# ...
jobs:
  # Stage 1 tests that the deployment is working correctly.
  stage-1:
    name: "Stage 1"
    uses: ./.github/workflows/wf-deploy.yml
    with:
      environment: "Integration Test"
    secrets: inherit
  # ...
```

We first initiate our **Stage 1** deployment, specifying that it's the `Integration Test` environment. We also allow the the reuseable workflow (defined in `wf-deploy.yml`) to inherit all secrets from the caller.

Next, we want to run our checks, but only after our **Stage 1** job has finished running successfully. To do this we use `needs` to define a dependency on a previous job ([docs here](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idneeds)).

```yaml
  # ...
  # Run tests against the environments from Stage 1.
  stage-1-check:
    name: "Stage 1: Check"
    needs: [stage-1]
    runs-on: ubuntu-latest
    steps:
      - name: "Check"
        run: |
          echo "Checking 'Integration Test'..."
          # Run tests against the environment...
          # Or alert/rollback if anything is wrong.
  # ...
```

We aren't doing much interesting for now in our test job, since we are only deploying a Domain, but this will be helpful later on when we start setting up our Frontend and APIs.

Similarly, we use `needs` again to specify how we move into **Stage 2**. We first set up `Production Single-tenant`:

```yaml
  # ...
  # Stage 2 is our more critical environments, and only proceeds if prior stages succeed.
  stage-2-single:
    name: "Stage 2: Single-tenant"
    needs: [stage-1-check]
    uses: ./.github/workflows/wf-deploy.yml
    with:
      environment: "Production Single-tenant"
    secrets: inherit

  stage-2-single-check:
    name: "Stage 2: Check Single-tenant"
    needs: [stage-2-single]
    runs-on: ubuntu-latest
    steps:
      - name: "Check"
        run: |
          echo "Checking 'Production Single-tenant'..."
          # Run tests against the environment...
          # Or alert/rollback if anything is wrong.
  # ...
```

And do the same for our `Production Multi-tenant` environment:

```yaml
  # ...
  stage-2-multi:
    name: "Stage 2: Multi-tenant"
    needs: [stage-1-check]
    uses: ./.github/workflows/wf-deploy.yml
    with:
      environment: "Production Multi-tenant"
    secrets: inherit

  stage-2-multi-check:
    name: "Stage 2: Check Multi-tenant"
    needs: [stage-2-multi]
    runs-on: ubuntu-latest
    steps:
      - name: "Check"
        run: |
          echo "Checking 'Production Multi-tenant'..."
          # Run tests against the environment...
          # Or alert/rollback if anything is wrong.
  # ...
```

We could have been using using build `matrix`'s again, but that would mean that the checks for **Stage 2** would only proceed after *both* of the Jobs completed. We would prefer that they check *immediately* once the deployment is done, so instead we split up these two into their own Jobs manually.

Voila 🎉 We've now set the skeleton for the deployment flow we pictured earlier:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-deployment-flow.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-deployment-flow.png" loading="lazy" alt="Deployment flow" title="Deployment flow" width="100%" /></a>
</div>

**Part 2: Reuseable workflow**

To better reuse logic that is identical from different jobs or even different workflows and repositories, GitHub supports making workflows reuseable ([docs here](https://docs.github.com/en/actions/using-workflows/reusing-workflows)).

We've done this in our `wf-deploy.yml` workflow, which we use in our `stage-1`, `stage-2-single`, and `stage-2-multi` jobs. Let's take a look at what it does.

First, we will need to define which inputs this workflow takes. Remember the `with` and `secrets` that we used earlier? That's how we pass information to our resuseable workflow. We define these in the `inputs` section:

```yaml
name: Deploy

on:
  workflow_call:
    inputs:
      environment:
        required: true
        type: string
# ...
```

Here we simply specify that we require an `environment` to be passed along. We will automatically inherit the `secrets`, but we would otherwise specify those explicitly as well.

We can now proceed to the logic, which resembles the `cd-bootstrap` workflow we looked at earlier. We first set up our environment, concurrency group, and then install our dependencies:

```yaml
# ...
jobs:
  deploy:
    name: "Deploy"
    runs-on: ubuntu-latest
    environment: ${{ inputs.environment }}
    # Limit to only one concurrent deployment per environment.
    concurrency:
      group: ${{ inputs.environment }}
      cancel-in-progress: false
    steps:
      - uses: actions/checkout@v3
      - uses: oven-sh/setup-bun@v1

      - name: Install dependencies
        working-directory: ./deployment
        run: bun install
      # ...
```

Before we proceed to actually deploying anything, we want to sanity check that our deployment looks valid. We do this by trying to first synthesize the whole deployment ([some info on synth here](https://docs.aws.amazon.com/cdk/v2/guide/hello_world.html)), and then run whatever test suite we have:

```yaml
      # ...
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
      # ...
```

Everything should now be good, so let's run our actual deployment:

```yaml
      # ...
      - name: Deploy to ${{ inputs.environment }}
        working-directory: ./deployment
        env:
          DOMAIN: ${{ vars.DOMAIN }}
          AWS_REGION: ${{ vars.AWS_REGION }}
          AWS_DEFAULT_REGION: ${{ vars.AWS_REGION }}
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
        run: bun run cdk deploy --concurrency 4 --all --require-approval never
```

And there we go! We've now automated our deployment flow, and no longer have to worry about manually deploying things to our environments.

### Trigger the Workflows

Push your project to GitHub. You now have access to the workflows and can trigger them manually.

If you haven't done it already, let's run the `Deployment: Bootstrap` workflow first, to set up CDK on all accounts. Alternatively, jump to the section [Manual alternative: Bootstrapping our Accounts](#manual-alternative-bootstrapping-our-accounts) for how to do this manually.

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-trigger-bootstrap-workflow.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-trigger-bootstrap-workflow.thumbnail.png" loading="lazy" alt="Manually trigger the bootstrap workflow" title="Manually trigger the bootstrap workflow" width="80%" /></a>
</div>

Next up, before we initiate the deployment it's recommended to be logged into your Domain Registrar that controls the DNS of your domain, so that you can quickly update your name servers to point to the Hosted Zone that we will be creating. This is necessary to DNS validate our ACM certificates.

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

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-2-cloudformation.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-2-cloudformation.thumbnail.png" loading="lazy" alt="Cloudformation stacks" title="Cloudformation stacks" width="85%" /></a>
</div>

We've now set up the foundation for all of our future deployments of applications and services 🥳

## Manual alternative: Bootstrapping our Accounts

Once you've cloned the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-2-automatic-deployments) (or made your own version of it), set up bun:

```bash
$ curl -fsSL https://bun.sh/install | bash
```

Then we can install all of our dependencies for our CDK stack:

```bash
$ cd deployment
$ bun install
```

We’ll be setting up CDK on each of our accounts, so that we can start using it for deployments.

Assuming that you have already switched your CLI environment to point to the AWS account that you want to bootstrap:

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
$ DOMAIN="app.example.com" bun run cdk deploy --concurrency 4 'Cloud' 'Cloud/**'
```

The `DOMAIN` environment variable is required here, since we need to know what domain we should use for the Hosted Zone.


## Bonus: Justfile and just

It might seem overkill right now, but we will eventually have many different commands across many folder locations in our mono-repo setup. To make this a bit easier to work with, we can use the tool [Just](https://github.com/casey/just) to help us out.

From `just`'s README:

> `just` is a handy way to save and run project-specific commands

From [the installation instructions](https://github.com/casey/just#packages) we can install it via:

```bash
# macOS:
$ brew install just
# Linux with prebuilt-mpr (https://docs.makedeb.org/prebuilt-mpr/getting-started/#setting-up-the-repository):
$ sudo apt install just
# Prebuilt binaries (assuming $HOME/.local/bin is in your $PATH):
$ curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to $HOME/.local/bin
```

This allows us to set up a `justfile` in the root of our project, which we can then use to define shortcuts to our commands. For example, we can define a shortcut to run our CDK commands:

```makefile
# Display help information.
help:
  @ just --list

# Setup dependencies and tooling for <project>, e.g. `just setup deployment`.
setup project:
  just _setup-{{project}}

_setup-deployment:
  #!/usr/bin/env bash
  set -euxo pipefail
  cd deployment
  bun install

# Deploy the specified <stack>, e.g. `just deploy 'Global/**'`, defaulting to --all.
deploy stack='--all':
  #!/usr/bin/env bash
  set -euxo pipefail
  cd deployment
  bun run cdk deploy --concurrency 4 --require-approval never {{ if stack == "--all" { "--all" } else { stack } }}

# Run tests for <project>, e.g. `just test deployment`.
test project:
  just _test-{{project}}

_test-deployment:
  #!/usr/bin/env bash
  set -euxo pipefail
  cd deployment
  bun test

_test-synth:
  #!/usr/bin/env bash
  set -euxo pipefail
  cd deployment
  bun run cdk synth --all
```

We can now run our commands via `just`:

```bash
# Setup our dependencies:
$ just setup deployment
# Run tests:
$ just test deployment
# Synthesize our CDK stack:
$ just test synth
# Deploy our CDK stack:
$ just deploy # or just deploy Global
```



## Next Steps

Next up is to add our first Frontend! Follow along in [Part 3 of the series](/posts/2023-10-16-the-stack-part-3.html).
