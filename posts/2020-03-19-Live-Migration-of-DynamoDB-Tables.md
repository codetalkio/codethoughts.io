---
title: Live Migration of DynamoDB Tables
tags: aws, dynamodb, sqs, migration
date: 2020-03-19
---

Recently I was faced with the challenge of having to migrate a set of AWS DynamoDB tables to completely new tables. We wanted to achieve this without affecting any of our users, and without having a maintenance window while migrating data from the old table to the new ones.

The following will be a very high-level overview of how you:

- Get all your DynamoDB events onto a queue
- Replicate your DynamoDB tables to the new tables (or even a different region)
- Continuously synchronize your original tables to your new tables
    - Restart the migration if you made an error
- Complete the switchover after validating everything looks OK

<div></div><!--more-->

<div class="callout">
  <div class="callout-bulb">💡</div>
  This won't be detailed walkthrough but will instead outline a method for people that are in the same situation we were.
</div>

## Context
To put this migration into context, our entire cloud and setup are automated via CloudFormation, deploying many micro-services.

To better orchestrate the deployment of these and have better handling of dependencies between the order of deployments, we had been working on a lot of infrastructure changes, switching around the tools that manage CloudFormation (from [serverless](https://serverless.com) to [AWS CDK](https://github.com/aws/aws-cdk){target="_blank" rel="noopener noreferrer"}).

Internally these tools generate unique references (logical ids), each in their own way, to the resources they generate CloudFormation for. This meant that we needed a way to change the logical ids of a resource for us to have the new tools adopt and manage the existing resources.

Unfortunately, this is not possible for DynamoDB tables^[As mentioned in [serverless#1677](https://github.com/serverless/serverless/issues/1677){target="_blank" rel="noopener noreferrer"}, this is not supported.], without recreating the resources ☹️

## Approach
The idea came after AWS announced support for importing existing resources into a CloudFormation stack^[[https://aws.amazon.com/blogs/aws/new-import-existing-resources-into-a-cloudformation-stack/](https://aws.amazon.com/blogs/aws/new-import-existing-resources-into-a-cloudformation-stack/){target="_blank" rel="noopener noreferrer"}.], and was further motivated by the later support for restoring a DynamoDB table to another region^[[https://aws.amazon.com/blogs/database/restore-amazon-dynamodb-backups-to-different-aws-regions-and-with-custom-table-settings/](https://aws.amazon.com/blogs/database/restore-amazon-dynamodb-backups-to-different-aws-regions-and-with-custom-table-settings/){target="_blank" rel="noopener noreferrer"}.].

The concept is simple and can be divided into two phases.

**Phase 1**

In the first phase, we will:

- Enable streams on all the DynamoDB tables you intend to migrate
- Set up a Lambda function that will receive the events from these streams
- Set up a FIFO SQS queue which the Lambda function will put all of the events on

<a href="/resources/images/dynamodb-migration-phase-1.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/dynamodb-migration-phase-1.thumbnail.png" loading="lazy" alt="Migration Phase 1" title="Migration Phase 1" width="100%" /></a>

After this is set up, all DynamoDB events will exist on the SQS queue. We will now create a backup for each of the tables. The time of the backups will be important later on. You don't have to note them down, since they are visible in the table backup overview.

**Phase 2**

For phase 2, we are focusing on setting up the new tables and starting the live synchronization process.

We will now:

- Restore the tables from a backup into to your new tables
    - This is where you can rename the tables, put them in different regions, etc
- Set up a Lambda Function that consumes from you SQS queue
  - The consumer Lambda should only act on events that have happened on or after your backup time

<a href="/resources/images/dynamodb-migration-phase-2.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/dynamodb-migration-phase-2.thumbnail.png" loading="lazy" alt="Migration Phase 2" title="Migration Phase 2" width="100%" /></a>

Since DynamoDB stream events contain all the information about a  record, we can break them into the following:

- `CREATE` and `MODIFY` is directly `put` the record into DynamoDB.
- For `DELETE`s, we'll perform a `delete` on the record (we have the keys from the old record image).

You now have your old table synchronized into your table continuously! 🎉

### Fixing a Mistake in the Process
I mentioned it was possible to restart the migration. The great thing about this setup is that you can simply tear down the new tables, if you've made a mistake during the process.

Since your SQS queue is FIFO, you simply choose the point in time of the queue that you need to process data from and forward. Everything before this point in time is simply dropped.

This allows you to try several times to get it right and removes the stress and potential for errors when migrating inside a tight maintenance window.

### Finalizing the Migration
When you are confident that your data is looking correct, you are ready to cut-off the updates to the SQS. This step might vary slightly depending on your setup and what updates your tables, so I'll take you through our specific case.

Our frontend uses our tables. This means that user interactions drive the updates/creates/deletes. This also means that if the API no longer points to the old DynamoDB tables, the changes will stop.

When we felt comfortable that the new tables were correct, and had finalized the setup, we updated our API to point to the new tables. Since our Lambda had been consuming the SQS all of this time, it will either be empty or have very few events in the queue. We could handle a few events being slightly delayed since the user's risk was non-existent and would be corrected within seconds after the SQS queue was empty.

Pointing the API to the new DynamoDB tables means that no more operations are done on the old tables, and thus no further events are put on the SQS. This had the effect that there was no rush to clean up the two Lambdas and SQS queue since they would simply not be performing any actions anymore.

### Utilizing this approach for data transformation
We've only talked about migrating tables, without really touching the data. Still, this approach allows you to perform a data transformation along with your migration without needing to coordinate a complicated API switch or support both formats of data simultaneously.

You could do the following instead:

- Follow phase 1 as before
- Phase 2 looks a bit different now
    - Create the new tables from the backups again
    - Run your script to transform all the data that is in the newly created tables
    - Your consumer Lambda is now a bit more complicated. You will need to add the same transformation step for each record that it receives, before `put`'ing it into the new table

This way, you perform live transformations on your data and have ample time to see everything looks as intended in the new tables, before switching over.


## Conclusion
While very high-level, this approach is a great tool to have in your toolbox when you run into those once-in-a-while cases where you need to perform these migrations.

If you are interested in me expanding more on the approach, please don't hesitate to leave a comment 🙂
