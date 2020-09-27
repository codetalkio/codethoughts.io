# Serverless: Rust

## Building
A basic build of the Lambda runtime is possible by following the instructions in the [rust-runtime README](https://github.com/awslabs/aws-lambda-rust-runtime/blob/5d50e1ca29b20fccaf85074a6904fa4b6ece4f05/README.md#aws-cli). In short, we cross-compile a static binary for `x86_64-unknown-linux-musl`, rename the binary to `bootstrap` and zip that up into a `lambda.zip` file. With Custom Runtimes, AWS Lambda looks for an executable called `bootstrap`, so this is why we need the renaming step.

```bash
$ rustup target add x86_64-unknown-linux-musl
$ cargo build --release --target x86_64-unknown-linux-musl --features vendored
$ cp ./target/x86_64-unknown-linux-musl/release/sls-rust ./bootstrap && zip lambda.zip bootstrap && rm bootstrap
```

## Deployment
For real-usage we will deploy using AWS CDK, but you can test it by deploying it via the AWS CLI. We'll do a couple of steps additional steps for the first time setup:

1. Set up a role to use with our Lambda function.
2. Attach policies to that role to be able to actually do something.
3. Deploy the Lambda function using the `lambda.zip` we've built.
4. Invoke the function with a test payload.
5. (Optional) Update the Lambda function with a new `lambda.zip`.


**Set up the IAM Role:**
```bash
$ aws iam create-role --role-name sls-rust-execution --assume-role-policy-document '{"Version": "2012-10-17","Statement": [{ "Effect": "Allow", "Principal": {"Service": "lambda.amazonaws.com"}, "Action": "sts:AssumeRole"}]}'
```

We also need to set some basic policies on the IAM Role for it to be invokeable and for XRay traces to work,
```bash
$ aws iam attach-role-policy --role-name sls-rust-execution --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
$ aws iam attach-role-policy --role-name sls-rust-execution --policy-arn arn:aws:iam::aws:policy/AWSXRayDaemonWriteAccess
```

**Deploy our function:**
```bash
$ aws lambda create-function --function-name sls-rust \
  --handler doesnt.matter \
  --cli-binary-format raw-in-base64-out \
  --zip-file fileb://./lambda.zip \
  --runtime provided \
  --role arn:aws:iam::$(aws sts get-caller-identity | jq -r .Account):role/sls-rust-execution \
  --environment Variables={RUST_BACKTRACE=1} \
  --tracing-config Mode=Active
```

NOTE: You can replace the `$(aws sts get-caller-identity | jq -r .Account)` call with your AWS account ID, if you do not have [jq](https://stedolan.github.io/jq/) installed.

**Invoke our function:**
```bash
$ aws lambda invoke --function-name sls-rust \
  --cli-binary-format raw-in-base64-out \
  --payload '{"firstName": "world"}' \
  output.json > /dev/null && cat output.json && rm output.json
{"message":"Hello, world!"}
```

We can also update the function code again, after creating a new deployment `lambda.zip`,

```bash
$ aws lambda update-function-code \
    --cli-binary-format raw-in-base64-out \
    --function-name  sls-rust \
    --zip-file fileb://lambda.zip
```

After having done this a couple of times, we can gather some traces in XRay to give us an idea of how our Rust application performs in general, and when it hits a cold start. For a simple "Hello, world!" applications, such as the one you can get using `cargo build --package lambda --example hello --release --target x86_64-unknown-linux-musl`, this is an overview of all the Traces after having force a bunch of cold starts:


You can expand the following traces to see how a regular trace looks like in XRay:

<a href="https://codetalk.io/resources/images/serverless-rust-cold-starts-overview.png" target="_blank" rel="noopener noreferrer"><img src="https://codetalk.io/resources/images/serverless-rust-cold-starts-overview.thumbnail.png" loading="lazy" alt="Cold starts overview" title="Cold starts overview" style="margin-right: 1%; width: 49%;" /></a>


## Libraries:
We are using a couple of libraries, in various state of maturity/release:

- The master branch of [aws-lambda-rust-runtime](https://github.com/awslabs/aws-lambda-rust-runtime) pending on [#216](https://github.com/awslabs/aws-lambda-rust-runtime/issues/216) ([README from PR](https://github.com/awslabs/aws-lambda-rust-runtime/blob/5d50e1ca29b20fccaf85074a6904fa4b6ece4f05/README.md)) to be finalised for official async/await support.
  - To statically build you might also need OpenSSL development headers, but we let the [openssl-sys package manage that](https://github.com/sfackler/rust-openssl/issues/980) for us with a `openssl-sys/vendored`.
  - We will need the musl tools, which we use instead of glibc, via `apt-get install musl-tools` for Ubuntu or `brew tap SergioBenitez/osxct && brew install FiloSottile/musl-cross/musl-cross` for macOS.
  - We need zip to create our deployment package, `apt-get install zip`.
-
