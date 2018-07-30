# Aramaic
"Serverless" Event Driven Blog Engine

[![Build Status](https://travis-ci.org/pstephens/aramaic.svg?branch=master)](https://travis-ci.org/pstephens/aramaic)

## **EARLY DAYS**

## Basic Design
Functions:
1. Render Page f(template, markdown, comments) -> Rendered HTML
2. Render Main Index
3. Render Year/Month Index
4. Source Object Updated


* SQS Queues to glue together
* Cloudwatch Cron events for timed publishing
* CloudFront for CDN
* S3 for raw + transformed object storage
* Certificate Manager for TLS Certs
* Route53 for DNS
* CloudFormation for provisioning

## Page Design

## Metadata Cache
