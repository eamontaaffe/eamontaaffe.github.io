---
layout: post
---

I want to deploy [this site](https://github.com/eamontaaffe/eamontaaffe.github.io). The website itself is statically generated with no javascript or anything fancy. I would like to make use of server side analytics to get a rough idea of the number of visitors to the site without needing to display a cookie notice to our European friends. I obviously don't want to spend much money and it would be nice if we could enable multiple region support so it isnt slow for international guests.

To do this we will make use of [AWS S3](https://aws.amazon.com/s3/) as a [static host](https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html) and use [AWS Cloudwatch Logs](https://aws/amazon.com/cloudwatch/) to handle our [analytics](https://docs.aws.amazon.com/AmazonS3/latest/dev/LoggingWebsiteTraffic.html). Also, I wan't to use [Terraform](https://www.terraform.io/) so that we have our [infrastructure as code](https://www.thoughtworks.com/insights/blog/infrastructure-code-reason-smile).

Lets get started. First we need to setup our [AWS provider](https://www.terraform.io/docs/providers/aws/) in Terraform.

```terraform
variable "region" {}

provider "aws" {
    region = "${var.region}"
}
```

Now we can create an S3 bucket which will contain our html and css files.

```terraform
variable "domain" {}

resource "aws_s3_bucket" "website" {
    bucket = "${var.domain}"
    acl = "public-read"
    
    website {
        index_document = "index.html"
    }
}
```
