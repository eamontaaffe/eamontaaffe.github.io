NAME=eamons-site
ID=master# $(shell uuidgen | tail -c 8 | tr '[:upper:]' '[:lower:]')
PROFILE=personal
REF=$(NAME)-$(ID)
BUCKET="s3://$(REF)/"


create: build stack upload url

update: build upload url

delete: stack-delete


#####
# UPLOAD

url:
	@echo "Url: http://$(REF).s3-website-ap-southeast-2.amazonaws.com"


upload:
	@echo "Uploading website to bucket: $(BUCKET)"
	@aws s3 sync _site/ $(BUCKET) --profile $(PROFILE) > /dev/null


upload-clean:
	@echo "Cleaning out bucket: $(BUCKET)"
	@-aws s3 rm $(BUCKET) --profile $(PROFILE) --recursive > /dev/null


#####
# STACK


stack: stack-create stack-create-wait


stack-clean: stack-delete stack-delete-wait


stack-create:
	@echo "Creating stack: $(REF)"
	@-aws cloudformation create-stack											\
	--stack-name $(REF)																		\
	--profile $(PROFILE)																	\
	--parameters ParameterKey=Name,ParameterValue=$(REF)	\
	--template-body file://cloudformation.yaml						\
	--capabilities CAPABILITY_IAM > /dev/null 2> /dev/null


stack-create-wait:
	@echo "Waiting for stack create to complete: $(REF)"
	@-aws cloudformation wait stack-create-complete \
	--stack-name $(REF)															\
	--profile $(PROFILE) > /dev/null 2> /dev/null


stack-delete: upload-clean
	@echo "Deleting stack: $(REF)"
	@-aws cloudformation delete-stack \
	--stack-name $(REF)								\
	--profile $(PROFILE) > /dev/null 2> /dev/null


stack-delete-wait:
	@echo "Waiting for stack delete to complete: $(REF)"
	@-aws cloudformation wait stack-delete-complete \
	--stack-name $(REF)															\
	--profile $(PROFILE) > /dev/null 2> /dev/null


#####
# BUILD

build:
	@echo "Building project"
	@-stack build > /dev/null 2> /dev/null
	@-stack exec site-exe rebuild > /dev/null 2> /dev/null


build-clean:
	@echo "Cleaning project"
	@-stack clean > /dev/null 2> /dev/null
