DOMAIN=taaffe.com.au
PROFILE=personal
REF=EamonsSiteStack
BUCKET_PRIMARY="s3://$(DOMAIN)/"
BUCKET_SECONDARY="s3://www.$(DOMAIN)/"


create: build stack upload url

update: build upload url

update-stack: stack-update stack-update-wait

delete: stack-delete


#####
# UPLOAD

url:
	@echo "Url: http://$(DOMAIN).s3-website-ap-southeast-2.amazonaws.com"


upload:
	@echo "Uploading website to bucket: $(BUCKET_PRIMARY)"
	@aws s3 sync _site/ $(BUCKET_PRIMARY) --profile $(PROFILE)


upload-clean:
	@echo "Cleaning out bucket: $(BUCKET_PRIMARY)"
	@-aws s3 rm $(BUCKET_PRIMARY) --profile $(PROFILE) --recursive


#####
# STACK


stack: stack-create stack-create-wait


stack-clean: stack-delete stack-delete-wait


stack-create:
	@echo "Creating stack: $(REF)"
	@-aws cloudformation create-stack													\
	--stack-name $(REF)																				\
	--profile $(PROFILE)																			\
	--parameters ParameterKey=Domain,ParameterValue=$(DOMAIN)	\
	--template-body file://cloudformation.yaml								\
	--capabilities CAPABILITY_IAM


stack-create-wait:
	@echo "Waiting for stack create to complete: $(REF)"
	@-aws cloudformation wait stack-create-complete						\
	--stack-name $(REF)																				\
	--profile $(PROFILE)

stack-update:
	@echo "Updating stack: $(REF)"
	@-aws cloudformation update-stack													\
	--stack-name $(REF)																				\
	--profile $(PROFILE)																			\
	--parameters ParameterKey=Domain,ParameterValue=$(DOMAIN)	\
	--template-body file://cloudformation.yaml								\
	--capabilities CAPABILITY_IAM


stack-update-wait:
	@echo "Waiting for stack update to complete: $(REF)"
	@-aws cloudformation wait stack-update-complete						\
	--stack-name $(REF)																				\
	--profile $(PROFILE)


stack-delete: upload-clean
	@echo "Deleting stack: $(REF)"
	@-aws cloudformation delete-stack													\
	--stack-name $(REF)																				\
	--profile $(PROFILE)


stack-delete-wait:
	@echo "Waiting for stack delete to complete: $(REF)"
	@-aws cloudformation wait stack-delete-complete						\
	--stack-name $(REF)																				\
	--profile $(PROFILE)


#####
# BUILD

build:
	@echo "Building project"
	@-stack build
	@-stack exec site-exe rebuild


build-clean:
	@echo "Cleaning project"
	@-stack clean
