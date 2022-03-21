deploy-locally:
	./deploy.zsh 

deploy-locally-no-mint:
	./deploy.zsh local 10 staging false

deploy-staging-ic:
	./deploy.zsh ic

deploy-staging-ic-full:
	./deploy.zsh ic 2015

deploy-production-ic-full-no-mint:
	./deploy.zsh ic 2015 production false