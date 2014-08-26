# This is only for building the elnode docker
docker:=sudo docker

elnode: build
	$(docker) push nicferrier/elnode

build:
	$(docker) build --no-cache=true -t nicferrier/elnode .

# End
