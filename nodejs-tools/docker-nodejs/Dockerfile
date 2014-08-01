FROM nicferrier/elnode
MAINTAINER nic@ferrier.me.uk
USER root
ADD node-install /tmp/node-install
RUN apt-get update
RUN apt-get install -y git git-core curl
RUN bash /tmp/node-install
RUN apt-get install -y nodejs
