# A Docker file to install elnode in a docker, this depends on Nic's existing emacs dockers.
FROM nicferrier/emacsd
MAINTAINER nic@ferrier.me.uk
# This is a stupid docker bug - you can't ADD tar files to the docker because it will unpack them
# So if you want to install a locally produced package you have to do all this
USER root
#RUN apt-get update
#RUN apt-get -y install curl
ADD Dockerfile-install.el /tmp/Dockerfile-install.el
RUN chown emacs /tmp/Dockerfile-install.el
USER emacs
#RUN curl -s http://172.17.42.1:8016/elnode-0.9.9.8.1.tar -o /tmp/elnode-0.9.9.8.1.tar
ENV HOME /home/emacs
ENV AUTH /home/emacs/.emacs.d/server/server
RUN /usr/local/emacs/bin/emacs -daemon ; /usr/local/emacs/bin/emacsclient -f $AUTH -e '(load-file "/tmp/Dockerfile-install.el")'
EXPOSE 8000
CMD /usr/local/emacs/bin/emacs -daemon ; tail -f /dev/null
