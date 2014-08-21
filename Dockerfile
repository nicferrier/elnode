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
ENV HOME /home/emacs
RUN /usr/local/emacs/bin/emacs -daemon ; /usr/local/emacs/bin/emacsclient -s /tmp/emacs1000/sock/server -e '(load-file "/tmp/Dockerfile-install.el")'
EXPOSE 8000
CMD /usr/local/emacs/bin/emacs -daemon ; tail -f /dev/null
