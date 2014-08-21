# A Docker file to install elnode in a docker, this depends on Nic's existing emacs dockers.
FROM nicferrier/emacsd
MAINTAINER nic@ferrier.me.uk
USER root
ADD Dockerfile-install.el /tmp/Dockerfile-install.el
RUN chown emacs /tmp/Dockerfile-install.el
USER emacs
ENV HOME /home/emacs
RUN /usr/local/emacs/bin/emacs -daemon ; /usr/local/emacs/bin/emacsclient -s /tmp/emacs1000/sock/server -e '(load-file "/tmp/Dockerfile-install.el")'
EXPOSE 8000
CMD /usr/local/emacs/bin/emacs -daemon ; tail -f /dev/null
