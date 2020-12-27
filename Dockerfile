# Run guile-gi in a container
#
# docker run -d \
#  -v /etc/localtime:/etc/localtime:ro \
#  -v /tmp/.X11-unix:/tmp/.X11-unix \
#  -e DISPLAY=unix$DISPLAY \
#  --device /dev/snd:/dev/snd \
#  --name guile-gi \
#  spk121/guile-gi

FROM ubuntu:latest
LABEL maintainer "Mike Gran <spk121@yahoo.com>"

WORKDIR /app

COPY . /app

ENV LANG C.UTF-8
ENV GUILD /usr/bin/guild
ENV TERM dumb
ENV VERBOSE true
ENV DEBIAN_FRONTEND noninteractive
ENV TZ America/Los_Angeles

RUN apt-get -y update
RUN apt-get install -y tzdata
RUN apt-get install -y libffi-dev gir1.2-glib-2.0 libgirepository1.0-dev \
  guile-3.0-dev libtool texinfo autoconf automake gnulib git \
  texlive libgtk-3-dev x11-apps xterm
RUN autoreconf -vif -Wall
RUN ./configure --enable-hardening --prefix=/usr && make && make install
# RUN useradd --create-home --home-dir $HOME guile-gi \
#    && gpasswd -a guile-gi audio \
#    && chown -R 
ENV DISPLAY :0

USER user
ENTRYPOINT ["/bin/sh", "-c", "$0 \"$@\"", "xterm", "-e", "guile"]
