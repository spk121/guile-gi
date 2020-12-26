FROM ubuntu:latest

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
  texlive
RUN apt-get install -y x11-apps xterm
RUN autoreconf -vif -Wall
RUN ./configure --enable-hardening --prefix=/usr && make && make install
RUN useradd -ms /bin/bash user
ENV DISPLAY :0

USER user
ENTRYPOINT ["/bin/sh", "-c", "$0 \"$@\"", "xterm", "-e", "guile"]
