FROM ubuntu:13.10

RUN echo deb-src http://archive.ubuntu.com/ubuntu saucy main universe >> /etc/apt/sources.list \
	&& apt-get update \
	&& env DEBIAN_FRONTEND=noninteractive apt-get -y install build-essential wget \
	&& env DEBIAN_FRONTEND=noninteractive apt-get -y build-dep nettle-dev \
	&& rm -rf /var/cache/apt/archives
RUN env DEBIAN_FRONTEND=noninteractive apt-get -y install autoconf automake libncursesw5-dev libssl-dev linux-headers-generic \
	&& rm -rf /var/cache/apt/archives
ADD Makefile /usr/src/Makefile
RUN make -C /usr/src default && make -C /usr/src/ cleanall
