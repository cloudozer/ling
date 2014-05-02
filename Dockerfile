FROM proger/openling-env
RUN mkdir -p /usr/src/openling
ADD . /usr/src/openling
RUN (cd /usr/src/openling && ./configure --with-xen=/usr/src/xen-4.4.0 --with-nettle=/usr/local/lib && make install)
RUN (cd /tmp; touch railing.config; railing image)
