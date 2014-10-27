FROM proger/openling-env
RUN mkdir -p /usr/src/openling
ADD . /usr/src/openling
RUN (cd /usr/src/openling && ./configure --with-nettle=/usr/local/lib && make install)
RUN (cd /tmp; touch railing.config; railing image)
