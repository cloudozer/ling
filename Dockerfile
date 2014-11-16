FROM proger/openling-env
RUN mkdir -p /usr/src/openling
ADD . /usr/src/openling
RUN mkdir -p /usr/src/openling/apps/crypto/ebin
RUN mkdir -p /usr/src/openling/apps/os_mon/ebin
RUN (cd /usr/src/openling && ./configure && make install)
RUN (cd /tmp; touch railing.config; railing image)
