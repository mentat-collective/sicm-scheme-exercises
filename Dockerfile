FROM ubuntu AS mit-scheme
MAINTAINER Sam Ritchie <sritchie09@gmail.com>

# libxt-dev, libltdl-dev
# Get dependencies, and clean up properly!
RUN apt-get update && DEBIAN_FRONTEND=noninteractive \
  apt-get install -y --no-install-recommends \
  build-essential \
  libx11-dev \
  m4 \
  python3-pip \
  rlwrap \
  texinfo \
  texlive \
  wget && \
  rm -rf /var/lib/apt/lists/*

# Set up environment variables to make the installation easier.
ENV SCHEME_VERSION 10.1.10
ENV SCHEME_DIR mit-scheme-${SCHEME_VERSION}
ENV SCHEME_TAR ${SCHEME_DIR}-x86-64.tar.gz

WORKDIR /tmp

# Get MIT-Scheme and install all the goodies, docs included.
RUN wget http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${SCHEME_VERSION}/${SCHEME_TAR} \
  && wget http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${SCHEME_VERSION}/md5sums.txt \
  && cat md5sums.txt | awk '/${SCHEME_TAR}/ {print}' | tee md5sums.txt \
  && tar xf ${SCHEME_TAR} \
  && cd ${SCHEME_DIR}/src \
  && ./configure && make && make install \
  && cd ../doc \
  && ./configure && make && make install-info install-html install-pdf \
  && cd ../.. \
  && rm -rf ${SCHEME_DIR} ${SCHEME_TAR} md5sums.txt

COPY ./scripts/with_rlwrap.sh /usr/local/bin/with-rlwrap
RUN chmod +x /usr/local/bin/with-rlwrap

# Copy in completions, so we get a nice experience at the repl.
COPY ./scripts/mit-scheme_completions.txt /root/.mit-scheme_completions

WORKDIR /root

# This allows mit-scheme to work out of the box; any arguments you send will get
# appended on to the end of this.
ENTRYPOINT ["with-rlwrap", "mit-scheme"]

# Next, we layer the scmutils library on top, and install the `mechanics`
# script to start it up.
FROM mit-scheme AS mechanics

# Install SCMUtils!
ENV SCM_UTILS_VERSION 20190830
ENV SCM_UTILS scmutils-${SCM_UTILS_VERSION}

WORKDIR /tmp

RUN wget http://groups.csail.mit.edu/mac/users/gjs/6946/${SCM_UTILS}.tar.gz \
  && tar xf ${SCM_UTILS}.tar.gz \
  && cd ${SCM_UTILS} \
  && ./install.sh \
  && mv mechanics.sh /usr/local/bin/mechanics \
  && cd .. \
  && rm -rf ${SCM_UTILS} ${SCM_UTILS}.tar.gz

# Copy in completions, so we get a nice experience at the repl.
COPY ./scripts/mit-scheme_completions.txt /root/.mechanics_completions

WORKDIR /root

ENTRYPOINT ["with-rlwrap", "mechanics"]

from mechanics AS mechanics-jupyter

WORKDIR /tmp

RUN pip3 install -U setuptools jupyter

RUN apt-get update && apt-get install -y --no-install-recommends \
  libzmq3-dev \
  pkg-config && \
  rm -rf /var/lib/apt/lists/*

# just for now, for testing.
RUN apt-get update && apt-get install -y --no-install-recommends

# Get the runtime itself. Could add a mode to mount it locally.
RUN wget https://github.com/joeltg/mit-scheme-kernel/archive/master.tar.gz \
  && tar xvf master.tar.gz \
  && cd mit-scheme-kernel-master \
  && make && make install

# make the link... awkward!
RUN rm /usr/local/bin/scheme && ln -s /usr/local/bin/mechanics /usr/local/bin/scheme

WORKDIR /root

EXPOSE 8888
CMD jupyter notebook --no-browser --allow-root --ip=0.0.0.0 --port=8888
