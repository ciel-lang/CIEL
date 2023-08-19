FROM clfoundation/sbcl:2.2.4 AS build

WORKDIR /home

RUN apt-get update \
    && apt-get install -y libmagic-dev libc6-dev gcc wget git make

COPY . .

# install CIEL dependencies
RUN mkdir -p ~/common-lisp \
    && ( cd ~/common-lisp/ && wget https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz  && tar -xvf asdf-3.3.5.tar.gz && mv asdf-3.3.5 asdf )

RUN QUICKLISP_ADD_TO_INIT_FILE=true /usr/local/bin/install-quicklisp

RUN make ql-deps \
    && make build \
    && cp bin/* /usr/local/bin/

