FROM clfoundation/sbcl:2.2.4 AS build

WORKDIR /home

RUN apt-get update \
    && apt-get install -y libmagic-dev libc6-dev gcc wget git make cl-quicklisp

COPY . .

# install CIEL dependencies
RUN mkdir -p ~/common-lisp \
    && ( cd ~/common-lisp/ && wget https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz  && tar -xvf asdf-3.3.5.tar.gz && mv asdf-3.3.5 asdf )

RUN sbcl --noinform --non-interactive \
    --eval '(load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp")' \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))'

RUN make ql-deps \
    && make build \
    && cp bin/* /usr/local/bin/

