# Dockerfile for sample_service

FROM erlang:22.2.3
MAINTAINER Venkatakumar Srinivasan

LABEL version="1.0"
LABEL description="Image for Github Actions"

ENV RUSTUP_HOME /opt/rust
ENV CARGO_HOME /opt/rust

# Install rust
RUN mkdir /opt/rust && \
    echo "export RUSTUP_HOME=/opt/rust" >> /etc/profile.d/rust.sh && \
    echo "CARGO_HOME=/opt/rust" >> /etc/profile.d/rust.sh && \
    curl https://sh.rustup.rs -sSf | sh -s -- -y --no-modify-path

RUN cd /usr/local/bin/ 	&& \
	for f in `ls /opt/rust/bin/*`; do echo "ln -s $f $(basename $f)" | /bin/bash; done

# Install base packages
RUN apt-get update  && \
	apt-get install -y --no-install-recommends \
	apt-utils                                  \
	python-pip                                 \
	python-setuptools                          \
        postgresql-client                          \
	gdebi  && \
	rm -rf /var/lib/apt/lists/*

# Update ansible
RUN pip install --upgrade pip setuptools wheel
RUN pip install --upgrade ansible

# Install packer
RUN curl -sO https://releases.hashicorp.com/packer/1.5.1/packer_1.5.1_linux_amd64.zip && \
	unzip packer_1.5.1_linux_amd64.zip && \
	mv packer /usr/bin/

# Cleanup
RUN apt-get update && \
	rm -rf /var/lib/apt/lists/*
