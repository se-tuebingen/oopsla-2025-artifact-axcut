FROM --platform=linux/amd64 ubuntu:24.04

ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true

# Set locale for UTF-8 encoding
ENV LANG=C.UTF-8

# Get the basic stuff
RUN apt-get update && \
    apt-get -y upgrade && \
    apt-get install -y \
    sudo

# Create sudouser user with sudo privileges
RUN useradd -ms /bin/bash sudouser && \
    usermod -aG sudo sudouser
# Disable sudo password
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Set as default user
USER sudouser


# Install hyperfine
WORKDIR /home/sudouser
RUN sudo apt-get install -y wget
RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.15.0/hyperfine_1.15.0_amd64.deb
RUN sudo dpkg -i hyperfine_1.15.0_amd64.deb

# Configure tzdata
RUN echo 'tzdata tzdata/Areas select Europe' | sudo debconf-set-selections
RUN echo 'tzdata tzdata/Zones/Europe select London' | sudo debconf-set-selections
RUN sudo apt-get install -qq --no-install-recommends tzdata

# Install tools for running, viewing and editing benchmarks
RUN sudo apt-get install -y make csvkit vim


# Install languages

## Install Rust
WORKDIR /home/sudouser
RUN sudo apt install -y curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/home/sudouser/.cargo/bin:${PATH}"
RUN rustup default 1.76.0


## Install MLton
WORKDIR /home/sudouser
#RUN sudo apt install -y curl
RUN sudo apt install -y libgmp-dev
RUN curl -sSL https://github.com/MLton/mlton/releases/download/on-20210117-release/mlton-20210117-1.amd64-linux-glibc2.31.tgz --output mlton.tgz
RUN tar -xzf mlton.tgz
WORKDIR /home/sudouser/mlton-20210117-1.amd64-linux-glibc2.31
RUN sudo make install


## Install opam
#RUN sudo apt-get install -y wget curl make
RUN sudo apt-get install -y git gcc m4 unzip bubblewrap bzip2
RUN curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh > /tmp/install.sh
RUN ["/bin/bash", "-c", "sudo /bin/bash /tmp/install.sh <<< /usr/local/bin"]

RUN opam init -y --disable-sandboxing --bare
RUN echo "test -r /home/sudouser/.opam/opam-init/init.sh && . /home/sudouser/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> /home/sudouser/.profile

## Install OCaml
RUN opam switch create -y 5.0.0
RUN eval $(opam env)

RUN opam install dune


## Install Koka
WORKDIR /home/sudouser
#RUN sudo apt install -y curl
RUN curl -sSL https://github.com/koka-lang/koka/releases/download/v3.1.1/install.sh | sudo sh


## Install nasm
RUN sudo apt-get install -y nasm


## Install ChezScheme
WORKDIR /home/sudouser
#RUN sudo apt install -y git
RUN sudo apt install -y libncurses-dev
RUN git clone --filter=blob:none https://github.com/cisco/ChezScheme.git
WORKDIR /home/sudouser/ChezScheme
RUN git checkout v10.1.0
RUN ./configure --threads --disable-x11 --installprefix=~/.local LDFLAGS=-ltinfo
RUN make && make install
ENV PATH="/home/sudouser/.local/bin:${PATH}"


## Install Idris2
WORKDIR /home/sudouser
#RUN sudo apt install -y gcc git make libgmp-dev
RUN git clone https://github.com/idris-lang/Idris2.git
WORKDIR /home/sudouser/Idris2
RUN git checkout c3239cb4c
RUN make bootstrap SCHEME=scheme
RUN make install
ENV PATH="/home/sudouser/.idris2/bin:${PATH}"


# Copy AxCut implementation
COPY --chown=sudouser:sudouser axcut /home/sudouser/axcut

# Copy benchmark programs
COPY --chown=sudouser:sudouser benchmark-programs /home/sudouser/benchmark-programs

# Copy implementation of normalization
COPY --chown=sudouser:sudouser normalization /home/sudouser/normalization


# Final steps
WORKDIR /home/sudouser
ENV DEBIAN_FRONTEND teletype
CMD ["/bin/bash"]
