FROM ubuntu:jammy
RUN yes | unminimize \
    && apt-get update \
    && apt-get install --yes \
        man-db \
        manpages-posix \
    && apt-get upgrade --yes \
    && apt-get clean

# Container building tools
RUN apt-get update \
    && apt-get install --yes \
        curl \
        software-properties-common \
    && apt-get clean

# Python tools
RUN apt-get update \
    && apt-get install --yes \
        python3 \
        python3-dev \
    && apt-get clean
ENV POETRY_HOME /opt/poetry
RUN ["/bin/bash", "-c", "set -o pipefail && curl --silent --show-error --location https://install.python-poetry.org | python3 -"]
RUN update-alternatives --install /usr/local/bin/poetry poetry /opt/poetry/bin/poetry 20

# Haskell tools
RUN curl -sSL https://get.haskellstack.org/ | sh

# Developer tools
RUN add-apt-repository ppa:git-core/ppa
RUN apt-get update \
    && apt-get install --yes \
        ack \
        atop \
        bash-completion \
        dtrx \
        fzf \
        git \
        htop \
        make \
        mlocate \
        strace \
        sudo \
        tmux \
        tree \
        vim \
        wget \
    && apt-get clean

RUN useradd --create-home --shell /bin/bash --group sudo jonathan \
    && passwd -d jonathan
USER jonathan
WORKDIR /home/jonathan
