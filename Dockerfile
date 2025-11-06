# Use an official SBCL image as the base
FROM ubuntu:latest

# Set the working directory
WORKDIR /usr/src/app

# Install dependencies
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    wget 

RUN apt-get install -y \
    openssl \
    libssl-dev \
    libtool \
    autoconf \
    unzip 

RUN wget https://cmake.org/files/v3.31/cmake-3.31.4.tar.gz
RUN tar -xzvf cmake-3.31.4.tar.gz
RUN cd cmake-3.31.4 && \
    ./bootstrap && \
    make && \
    make install

#Install git
RUN apt-get install -y git
RUN apt-get install -y xorg-dev
RUN apt-get install -y g++

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    wget 


RUN git clone --branch v3.1.1 --depth 1 https://github.com/erincatto/box2d.git
# RUN git clone https://github.com/erincatto/box2d.git
# HOTFIX 1: Fix the CMakeLists.txt file to not build the samples
RUN sed -i 's/option(BOX2D_SAMPLES "Build the Box2D samples" ON)/option(BOX2D_SAMPLES "Build the Box2D samples" OFF)/g' /usr/src/app/box2d/CMakeLists.txt
RUN cd  /usr/src/app/box2d && \
    ./build.sh


RUN apt install -y sbcl clang
    
RUN git clone https://gitlab.isp.uni-luebeck.de/hai/geomates.git
# COPY ../geomates /usr/src/app/geomates
RUN cd geomates && \
    clang -I ../box2d/include -g -pedantic -Wall -fPIC -shared -Wl,--no-undefined -lm -o wrapper.so wrapper.c ../box2d/build/src/libbox2d.a


    #clang -I ../box2d/include -g -pedantic -Wall -fPIC -shared -Wl,--no-undefined -lm -o wrapper.so wrapper.c ../box2d/build/src/libbox2d.a
# HOTFIX 2:
RUN sed -i  's/#(127 0 0 1)/#(0 0 0 0)/g' /usr/src/app/geomates/geomates.lisp

EXPOSE 8000

WORKDIR /usr/src/app/geomates