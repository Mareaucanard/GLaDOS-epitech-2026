FROM fedora:latest

RUN dnf install -y stack
RUN dnf install -y make
RUN dnf install -y xz
RUN dnf install -y g++
RUN dnf install -y gcc-c++
RUN dnf install -y gcc-gfortran
RUN dnf install -y nodejs

RUN stack setup --resolver lts-21.12

CMD ["bash"]