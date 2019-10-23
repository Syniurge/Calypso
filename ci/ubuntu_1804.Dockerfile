FROM ubuntu:18.04

MAINTAINER Elie Morisse <syniurge@gmail.com>

ENV \
  ARCH=x86_64 \
  LDC_VERSION=1.17.0 \
  LLVM_VERSION=9

RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install -y curl wget software-properties-common git

# NOTE: Official LLVM PPA's llvm-9-tools package is broken
# RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

# RUN add-apt-repository "deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic-${LLVM_VERSION}  main" \

RUN add-apt-repository -y ppa:oibaf/graphics-drivers \
 && apt-get install -y build-essential cmake python-pip zlib1g-dev \
                       llvm-${LLVM_VERSION}-dev llvm-${LLVM_VERSION}-tools \
                       lldb-${LLVM_VERSION} libclang-common-${LLVM_VERSION}-dev # NOTE: shouldn't be necessary, but LLVMExports.cmake complains about missing executables

RUN ln -s FileCheck-${LLVM_VERSION} /usr/bin/FileCheck \
 && ln -s not-${LLVM_VERSION} /usr/bin/not

# Packaging BUG workaround : for some reason both the official LLVM and Oibaf PPA packages apply a patch to llvm/ADT/Triple.h that renames the Triple::KFreeBSD enum member to Triple::kFreeBSD, and breaks the Clang build, so revert that change
RUN sed -e 's/kFreeBSD/KFreeBSD/g' /usr/include/llvm-${LLVM_VERSION}/llvm/ADT/Triple.h > /usr/include/llvm-${LLVM_VERSION}/llvm/ADT/Triple.h.new \
 && mv /usr/include/llvm-${LLVM_VERSION}/llvm/ADT/Triple.h.new /usr/include/llvm-${LLVM_VERSION}/llvm/ADT/Triple.h

RUN curl -LfsS -o /tmp/ldc.tar.xz "https://github.com/ldc-developers/ldc/releases/download/v${LDC_VERSION}/ldc2-${LDC_VERSION}-linux-${ARCH}.tar.xz" \
 && tar -xf /tmp/ldc.tar.xz -C / --strip=1 \
 && rm /tmp/ldc.tar.xz \
 && rm -f /var/cache/apt/archives/*.deb \
 && pip install --user lit
