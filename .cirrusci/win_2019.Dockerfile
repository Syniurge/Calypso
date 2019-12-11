FROM cirrusci/windowsservercore:2019

MAINTAINER Elie Morisse <syniurge@gmail.com>

ENV \
  ARCH=x86_64 \
  LDC_VERSION=1.18.0 \
  LLVM_VERSION=9.0.0

RUN choco install -y visualstudio2017buildtools --package-parameters "--installPath C:\BuildTools --quiet" || exit /B 0
RUN choco install visualstudio2017-workload-vctools --package-parameters "--installPath C:\BuildTools --quiet" || exit /B 0

RUN choco install -y ldc --version %LDC_VERSION%
RUN choco install -y llvm --version %LLVM_VERSION%
RUN choco install -y cmake --installargs 'ADD_CMAKE_TO_PATH=System'
RUN choco install -y python
RUN choco install -y ninja || exit /B 0

RUN setx PATH "%PATH%;C:\ProgramData\chocolatey\lib\ninja\tools;C:\Program Files (x86)\Windows Kits\10\bin\10.0.17763.0\x64"

RUN pip install lit
