# Makefile for the Calypso demo of the Qt project found here:
# http://forum.dlang.org/thread/mm81tt$bph$1@digitalmars.com

DCOMP = ldc2
DFLAGS = -wi -v
RPATH = -L-rpath=/usr/lib/x86_64-linux-gnu/qt5/ -L-rpath=/usr/include/qt5
LIBS = -L-lQt5Widgets -L-lQt5Gui -L-lQt5Core -L-lGL -L"-lstdc++"
OPT = -O3
CXX_FLAGS = -fPIC -c
CXX_INCLUDES = -I/usr/include/qt5/
DEFINES =

CPP_ARGS = -cpp-args -fPIE
CPP_DEFINES = -cpp-args -D_REENTRANT -cpp-args -DQT_WIDGETS_LIB -cpp-args -DQT_GUI_LIB -cpp-args -DQT_CORE_LIB 
CPP_INCLUDES = -cpp-args -I/usr/lib/x86_64-linux-gnu/qt5/mkspecs/linux-g++ -cpp-args -I/usr/include/qt5 -cpp-args -I/usr/include/qt5/QtWidgets -cpp-args -I/usr/include/qt5/QtGui -cpp-args -I/usr/include/qt5/QtCore


qt5demo: qt5demo.d moc_cpp-qt5demo.o cpp-qt5demo.o
	$(DCOMP) moc_cpp-qt5demo.o cpp-qt5demo.o $(CPP_ARGS) $(DFLAGS) $(RPATH) $(LIBS) $(CPP_DEFINES) $(CPP_INCLUDES) $< -of $@

moc_cpp-qt5demo.o: moc_cpp-qt5demo.cpp
	clang++ $(CPP_OPT) $(CXX_FLAGS) $(CXX_INCLUDES) $< -o $@

cpp-qt5demo.o:	cpp-qt5demo.cpp cpp-qt5demo.hpp
	clang++ $(CPP_OPT) $(CXX_FLAGS) $(CXX_INCLUDES) $< -o $@

moc_cpp-qt5demo.cpp: cpp-qt5demo.hpp
	moc $(DEFINES) -I /usr/include/qt5 $< -o $@


clean:
	\rm -f qt5demo *.o moc_cpp-qt5demo.cpp calypso_cache calypso_cache.gen calypso_cache.h.pch calypso_cache.h
