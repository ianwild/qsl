# -*- makefile -*-

#BOARD_TAG = uno
#MONITOR_PORT = /dev/ttyACM0

BOARD_TAG = nano328
MONITOR_PORT = /dev/ttyUSB0

EXTRA_FLAGS = -Os -flto
DEFS = -DTARGET_ARDUINO=1

CFLAGS   += $(DEFS) -W -Wall -std=c11
CFLAGS   += -Wmissing-prototypes -Wstrict-prototypes -Werror
CXXFLAGS += $(DEFS) -W -Wall -std=gnu++11

include /usr/share/arduino/Arduino.mk
