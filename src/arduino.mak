# -*- makefile -*-

#BOARD_TAG = uno
#MONITOR_PORT = /dev/ttyACM0

BOARD_TAG = nano328
MONITOR_PORT = /dev/ttyUSB0

AR_NAME = avr-gcc-ar
EXTRA_FLAGS := -Os -flto

DEFS := -DTARGET_ARDUINO=1
DEFS += -DWITH_MEMSTATS=0
DEFS += -DWITH_RC_SCRIPT=0

CFLAGS   += $(DEFS) -W -Wall -std=c11
CFLAGS   += -Wmissing-prototypes -Wstrict-prototypes -Werror
CFLAGS   += $(EXTRA_FLAGS)
CXXFLAGS += $(DEFS) -W -Wall -std=gnu++11
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS  += $(EXTRA_FLAGS)

include /usr/share/arduino/Arduino.mk
