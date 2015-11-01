# -*- makefile -*-

#BOARD_TAG = uno
#MONITOR_PORT = /dev/ttyACM0

BOARD_TAG = nano328
MONITOR_PORT = /dev/ttyUSB0

OPTIMISER_FLAGS = -O6 -flto
WARNING_FLAGS = -W -Wall -Wmissing-prototypes -Wstrict-prototypes -Werror

CFLAGS   += $(OPTIMISER_FLAGS) -DTARGET_ARDUINO=1 $(WARNING_FLAGS)
CXXFLAGS += -std=gnu++11 $(OPTIMISER_FLAGS) -DTARGET_ARDUINO=1
LDFLAGS  += $(OPTIMISER_FLAGS)

include /usr/share/arduino/Arduino.mk
