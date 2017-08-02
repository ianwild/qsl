# -*- makefile -*-
ARDUINO_DIR = $(HOME)/Arduino/arduino-1.8.3
ARDMK_DIR = $(HOME)/Arduino/Arduino-Makefile-master
#BOARDS_TXT = $(ARDUINO_DIR)/hardware/arduino/avr/boards.txt

#BOARD_TAG = uno
#MONITOR_PORT = /dev/ttyACM0

BOARD_TAG = nano
BOARD_SUB = atmega328
MONITOR_PORT = /dev/ttyUSB0

AR_NAME = avr-gcc-ar
#EXTRA_FLAGS := -Os -flto

QSL_OPTIONS := -DTARGET_ARDUINO=1
QSL_OPTIONS += -DWITH_MEMSTATS=0
QSL_OPTIONS += -DWITH_RC_SCRIPT=0
QSL_OPTIONS += -DWITH_NAMESPACE=1

CFLAGS   += $(QSL_OPTIONS) -W -Wall -std=c11
#CFLAGS   += -Wmissing-prototypes -Wstrict-prototypes -Werror
CFLAGS   += $(EXTRA_FLAGS)
CXXFLAGS += $(QSL_OPTIONS) -W -Wall -std=gnu++11
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS  += $(EXTRA_FLAGS)

include $(ARDMK_DIR)/Arduino.mk
