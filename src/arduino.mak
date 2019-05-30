# -*- makefile -*-

#BOARD_TAG = leonardo
#BOARD_TAG = mega
BOARD_TAG = nano
#BOARD_TAG = uno

ifneq ($(filter leonardo mega uno,$(BOARD_TAG)),)
  MONITOR_PORT = /dev/ttyACM0
endif

ifeq (mega,$(BOARD_TAG))
  BOARD_SUB = atmega2560
endif

ifeq (nano,$(BOARD_TAG))
  MONITOR_PORT = /dev/ttyUSB0
  BOARD_SUB = atmega328
endif

# uncommenting this next line frees up (2 * (64 - 16)) bytes of RAM
#EXTRA_FLAGS := -DSERIAL_RX_BUFFER_SIZE=16 -DSERIAL_TX_BUFFER_SIZE=16

QSL_OPTIONS := -DTARGET_ARDUINO=1
QSL_OPTIONS += -DWITH_MEMSTATS=0
QSL_OPTIONS += -DWITH_RC_SCRIPT=1
QSL_OPTIONS += -DWITH_NAMESPACE=1

CFLAGS   += $(QSL_OPTIONS) -W -Wall -std=c11
CFLAGS   += -Wmissing-prototypes -Wstrict-prototypes -Werror
   # take out "-Werror" unless you're happy to correct a couple of
   # Arduino header files
CFLAGS   += $(EXTRA_FLAGS)
CXXFLAGS += $(QSL_OPTIONS) -W -Wall -std=gnu++11
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS  += $(EXTRA_FLAGS)

ARDUINO_DIR = $(HOME)/Arduino/arduino-1.8.3
ARDMK_DIR = $(HOME)/Arduino/Arduino-Makefile-master

include $(ARDMK_DIR)/Arduino.mk

picocom:
	picocom $(MONITOR_PORT) 9600 --emap crcrlf -c --imap lfcrlf
