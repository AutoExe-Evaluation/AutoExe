CC = gcc
CFLAGS = -O2 -I ../include/ \
    -std=c11 -fPIC -fvisibility=hidden \
    -I. -Iwasm -Iinclude

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

TARGET=libtree-sitter.a

all: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) rcs $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean

