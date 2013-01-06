port: port.c erl_comm.c
	gcc -o suunto_port port.c erl_comm.c

all: port