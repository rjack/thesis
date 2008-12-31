include config.mk


.PHONY	: clean all


all		: $(EXE)


$(EXE)		: $(OBJ)
	$(LD) -o $@ $(LDFLAGS) $(OBJ)


# TODO chiamare $(shell grep -o 'h/.*\.h' %.c)
%.o		: %.c
	$(CC) $(CPPOPT) $(CFLAGS) -c $< -o $@


clean		:
	rm -f $(OBJ) $(EXE)
