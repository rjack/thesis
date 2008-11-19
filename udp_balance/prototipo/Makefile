include config.mk


.PHONY	: clean


$(EXE)	: $(OBJ)
	$(LD) -o $(EXE) $(LDFLAGS) $(OBJ)


%.o	: %.c $(DEPCOM)
	$(CC) $(CPPOPT) $(CFLAGS) -c $< -o $@


crono.o	: crono.h


clean	:
	rm -f $(OBJ) $(EXE)
