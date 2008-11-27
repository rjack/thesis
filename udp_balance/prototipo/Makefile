include config.mk


.PHONY	: clean all


all		: $(EXE)


$(ULB_EXE)	: $(ULB_OBJ)
	$(LD) -o $@ $(LDFLAGS) $(ULB_OBJ)


$(PRX_EXE)	: $(PRX_OBJ)
	$(LD) -o $@ $(LDFLAGS) $(PRX_OBJ)


%.o		: %.c $(DEPCOM)
	$(CC) $(CPPOPT) $(CFLAGS) -c $< -o $@


clean		:
	rm -f $(OBJ) $(EXE)
