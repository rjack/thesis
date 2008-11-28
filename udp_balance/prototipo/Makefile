include config.mk


.PHONY	: clean all


all		: $(EXE)


$(ULB_EXE)	: main.o $(ULB_OBJ)
	$(LD) -o $@ $(LDFLAGS) $< $(ULB_OBJ)


$(PRX_EXE)	: proxy.o $(ULB_OBJ)
	$(LD) -o $@ $(LDFLAGS) $< $(ULB_OBJ)


%.o		: %.c $(DEPCOM)
	$(CC) $(CPPOPT) $(CFLAGS) -c $< -o $@


clean		:
	rm -f $(OBJ) $(EXE)
