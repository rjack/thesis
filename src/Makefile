include config.mk


.PHONY		: clean all


all		: $(EXE)


$(EXE)		: $(OBJ)
	$(LD) -o $@ $(LDFLAGS) $(OBJ)


%.o		: %.c
	@$(MAKEDEPEND); \
		cp $*.d $*.P; \
		sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
		-e '/^$$/ d' -e 's/$$/ :/' < $*.d >> $*.P; \
		rm -f $*.d
	$(CC) $(CPPOPT) $(CFLAGS) -c $< -o $@


clean		:
	rm -f $(OBJ) $(DEP) $(EXE)
