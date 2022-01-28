make -s lib -C auxil
make -s lib -C math
make -s lib -C stdio
make -s lib -C stdlib
make -s lib -C string
make -s lib -C new

ar -cvqs lib.a auxil/*.o math/*.o stdio/*.o stdlib/*.o string/*.o new/*.o
objcopy --redefine-syms=change_syms lib.a

