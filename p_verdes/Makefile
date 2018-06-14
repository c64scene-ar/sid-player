# Makefile copied from Zoo Mania game

.SILENT:

DIST_IMAGE = "pvm5_dist.d64"
DEV_IMAGE = "pvm5_dev.d64"
C1541 = c1541
X64 = x64

all: dev dist

SRC=src/main.s src/utils.s
prg:
	cl65 -d -g -Ln pvm5.sym -u __EXEHDR__ -t c64 -o pvm5.prg -C pvm5.cfg ${SRC}

dev: prg
	$(C1541) -format "pvm5,02" d64 $(DEV_IMAGE)
	$(C1541) $(DEV_IMAGE) -write pvm5.prg
	$(C1541) $(DEV_IMAGE) -list

dist: prg
	exomizer sfx sys -o pvm5_exo.prg pvm5.prg
	$(C1541) -format "pvm5 dist,02" d64 $(DIST_IMAGE)
	$(C1541) $(DIST_IMAGE) -write pvm5_exo.prg "pvm5 monolongo"
	$(C1541) $(DIST_IMAGE) -list

test: dev 
	$(X64) -moncommands pvm5.sym $(DEV_IMAGE)

testdist: dist
	$(X64) -moncommands pvm5.sym $(DIST_IMAGE)

clean:
	rm -f src/*.o pvm5.prg pvm5_exo.prg pvm5.sym $(DEV_IMAGE) $(DIST_IMAGE)
