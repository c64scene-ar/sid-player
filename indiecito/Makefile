# Makefile copied from Zoo Mania game

.SILENT:

DIST_IMAGE = "pvm2_dist.d64"
DEV_IMAGE = "pvm2_dev.d64"
C1541 = c1541
X64 = x64

all: dev dist

SRC=src/main.s src/utils.s
prg:
	cl65 -d -g -Ln pvm2.sym -u __EXEHDR__ -t c64 -o pvm2.prg -C pvm2.cfg ${SRC}

dev: prg
	$(C1541) -format "pvm2,02" d64 $(DEV_IMAGE)
	$(C1541) $(DEV_IMAGE) -write pvm2.prg
	$(C1541) $(DEV_IMAGE) -list

dist: prg
	exomizer sfx sys -o pvm2_exo.prg pvm2.prg
	$(C1541) -format "pvm2 dist,02" d64 $(DIST_IMAGE)
	$(C1541) $(DIST_IMAGE) -write pvm2_exo.prg "pvm2 indiecito"
	$(C1541) $(DIST_IMAGE) -list

test: dev 
	$(X64) -moncommands pvm2.sym $(DEV_IMAGE)

testdist: dist
	$(X64) -moncommands pvm2.sym $(DIST_IMAGE)

clean:
	rm -f src/*.o pvm2.prg pvm2_exo.prg pvm2.sym $(DEV_IMAGE) $(DIST_IMAGE)
