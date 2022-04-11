# eva Makefile

DC ?=		ldc2
DFLAGS ?=	-O -release

PROG =	evac

all:
	${DC} ${DFLAGS} -c -of${PROG}.o ${PROG}.d
	${DC} ${LDFLAGS} -of${PROG} ${PROG}.o

clean:
	rm -f ${PROG} ${PROG}.o ${PROG}.core
