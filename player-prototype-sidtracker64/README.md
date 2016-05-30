# Player para tema hecho con SidTracker64

## Nota tecnica

Como los temas hechos con SidTracker64 suelen estar exportados con una freq. "rara"
(no es ni PAL y NTSC compatible), no se puede usar el raster IRQ:

* ni para tocar el tema. se usa el timer CIA para tocarlo.
* ni para hacer efectitos, porque puede colisionar con el timer CIA, y no queremos que el tema suene mal en ningún momento. se tiene que priorizar la música.


## Parsear el .sid

* Con esta tool ([sid_info.py](https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_info.py) ), parsear el .sid:

```
$ sid_info.py mongolongo.sid
File: mongolongo.sid
Header: PSID, Version: 2 (Offset: $007c)
Load Address: $0000 ($1000)
Init Address: $1000
Play Address: $1003
Songs: 1 / Start Song: 1
Speed: 0b11111111111111111111111111111111
Title: MONGOLONGO
Author: SidTracker64 v1.0
Released: 2016-04-10
Flags: Built-in music player, C64 compatible, PAL, 8580
SidTracker64 info:
  Shadow variables: $18da - $18f1
  Gate variables: $1909, $1910, $1917
  Freq. PAL table lo/hi: $2234 / $2294
  Play Frequency: ~98.01hz
  CIA Timer PAL: $2745
  CIA Timer NTSC: $28c3
  CIA Timer PAL-N: $28ca
```

![](https://lh3.googleusercontent.com/-FW27-CTV2n0/V0w2K10HCjI/AAAAAAABeJY/lbE9HXjOHIQIJGTgBHYsCMbMKBuS7rYJwCCo/s640/Screen%2BShot%2B2016-05-30%2Bat%2B9.35.19%2BAM.png)

## Actualizar el player

* Abrir el [main.s](src/main.s) y editarlo

### Actualizar las constantes de SidTracker64

Estan por la [linea 33](https://github.com/c64scene-ar/sid-player/blob/master/player-prototype-sidtracker64/src/main.s#L33):

* `SID_SHADOW0`
* `SID_GATE0`
* `SID_FREQ_TABLE_LO`
* `SID_FREQ_TABLE_HI`
* `PAL_FREQ`
* `NTSC_FREQ`
* `PALN_FREQ`

![](https://lh3.googleusercontent.com/-uBnxrPf4FOY/V0w2Lh4OH1I/AAAAAAABeJw/xZUzkLeW9T4IIkRxf23BxTUIpolpKmOYQCCo/s640/Screen%2BShot%2B2016-05-30%2Bat%2B9.39.49%2BAM.png)


### Actualizar los créditos

[Linea 630](https://github.com/c64scene-ar/sid-player/blob/master/player-prototype-sidtracker64/src/main.s#L630), debería estar la variable `credits_label`

![](https://lh3.googleusercontent.com/-pbAigkANIv0/V0w2LcVcR6I/AAAAAAABeJk/QtuuSWT4T9o80MSvdTuPHFm3v2lCCDXdACCo/s640/Screen%2BShot%2B2016-05-30%2Bat%2B9.36.53%2BAM.png)

### Actualizar el título

[Linea 648](https://github.com/c64scene-ar/sid-player/blob/master/player-prototype-sidtracker64/src/main.s#L648), debería estar la variable `turro_label`

Probablemente haya que actualizar el archivito [vchar64](https://github.com/ricardoquesada/vchar64) que tiene los fonts:

* [pvm5-charset-multicolor.vchar64proj](external_res/pvm5-charset-multicolor.vchar64proj)

![](https://lh3.googleusercontent.com/-aAAK1VWePBE/V0w2K4cWt1I/AAAAAAABeJc/M6PV5QmIX8okhXkTK03o0h00leUnwcncQCCo/s640/Screen%2BShot%2B2016-05-30%2Bat%2B9.36.05%2BAM.png)

Editarlo y exportar el charset con el nombre de `pvm5-charset-multicolor` y ponerlo en `src`.
El nombre completo al ser exportado debería ser `src/pvm5-charset-multicolor-charset.bin`

## Compilar y testear

Desde el directorio `player-prototype-sidtracker64` (donde esta el archivito `Makefile`) hacer:

Genera el archivito `pvm5.prg` y el `pvm5_dev.d64`
```
$ make
```

y este además corre el player en VICE con debug info para poder ser debugeado desde el monitor de VICE:

```
$ make test
```

Hay que tener [cc65](http://cc65.github.io/cc65/) instalado en el path. Tener el [c1541](http://vice-emu.sourceforge.net/) en el path es opcional.

## Subir el nuevo player a repo

Para el historial y demás cosas, subir el player a [c64scene-ar)(https://github.com/c64scene-ar/sid-player)
