#!/bin/sh

if [ -e $1/id1 ]; then
	./extract_maps --out data/quake											\
		--extract-map																\
		--palette $1/id1/pak0/gfx/palette.lmp								\
		--extract-texture															\
		$1/id1/pak0/maps/start.bsp $1/id1/pak0/maps/e?m?.bsp
		#$1/id1/pak?/maps/{start,end}.bsp $1/id1/pak?/maps/e?m?.bsp

fi
