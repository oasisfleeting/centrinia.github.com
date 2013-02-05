#!/usr/bin/python

import struct
import re
import json

def strip_null(s):
	return s.split('\0',1)[0]

level_lumps = ['THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SEGS','SSECTORS','NODES','SECTORS','REJECT','BLOCKMAP']
levels = {}

def process_level_lump(name,filepos,wad_file):

	wad_file.seek(filepos)
	lumps = []
	if name == 'THINGS':
		# short[5] : x,y,angle,type,flags
		THING_SIZE = 2*5
		for j in range(size/THING_SIZE):
			x,y,angle,thing_type,flags = struct.unpack('5h',wad_file.read(THING_SIZE))
			lump = {	'x': x,
						'y': y,
						'angle': angle,
						'type': thing_type,
						'flags': flags
						}
			lump['index'] = j
			lumps.append(lump)
			#print x,y,angle,thing_type,flags
	elif name == 'LINEDEFS':
		# short[7] : start vertex,end vertex,flags,special type,sector tag,right sidedef,left sidedef
		LINEDEF_SIZE = 2*7
		for j in range(size/LINEDEF_SIZE):
			start,end,flags,special_type,sector,right_sidedef,left_sidedef = struct.unpack('7h',wad_file.read(LINEDEF_SIZE))
			lump = {	'start vertex': start,
						'end vertex': end,
						'flags': flags,
						'special type': special_type,
						'sector': sector,
						'right sidedef': right_sidedef,
						'left sidedef': left_sidedef
						}
			lump['index'] = j
			lumps.append(lump)

			#print start,end,flags,special_type,sector,right_sidedef,left_sidedef
	elif name == 'SIDEDEFS':
		# short[2] : x offset, y offset
		# char[8][3]: upper texture, lower texture, middle texture
		# short: sector number
		SIDEDEF_SIZE = 2*2+8*3+2
		for j in range(size/SIDEDEF_SIZE):
			x,y,upper,lower,middle,sector = struct.unpack('2h8s8s8sh',wad_file.read(SIDEDEF_SIZE))
			upper,lower,middle = map(strip_null,[upper,lower,middle])
			lump = {	'x offset': x,
						'y offset': y,
						'upper texture': upper,
						'lower texture': lower,
						'middle texture': middle,
						'sector': sector
						}
			lump['index'] = j
			lumps.append(lump)
			#print x,y,upper,lower,middle,sector
	elif name == 'VERTEXES':
		# short[2] : x,y
		VERTEX_SIZE = 2*2
		for j in range(size/VERTEX_SIZE):
			x,y = struct.unpack('2h',wad_file.read(VERTEX_SIZE))
			lump = {	'x' : x,
						'y' : y
						}
			lump['index'] = j
			lumps.append(lump)
	elif name == 'SEGS':
		# short[6]: start vertex number, end vertex number, angle, linedef, direction, offset
		SEG_SIZE = 2*6
		for j in range(size/SEG_SIZE):
			start,end,angle,linedef,direction,offset = struct.unpack('6h',wad_file.read(SEG_SIZE))
			lump = {	'start vertex': start,
						'end vertex': end,
						'angle': angle,
						'linedef': linedef,
						'direction': direction,
						'offset': offset
						}
			lump['index'] = j
			lumps.append(lump)
	elif name == 'SSECTORS':
		# short[2]: seg count, first seg number
		SSECTOR_SIZE = 2*2
		for j in range(size/SSECTOR_SIZE):
			count,first = struct.unpack('2h',wad_file.read(SSECTOR_SIZE))
			lump = {	'seg count': count,
						'seg offset': first
						}
			lump['index'] = j
			lumps.append(lump)
	elif name == 'NODES':
		# short[4]: partition line x, partition line y, partition line x delta, partition line y delta
		# short[4][2]: right bounding box, left bounding box (top,bottom,left,right)
		# short[2]: right child, left child
		NODE_SIZE = 2*14
		for j in range(size/NODE_SIZE):
			x,y,x_delta,y_delta,				\
			r_top,r_bot,r_left,r_right,	\
			l_top,l_bot,l_left,l_right,	\
			right_child,left_child			\
				= struct.unpack('14h',wad_file.read(NODE_SIZE))
			lump = {	'partition line x': x,
						'partition line y': y,
						'partition line delta x': x_delta,
						'partition line delta y': y_delta,
						'right bounding box top': r_top,
						'right bounding box bot': r_bot,
						'right bounding box left': r_left,
						'right bounding box right': r_right,
						'left bounding box top': l_top,
						'left bounding box bot': l_bot,
						'left bounding box left': l_left,
						'left bounding box right': l_right,
						'right child': right_child,
						'left child': left_child
						}
			lump['index'] = j
			lumps.append(lump)
	elif name == 'SECTORS':
		# short[2]: floor height, ceiling height
		# char[8][2]: floor texture, ceiling texture
		# short[3]: light level, type, tag number
		SECTOR_SIZE = 2*2+2*8+2*3
		for j in range(size/SECTOR_SIZE):
			floor_height,ceiling_height,		\
			floor_texture,ceiling_texture,	\
			light_level,sector_type,tag		\
				= struct.unpack('2h8s8s3h',wad_file.read(SECTOR_SIZE))
			lump = {	'floor height': floor_height,
						'ceiling height': ceiling_height,
						'floor texture': floor_texture,
						'ceiling texture': ceiling_texture,
						'light level': light_level,
						'sector type': sector_type,
						'tag number': tag
						}
			lump['index'] = j
			lumps.append(lump)
	elif name == 'REJECT':
		lump = {'reject': map(ord,wad_file.read(size))}
		lumps.append(lump)

	return lumps
	
def normalize_level(level):
	vertices = map(lambda vertex: [vertex['x'],vertex['y'],1], level['VERTEXES'])

	sectors = []
	for sector in level['SECTORS']:
		obj = sector.copy()
		obj['linedefs'] = []
		obj['vertexes'] = []
		sectors.append(obj)

	linedefs = []
	for linedef in level['LINEDEFS']:
		obj = linedef.copy()
		obj['start vertex'] = vertices[linedef['start vertex']]
		obj['end vertex'] = vertices[linedef['end vertex']]
		if linedef['right sidedef'] >= 0:
			linedef['right sidedef'] = level['SIDEDEFS'][linedef['right sidedef']]
		else:
			linedef['right sidedef'] = None
		if linedef['right sidedef'] >= 0:
			linedef['left sidedef'] = level['SIDEDEFS'][linedef['left sidedef']]
		else:
			linedef['left sidedef'] = None

		linedefs.append(obj)

	for linedef in linedefs:
		sidedef_indexes = filter(lambda index: index>=0, map(lambda key: linedef[key],['right sidedef','left sidedef']))
		sidedefs = map(lambda index: level['SIDEDEFS'][index], sidedef_indexes)
		for sidedef in sidedefs:
			sector = sectors[sidedef['sector']]

			this_linedef = linedef.copy()
			this_linedef['sidedef'] = sidedef
			sector['linedefs'].append(this_linedef)
	
	def flip_linedef(linedef):
		result = linedef.copy()
		result['start vertex'] = linedef['end vertex']
		result['end vertex'] = linedef['start vertex']
		result['left sidedef'] = linedef['right sidedef']
		result['right sidedef'] = linedef['left sidedef']
		return result

	def normalize_linedefs(linedefs):
		current_linedef = linedefs[0]
		linedefs.remove(current_linedef)

		current_vertex = current_linedef['start vertex']
		result = [(current_linedef,current_vertex)]

		while len(linedefs)>0:
			current_vertex = current_linedef['end vertex']
			#(next_linedef,next_vertex) =																		\
			#	next(((linedef,linedef['start vertex'])														\
			#		for linedef in linedefs if linedef['start vertex'] == current_vertex),		\
			#	next(((linedef,linedef['end vertex'])														\
			#		for linedef in linedefs if linedef['end vertex'] == current_vertex), (None,None)))
			next_linedef = None
			for linedef in linedefs:
				if linedef['start vertex'] == current_vertex:
					next_linedef = linedef
					linedefs.remove(linedef)
					break
				elif linedef['end vertex'] == current_vertex:
					next_linedef = flip_linedef(linedef)
					linedefs.remove(linedef)
		 			break

			#print '\t' + str(next_linedef)
			#for linedef in linedefs:
			#	print linedef['start vertex'],linedef['end vertex']
			result.append((next_linedef,current_vertex))
			current_linedef = next_linedef
			if not current_linedef:
				print current_vertex
				for linedef in linedefs:
					print linedef['start vertex'],linedef['end vertex']
					#print linedef
				print '\n'
				for (linedef,vertex) in result:
					print linedef['start vertex'],linedef['end vertex'],vertex
		return result


	for sector in sectors:
		linedef_vertex = normalize_linedefs(sector['linedefs'])
		for (linedef,vertex) in linedef_vertex:
			sector['vertexes'].append(vertex)
	return sectors

def gcd(a,b):
	if abs(a)<abs(b):
		(b,a) = (a,b)
	while b!=0:
		c = a % b
		a = b
		b = c
	return a

def reduce_projective(vector):
	g = reduce(lambda acc,x: gcd(acc,x),vector[1:],vector[0])
	if g!=0:
		return map(lambda x: x/g,vector)
	else:
		return vector

def subtract_vectors(vector0,vector1):
	return map(lambda (x,y): x-y,zip(vector0,vector1))

def subtract_vectors_projective(vector0,vector1):
	vector2 = map(lambda (x,y): x*vector1[-1]-y*vector0[-1],zip(vector0[0:-1],vector1[0:-1]))
	vector2.append(vector0[-1]*vector1[-1])
	return reduce_projective(vector2)

def dot_product(vector0,vector1):
	return sum(map(lambda (x,y): x*y,zip(vector0,vector1)))

def dot_product_projective(vector0,vector1):
	return [dot_product(vector0[:-1],vector1[:-1]),vector0[-1]*vector1[-1]]

def normal_equation(plane,vector):
	return dot_product(plane['normal'],vector)-plane['dist']

def make_line_projective(end,start):
	normal = subtract_vectors_projective(end,start)
	normal[:2] = normal[1::-1]
	normal[0] = -normal[0]
	
	print normal
	#print end,start
	#print normal
	#normal[:-1] = normal[-2::-1]
	#normal[0] = -normal[0]

# [x/w,y/w,z/w] . [a/d,b/d,c/d] = [x,y,z].[a,b,c] / (w*d)
# [normal.xyz*normal.w*start.w,dot(normal.xyz,start.xyz)]
	dist = dot_product(normal[:-1],start[:-1])
	normal[:-1] = map(lambda x: x*normal[-1]*start[-1], normal[:-1])
	normal[-1] = -dist*normal[-1]
	#normal[-1] = -dist

	normal = map(lambda x: -x, normal)
	#print dot_product(normal,start),dot_product(normal,end),normal

	return normal

def mix_projective(a,b,t):
	# a*(1-t)+b*t => [a.xyz*(t.w-t.x)/(a.w*t.w) + b.xyz*t.x/(b.w*t.w),1]
	# [a.xyz*(t.w-t.x)*b.w + b.xyz*t.x*a.w,a.w*b.w*t.w]
	c0 = map(lambda x: x*b[-1]*(t[-1]-t[0]),a[:-1])
	c1 = map(lambda x: x*a[-1]*t[0],b[:-1])
	c = map(lambda (x,y): x+y,zip(c0,c1))
	c.append(a[-1]*b[-1]*t[-1])
	return reduce_projective(c)

def determinant_2d(x,y):
	return x[0]*y[1]-y[0]*x[1]

def intersect_2d_projective(line0,line1):
	w = determinant_2d(line0,line1)
	x = determinant_2d([line0[2],line0[1]],[line1[2],line1[1]])
	y = determinant_2d([line0[0],line0[2]],[line1[0],line1[2]])
	return reduce_projective([x,y,w])

def intersect_segment(normal,end,start):
	# (p-p0).n = 0 => p.n =0
	# p = (1-t)*s+t*e -> p = s+t*(e-s)
	# ((1-t)*s+t*e-p0).n = 0 -> ((s-p0)-t*(e-s)).n = 0  => ((1-t)*s+t*e).n = 0 -> s.n+t*(e-s).n=0 -> t = -s.n/(e-s).n
	# t = ( p0.n - s.n ) / ((e-s).n)
	direction = subtract_vectors_projective(end,start)
	#direction = map(lambda x:x/direction[-1],direction)
	#normal = map(lambda x:x/normal[-1],normal)
	#print direction,end,start,normal
	denominator = dot_product(direction[:-1],normal[:-1])
	if denominator != 0:
		#t = map(lambda (x,y): -x*y,zip(dot_product_projective(normal,start),denominator))
		t = [-dot_product(normal,start)*direction[-1],denominator]
		if denominator < 0:
			t = map(lambda x: -x, t)
		#t = map(lambda x:x/t[-1],t)
		vector = mix_projective(start,end,t)
			
		return (t,vector)
	else:
		return None

def clip_polygon(vertexes,splitter):
	if len(vertexes) == 0:
		return vertexes
	result = []
	previous = vertexes[-1]
	for current in vertexes:
		#print current,previous
		#print dot_product(splitter,current),dot_product(splitter,previous)

		if (dot_product(splitter,current)>=0) != (dot_product(splitter,previous)>=0):
			#try:
			intersection_result = intersect_segment(splitter,current,previous)
			if intersection_result:
				(t,intersection) = intersection_result
				if 0 <=t[0] and t[0]<=t[1] and dot_product(splitter,intersection)>=0:
					result.append(intersection)
			#except TypeError:
			#	print current,previous,splitter

		if dot_product(splitter,current)>=0:
			result.append(current)
		previous = current
	return result

def to_quake(level):
	def parse_reject(sector_count,raw):
		reject = []
		for i in range(sector_count):
			row = []
			for j in range(sector_count):
				index = i*sector_count+j
				row.append((raw[index/8] >> (index % 8)) & 1 == 1);
			reject.append(row)
		return reject


	def find_item(items,item):
		if items.count(item) == 0:
			items.append(item)
			return len(items)-1
		else:
			return items.index(item)
			
	def augment_vertex_projective(vertex,z):
		vector = vertex[:-1]
		vector.append(z*vertex[-1])
		vector.append(vertex[-1])
		return vector

	def augment_vertex(index,z):
		vertex = level['VERTEXES'][index]
		return [vertex['x'],vertex['y'],z]

	def cross_product(vectors):
		result = [	\
			vectors[0][0]*vectors[1][1]-vectors[0][1]*vectors[1][2],
			-(vectors[0][0]*vectors[1][2]-vectors[0][2]*vectors[1][2]),
			vectors[0][1]*vectors[1][2]-vectors[0][2]*vectors[1][1]
		]

		return result
	def make_wall_face(planes,vertexes,sector,seg):
		vertex_indexes = []

		vertex_indexes.append(find_item(vertexes, augment_vertex(seg['start vertex'],sector['floor height'])))
		vertex_indexes.append(find_item(vertexes, augment_vertex(seg['end vertex'],sector['floor height'])))
		vertex_indexes.append(find_item(vertexes, augment_vertex(seg['end vertex'],sector['ceiling height'])))
		vertex_indexes.append(find_item(vertexes, augment_vertex(seg['start vertex'],sector['ceiling height'])))

		#if seg['direction'] != 0:
		#	vertex_indexes.reverse()

		vertex0 = vertexes[vertex_indexes[0]]
		normal = cross_product(map(lambda i: subtract_vectors(vertexes[i],vertex0),vertex_indexes[1:3]))
		dist = dot_product(normal,vertex0)
		return { 'plane id': find_item(planes, {'normal':normal, 'dist': dist}),
					'texture index': 0,
					'vertices index': vertex_indexes,
					'front side': True
					}
# The visibility list is run length encoded. A span of 8*N zeros aligned to a multiple of 8 would produce the byte sequence [0,N], if N is less than 256
	def visilist_rle(row):
		rle = []
		i = 0
		while i<len(row):
			zeros = 0
			while i<len(row) and not reduce(lambda x,y: x or y,row[i:i+8],False):
				i += 8
				zeros+=1

			while zeros >= 255:
				rle.append(0)
				rle.append(255)
				zeros-=255
			if zeros>0:
				rle.append(0)
				rle.append(zeros)
			if i<len(row):
				rle.append(reduce(lambda acc,y: acc*2+y,row[i:i+8][::-1],0))
				i+=8
		return rle

	sector_count = len(levels[levelname]['SECTORS'])
	reject = parse_reject(sector_count,level['REJECT'][0]['reject'])

	visilist = []
	visilist_offsets = []
	for sector in level['SECTORS']:
		row = reject[sector['index']]
		rle = visilist_rle(row)
		visilist_offsets.append(len(visilist))
		visilist.extend(rle)

	# 'planes'
	# 'normal','dist'
	planes = []

	nodes = []
	for node in level['NODES']:
		node['parent'] = None

	for node in level['NODES']:
		children_nodes = [None,None]
		children_leaves = [None,None]
		if node['right child'] & 0x8000 != 0:
			children_leaves[0] = node['right child'] & 0x7fff
			level['SSECTORS'][children_leaves[0]]['parent'] = node
		else:
			children_nodes[0] = node['right child']
			level['NODES'][children_nodes[0]]['parent'] = node

		if node['left child'] & 0x8000 != 0:
			children_leaves[1] = node['left child'] & 0x7fff
			level['SSECTORS'][children_leaves[1]]['parent'] = node
		else:
			children_nodes[1] = node['left child']
			level['NODES'][children_nodes[1]]['parent'] = node

		# The normal points toward the right.
		normal = [	node['partition line delta y'],	\
						-node['partition line delta x'],	\
						0 ]
		dist = dot_product(normal, [node['partition line x'], node['partition line y'],0])
		#print dist-dot_product(normal, [node['partition line x']+node['partition line delta x'], node['partition line y']+node['partition line delta y'],0])
		
		#print dist,normal
		plane_id = find_item(planes, {'normal': normal, 'dist': dist})

		node['plane'] = planes[plane_id]
		nodes.append({	'plane id': plane_id,
							'children nodes': children_nodes,
							'children leaves': children_leaves
							})
	for node in nodes:
		plane = planes[node['plane id']]
		for k in [0,1]:
			if node['children nodes'][k]:
				t = level['NODES'][node['children nodes'][k]]
				vec = [t['partition line x'],t['partition line y'],0]
				#print k,-(k*2-1)*(dot_product(plane['normal'],vec)-plane['dist'])
				vec = [t['partition line x']+t['partition line delta x'],t['partition line y']+t['partition line delta y'],0]
				#print k,-(k*2-1)*(dot_product(plane['normal'],vec)-plane['dist'])
			if node['children leaves'][k]:
				ssec = level['SSECTORS'][node['children leaves'][k]]
				dots = []
				for i in range(ssec['seg count']):
					seg = level['SEGS'][ssec['seg offset']+i]
					t = level['VERTEXES'][seg['start vertex']]
					vec = [t['x'],t['y'],0]
					dots.append(-(2*k-1)*(dot_product(plane['normal'],vec)-plane['dist']))
				#print k,2*k-1,dots

	# SSECTORS <-> 'leaves'
	# 'face indexes','visilist start','type'

	def sector_from_ssector(ssector):
		seg = level['SEGS'][ssector['seg offset']]
		linedef = level['LINEDEFS'][seg['linedef']]
		if seg['direction'] == 0:
			sidedef = level['SIDEDEFS'][linedef['right sidedef']]
		else:
			sidedef = level['SIDEDEFS'][linedef['left sidedef']]
		sector = level['SECTORS'][sidedef['sector']]
		return sector

	def to_vector_projective(v):
		return [v['x'],v['y'],0,1]

	def leaf_vertexes(ssector):
		polygon = [		[bounds[0][0],bounds[1][0],0,1],	\
							[bounds[0][1],bounds[1][0],0,1],	\
							[bounds[0][1],bounds[1][1],0,1],	\
							[bounds[0][0],bounds[1][1],0,1]]
		#print 'bound:\t' + str(polygon)
		# Only maintain the right side of the clipping planes.

		previous = ssector
		node = ssector['parent']
		while node != None:
			if node['right child']&0x7fff == previous['index']:
				side = 1
			else:
				side = -1
			side = -side
			normal = list(node['plane']['normal'])
			normal.append(-node['plane']['dist'])
			normal = reduce_projective(map(lambda x: x*side,normal))

			#print node['partition line delta x'],node['partition line delta y']
			#print normal
			#print 'polygon:\t' + str(polygon) + ',' + str(normal)
			polygon = clip_polygon(polygon,normal)

			previous = node
			node = node['parent']
			#print 'side:\t' + str(side)

		for seg_index in range(seg_first,seg_count+seg_first):
			seg = level['SEGS'][seg_index]
			vertex0 = to_vector_projective(level['VERTEXES'][seg['start vertex']])
			vertex1 = to_vector_projective(level['VERTEXES'][seg['end vertex']])
			splitter = reduce_projective(make_line_projective(vertex1,vertex0))
			#print 'polygon:\t' + str(polygon) + ',' + str(splitter)
			polygon = clip_polygon(polygon,splitter)

		#print 'clipped:\t' + str(polygon)

		return polygon

	bounds = to_vector_projective(level['VERTEXES'][0])
	#bounds = [[2**32,2**32,1],[-2**32,-2**32,1]]
	bounds = [bounds,bounds]
	for vertex in level['VERTEXES']:
		if vertex['x'] < bounds[0][0]:
			bounds[0][0] = vertex['x']
		if vertex['x'] > bounds[0][1]:
			bounds[0][1] = vertex['x']
		if vertex['y'] < bounds[1][0]:
			bounds[1][0] = vertex['y']
		if vertex['y'] > bounds[1][1]:
			bounds[1][1] = vertex['y']

		
	leaves = []
	vertexes = []
	faces = []
	for ssector in level['SSECTORS']:
		seg_count = ssector['seg count']
		seg_first = ssector['seg offset']
		sector = sector_from_ssector(ssector)
		# level['SECTORS'][level['LINEDEFS'][level['SEGS'][ssector['seg offset']]['linedef']]['sector']]

		face_indexes = []
		floor_vertexes = []
		ceiling_vertexes = []
		#print seg_first,seg_count
		for seg_index in range(seg_first,seg_count+seg_first):
			seg = level['SEGS'][seg_index]
			if level['LINEDEFS'][seg['linedef']]['flags'] & 0x04 == 0: # Two sided
				wall = make_wall_face(planes,vertexes,sector,seg)
				face_indexes.append(find_item(faces,wall))

		floor_vertexes = leaf_vertexes(ssector)
		ceiling_vertexes = list(floor_vertexes);
		ceiling_vertexes.reverse()

		floor_vertex_indexes = []
		for vertex in floor_vertexes:
			vertex[2] = sector['floor height']*vertex[-1]
			#print vertex
			floor_vertex_indexes.append(find_item(vertexes,vertex))

		ceiling_vertex_indexes = []
		for vertex in ceiling_vertexes:
			vertex[2] = sector['ceiling height']*vertex[-1]
			ceiling_vertex_indexes.append(find_item(vertexes,vertex))

# Make the ceiling and floor faces.
		floor_face = {	'plane id': find_item(planes,{ 'normal': [0,0,1],'dist': sector['floor height']}),
							'texture index': 0,
							'vertices index': floor_vertex_indexes,
							'front side': True
							}
		ceiling_face = {	'plane id': find_item(planes, {'normal': [0,0,-1],'dist': -sector['ceiling height']}),
								'texture index': 0,
								'vertices index': ceiling_vertex_indexes,
								'front side': True
							}

		for index in ceiling_vertex_indexes:
			vertex = vertexes[index]

		if len(floor_face['vertices index'])>=3:
			face_indexes.append(find_item(faces,floor_face))
		#if len(ceiling_face['vertices index'])>=3:
		#	face_indexes.append(find_item(faces,ceiling_face))

		leaf = {	'face indexes': face_indexes,
					'visilist start': visilist_offsets[sector['index']],
					'type': -1
				};
		leaves.append(leaf)

	def project_down(v):
		if len(v)>3:
			return map(lambda x: (1.0*x)/v[-1],v[:3])
		else:
			return v

	vertexes = map(project_down, vertexes)
	return {	'nodes': nodes,
				'faces': faces,
				'leaves': leaves,
				'planes': planes,
				'vertices': vertexes,
				'visibility list': None, # visilist
				'root node': len(nodes)-1
				}

with open('doom.wad','r') as wad_file:
	mapnames = re.compile('MAP\d\d|E\dM\d')

	HEADER_SIZE = 4+4+4
	wad_header = wad_file.read(HEADER_SIZE)
	identification,numlumps,infotableofs = struct.unpack('4sII',wad_header)

	print identification
	print numlumps
	print infotableofs
	processing_levels = False
	for i in range(numlumps):
		DIRECTORY_ENTRY_SIZE = 4+4+8
		wad_file.seek(i*DIRECTORY_ENTRY_SIZE+infotableofs)
		directory_entry = wad_file.read(DIRECTORY_ENTRY_SIZE)
		filepos,size,name = struct.unpack('II8s',directory_entry)

		name = strip_null(name)

		if size == 0 and not mapnames.match(name) is None:
			processing_levels = True
			level_name = name
			levels[level_name] = {}
			print level_name
			continue

		if processing_levels:
			if level_lumps.count(name) > 0:
				#print '\t' + name
				levels[level_name][name] = process_level_lump(name,filepos,wad_file)
			else:
				processing_levels = False
		#else:
		#	print '*' + name
	
	#polygon = [-4864, -4864, 0, 1],[3808, -4864, 0, 1],[3808, 3808, 0, 1],[-4864, 3808, 0, 1]
	#normal = [0, 1, 0, 4672]
	#clip_polygon(polygon,normal)
	#polygon = [[0,0,0,1],[1,0,0,1],[1,1,0,1],[0,1,0,1]]
	#normal = make_line_projective([1,1,0,2],[1,0,0,3])
	#normal = make_line_projective([1,0,0,1],[1,1,0,1])
	#print clip_polygon(polygon,normal),polygon
	#print normal

	#for levelname in levels.keys():
	for levelname in ['E1M1']:
	#for levelname in []:
		print levelname
		level = levels[levelname]
		#thingname = 'REJECT'
		#sector_count = len(levels[levelname]['SECTORS'])
		quake = to_quake(level)

		player_origin = [0,0,0]
		for thing in level['THINGS']:
			if thing['type'] == 1:
				player_origin = [thing['x'],thing['y'],0]
				break

		quake['player start'] = {	'origin': player_origin,
											'angle': 0
											}

		quake['filename'] = levelname.lower() + '.bsp'
		quake['textures'] = [{	'displacements': [0,0],
										'miptex index': 0,
										'vectors': [[1,0,0],[0,1,0]]
										}]

		#print len(quake['vertices'])
		#print quake['vertices']
		with open('doom/' + levelname.lower() + '.json','w') as fp:
			json.dump(quake,fp)

		#print('\t{}: {}'.format(thingname,parse_reject(sector_count,levels[levelname][thingname][0]['reject'])))
		#for thingname in levels[levelname].keys():
		#print('\t{}: {}'.format(thingname,levels[levelname][thingname]))
