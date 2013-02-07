#!/usr/bin/python

import struct
import re
import json
import clnum
from clnum import *

import sys
import pygame
import time


def strip_null(s):
	return s.split('\0',1)[0]

level_lumps = ['THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SEGS','SSECTORS','NODES','SECTORS','REJECT','BLOCKMAP']
levels = {}

def process_level_lump(name,filepos,wad_file,wadtype):
	wad_file.seek(filepos)
	lumps = []
	if name == 'THINGS':
		if wadtype == 'doom':
			THING_SIZE = 2*5
		elif wadtype == 'hexen':
			THING_SIZE=2*7+6

		# short[5] : x,y,angle,type,flags
		for j in range(size/THING_SIZE):
			if wadtype == 'doom':
				x,y,angle,thing_type,flags = struct.unpack('5h',wad_file.read(THING_SIZE))
			elif wadtype == 'hexen':
				thing_id,x,y,start_height,angle,thing_type,flags,	\
				action_special_args = struct.unpack('7h6s',wad_file.read(THING_SIZE))

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
		if wadtype == 'doom':
			LINEDEF_SIZE = 2*7
		elif wadtype == 'hexen':
			LINEDEF_SIZE=2*3+6+2*2
		# short[7] : start vertex,end vertex,flags,special type,sector tag,right sidedef,left sidedef
		for j in range(size/LINEDEF_SIZE):
			if wadtype == 'doom':
				start,end,flags,special_type,args,			\
				right_sidedef,left_sidedef = struct.unpack('7h',wad_file.read(LINEDEF_SIZE))
			elif wadtype == 'hexen':
				start,end,flags,		\
				special_type,args,	\
				right_sidedef,left_sidedef = struct.unpack('3hc5s2h',wad_file.read(LINEDEF_SIZE))

			lump = {	'start vertex': start,
						'end vertex': end,
						'flags': flags,
						'special type': special_type,
						'args': args,
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

def rational(x):
	return mpq(x,1)

def select_node_child(node,right,level):
	SSECTOR_MASK = 0x8000
	if right:
		index = node['right child']
	else:
		index = node['left child']
	if index & SSECTOR_MASK != 0:
		return (False,level['SSECTORS'][index & (SSECTOR_MASK-1)])
	else:
		return (True,level['NODES'][index])

def draw_polygon(polygon,depth,bounds):
	if len(polygon)==0:
		return

	extents = max(bounds[1][0]-bounds[0][0],bounds[1][1]-bounds[0][1])
	scale_x = extents
	scale_y = extents
	pointlist = []
	for current in polygon:
		cx = float((current.elements[0]-bounds[0][0])/scale_x)*window_size+window_padding
		cy = float(1-(current.elements[1]-bounds[0][1])/scale_y)*window_size+window_padding
		pointlist.append((cx,cy))

	color_level = 150
	#print polygon
	pygame.draw.lines(window,(((depth&1)^1)*color_level,0,(depth&1)*color_level),True,pointlist)
	#pygame.draw.polygon(window,(((depth&1)^1)*color_level,0,(depth&1)*color_level),pointlist)
	#time.sleep(50/1000.0)
	pygame.display.flip()

def draw_subsector(subsector,bounds):
	extents = max(bounds[1][0]-bounds[0][0],bounds[1][1]-bounds[0][1])
	scale_x = extents
	scale_y = extents
	for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
		vertexes = map(lambda key: level['VERTEXES'][seg[key]]['vector'],['start vertex','end vertex'])
		px = float((vertexes[0].elements[0]-bounds[0][0])/scale_x)*window_size+window_padding
		py = float(1-(vertexes[0].elements[1]-bounds[0][1])/scale_y)*window_size+window_padding
		cx = float((vertexes[1].elements[0]-bounds[0][0])/scale_x)*window_size+window_padding
		cy = float(1-(vertexes[1].elements[1]-bounds[0][1])/scale_y)*window_size+window_padding
		pygame.draw.line(window,(0,255,0),(px,py),(cx,cy))
	#time.sleep(50/1000.0)
	pygame.display.flip()


def normalize_level(level):
	def make_node_plane(node):
		x0 = node['partition line x']
		y0 = node['partition line y']
		x1 = node['partition line delta x']
		y1 = node['partition line delta y']
		v0 = Vector(map(rational,[x0,y0,0]))
		v1 = Vector(map(rational,[x1,y1,0]))
		return make_line(v1+v0,v0)

	for node in level['NODES']:
		node['parent'] = None

	for node in level['NODES']:
		select_node_child(node,True,level)[1]['parent'] = node
		select_node_child(node,False,level)[1]['parent'] = node

		node['plane'] = make_node_plane(node)

	for vertex in level['VERTEXES']:
		elements = map(lambda coord: rational(vertex[coord]),['x','y'])
		elements.append(rational(0))
		vertex['vector'] = Vector(elements)

	for sector in level['SECTORS']:
		sector['floor height'] = rational(sector['floor height'])
		sector['ceiling height'] = rational(sector['ceiling height'])

	for subsector in level['SSECTORS']:
		seg = level['SEGS'][subsector['seg offset']]
		linedef = level['LINEDEFS'][seg['linedef']]
		if seg['direction'] == 0:
			sidedef = level['SIDEDEFS'][linedef['right sidedef']]
		else:
			sidedef = level['SIDEDEFS'][linedef['left sidedef']]
		sector = level['SECTORS'][sidedef['sector']]
		subsector['sector'] = sector

	def carve_subsector(polygon, subsector):
		result = polygon
		for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
			vertexes = map(lambda key: level['VERTEXES'][seg[key]]['vector'],['start vertex','end vertex'])
			splitter = make_line(vertexes[1], vertexes[0])
			(result,outside) = clip_polygon(result,splitter,rational(1))
		return result
	def split_polygon(node,depth,polygon):
		#print polygon
		(right_polygon,left_polygon) = clip_polygon(polygon,node['plane'],rational(1))
		for (next_polygon,is_right) in zip([right_polygon,left_polygon],[True,False]):
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				draw_polygon(next_polygon,depth+1,bounds)
				split_polygon(child,depth+1,next_polygon)
			else:
				flat = carve_subsector(next_polygon,child)
				child['polygon'] = flat
				draw_polygon(flat,depth+1,bounds)

	def draw_subsectors(node,depth):
		for is_right in [True,False]:
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				draw_subsectors(child,depth+1)
			else:
				draw_subsector(child,bounds)

	pygame.init()

	bounds = map(rational,[level['VERTEXES'][0]['x'],level['VERTEXES'][0]['y']])
	bounds = [list(bounds),list(bounds)]
	#print bounds[0]
	#print min(bounds[0][0],bounds[0][1])
	#print max(bounds[0][0],bounds[0][1])
	for vertex in level['VERTEXES']:
		x = rational(vertex['x'])
		y = rational(vertex['y'])
		#print x,y,bounds
		#print x,bounds[0][0], min(x,bounds[0][0])

		bounds[0][0] = min(bounds[0][0],x)
		bounds[0][1] = min(bounds[0][1],y)
		bounds[1][0] = max(bounds[1][0],x)
		bounds[1][1] = max(bounds[1][1],y)

	split_polygon(level['NODES'][-1],0,							\
			map(Vector,													\
				[[bounds[0][0],bounds[0][1],rational(0)],		\
				[bounds[1][0],bounds[0][1],rational(0)],		\
				[bounds[1][0],bounds[1][1],rational(0)],		\
				[bounds[0][0],bounds[1][1],rational(0)]]))
	draw_subsectors(level['NODES'][-1],0)

	return level


def gcd(a,b):
	if abs(a)<abs(b):
		(b,a) = (a,b)
	while b!=0:
		c = a % b
		a = b
		b = c
	return a

class Rational:
	def __init__(self, num, den=1):
		self.num = num
		self.den = den
	def reduce(self):
		g = gcd(self.num,self.den)
		if g != 0 and g != 1:
			return Rational(self.num/g,self.den/g)
		else:
			return self
	@staticmethod
 	def to_rational(a):
		if type(a) != 'Rational':
			return Rational(a)
		else:
			return a
	def __binop(a,b,f):
		return Rational(f(a.num*b.den,b.num*a.den),a.den*b.den).reduce()
	def __add__(a,b):
		return a.__binop(b,lambda x,y: x+y)
	def __sub__(a,b):
		return a.__binop(b,lambda x,y: x-y)
	def __mul__(a,b):
		return Rational(a.num*b.num,a.den*b.den).reduce()
	def __div__(a,b):
		return Rational(a.num*b.den,a.den*b.num).reduce()
	def __neg__(a):
		return Rational(-a.num,a.den)
	def __str__(self):
		if self.den !=1:
			return str(self.num) + '/' + str(self.den)
		else:
			return str(self.num)
 	def __repr__(self):
		return str(float(self.num)/float(self.den))
	def __cmp__(a,b):
 		return cmp(a.num*b.den,b.num*a.den)

class Vector:
	def __init__(self, elements=[]):
		self.elements = list(elements)
	def cross(a,b):
		x = a.elements[1]*b.elements[2]-a.elements[2]*b.elements[1]
		y = -(a.elements[0]*b.elements[2]-a.elements[2]*b.elements[0])
		z = a.elements[0]*b.elements[1]-a.elements[1]*b.elements[0]
		return Vector([x,y,z])
	def dot(a,b):
		return reduce(lambda acc,x: acc+x,map(lambda (x,y):x*y,zip(a.elements,b.elements)),rational(0))
	def __binop(a,b,f):
		return Vector(map(lambda (x,y): f(x,y), zip(a.elements,b.elements)))
	def __add__(a,b):
		return a.__binop(b,lambda x,y: x+y)
	def __sub__(a,b):
		return a.__binop(b,lambda x,y: x-y)
	def __str__(self):
		return str(self.elements)
	def __repr__(self):
		return str(map(float,self.elements))
	def __mul__(a,s):
		return Vector(map(lambda x: x*s, a.elements))

class Plane:
	def __init__(self,normal,dist):
		self.normal = normal
		self.dist = dist
	def normal_equation(self,vector):
		return self.normal.dot(vector) - self.dist
	def __str__(self):
		return '{' + str(self.normal) + '.p=' +  str(self.dist)+ '}'
	#def __repr__(self):
	#	return str({ 'normal': self.normal, 'dist': float(self.dist) })

def make_line(end,start,up=Vector(map(rational,[0,0,1]))):
	normal = (end-start).cross(up)
	dist  = normal.dot(end)
	return Plane(normal,dist)

def mix(a,b,t):
	return a+(b-a)*t

def intersect_segment(splitter,end,start):
	# (p-p0).n = 0
	# p = (1-t)*s+t*e
	# ((1-t)*s+t*e-p0).n = 0
	# (s-p0).n+t*(e-s).n = 0
	# t = - (s-p0).n / (e-s).n 

	direction = end-start
	#direction = map(lambda x:x/direction[-1],direction)
	#normal = map(lambda x:x/normal[-1],normal)
	#print direction,end,start,normal
	denominator = splitter.normal.dot(direction)
	if denominator != rational(0):
		#t = map(lambda (x,y): -x*y,zip(dot_product_projective(normal,start),denominator))
		t = -splitter.normal_equation(start)/denominator
		vector = mix(start,end,t)
		return (t,vector)
	else:
		return None

def clip_polygon(vertexes,splitter,side):
	if len(vertexes) == 0:
		return (vertexes,vertexes)
	right = []
	left = []
	previous = vertexes[-1]

	previous_dot = splitter.normal_equation(previous)*side
	for current in vertexes:
		current_dot = splitter.normal_equation(current) * side
		if (current_dot >= rational(0)) != (previous_dot >= rational(0)):
			intersection_result = intersect_segment(splitter,current,previous)
			if intersection_result:
				(t,intersection) = intersection_result
				intersection_dot = splitter.normal_equation(intersection)*side
				#print t,intersection_dot
				if rational(0) <= t and t <= rational(1): # and intersection_dot>=rational(0):
					if intersection_dot >= rational(0):
						right.append(intersection)
					if intersection_dot <= rational(0):
						left.append(intersection)

		if current_dot >= rational(0):
			right.append(current)
		if current_dot <= rational(0):
			left.append(current)

		(previous_dot,previous) = (current_dot,current)
	return (right,left)

def to_quake(level):
	def find_item(item_type,items,item):
		if items.count(item) == 0:
			if item_type == 'vertex':
				vertex = map(float,item.elements)
				items.append(vertex)
			elif item_type == 'plane':
				normal = map(float,item.normal.elements)
				dist = float(item.dist)
				plane = {'normal': normal, 'dist': dist}
				items.append(plane)
			else:
				items.append(item)

			return len(items)-1
		else:
			return items.index(item)
			
	def make_wall_face(planes,vertexes,seg,floor_height,ceiling_height):
		def augment_vertex(index,elevation):
			vertex = Vector(level['VERTEXES'][index]['vector'].elements)
			vertex.elements[2] = elevation
			return vertex

		vertex_indexes = []

		face_vertexes = []	
		face_vertexes.append(augment_vertex(seg['start vertex'],floor_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],floor_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],ceiling_height))
		face_vertexes.append(augment_vertex(seg['start vertex'],ceiling_height))
		vertex_indexes = map(lambda face_vertex: find_item('vertex',vertexes, face_vertex), face_vertexes)
		#if seg['direction'] != 0:
		#	vertex_indexes.reverse()
		
		#normal = cross_product(map(lambda i: subtract_vectors(vertexes[i],vertex0),vertex_indexes[1:3]))
		#dist = dot_product(normal,vertex0)
		normal = (face_vertexes[1]-face_vertexes[0]).cross(face_vertexes[2]-face_vertexes[0])
		dist = normal.dot(face_vertexes[0])
		return { 'plane id': find_item('plane',planes, Plane(normal, dist)),
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
	#reject = parse_reject(sector_count,level['REJECT'][0]['reject'])
	
	visilist = []
	visilist_offsets = []
	#for sector in level['SECTORS']:
	#	row = reject[sector['index']]
	#	rle = visilist_rle(row)
	#	visilist_offsets.append(len(visilist))
	#	visilist.extend(rle)

	up_vector = Vector(map(rational,[0,0,1]))
	down_vector = Vector(map(rational,[0,0,-1]))
	# 'planes'
	# 'normal','dist'
	planes = []
	nodes = []
	for node in level['NODES']:
		print 'node:\t' + str(float(node['index'])/len(level['NODES']))

		children_nodes = [None,None]
		children_leaves = [None,None]
		if node['right child'] & 0x8000 != 0:
			children_leaves[0] = node['right child'] & 0x7fff
		else:
			children_nodes[0] = node['right child']

		if node['left child'] & 0x8000 != 0:
			children_leaves[1] = node['left child'] & 0x7fff
		else:
			children_nodes[1] = node['left child']

		partition_line_vertex0 = Vector(map(rational,[node['partition line x'], node['partition line y'],0]))
		partition_line_vertex1 = partition_line_vertex0 + \
		Vector(map(rational,[node['partition line delta x'], node['partition line delta y'],0]))
		plane = make_line(partition_line_vertex1, partition_line_vertex0)

		#print dist,normal
		plane_id = find_item('plane',planes, plane)

		node['plane'] = planes[plane_id]
		nodes.append({  'plane id': plane_id,		\
			'children nodes': children_nodes,	\
			'children leaves': children_leaves	\
			})


	
	leaves = []
	vertexes = []
	faces = []
	for subsector in level['SSECTORS']:
		print 'subsector:\t' + str(float(subsector['index'])/len(level['SSECTORS']))
		seg_count = subsector['seg count']
		seg_first = subsector['seg offset']
		sector = subsector['sector']

		face_indexes = []
		floor_vertexes = []
		ceiling_vertexes = []
		floor_height = sector['floor height']
		ceiling_height = sector['ceiling height']
		#print seg_first,seg_count
		for seg_index in range(seg_first,seg_count+seg_first):
			seg = level['SEGS'][seg_index]
			if level['LINEDEFS'][seg['linedef']]['flags'] & 0x04 == 0: # Not two sided
				face = make_wall_face(planes,vertexes,seg,floor_height, ceiling_height)
				face_indexes.append(find_item('face',faces,face))
			else:
				linedef = level['LINEDEFS'][seg['linedef']]
				if seg['direction'] != 0:
					next_sidedef = level['SIDEDEFS'][linedef['right sidedef']]
				else:
					next_sidedef = level['SIDEDEFS'][linedef['left sidedef']]
				next_sector = level['SECTORS'][next_sidedef['sector']]
				next_floor_height = next_sector['floor height']
				next_ceiling_height = next_sector['ceiling height']

				if next_floor_height > floor_height:
					floor_face = make_wall_face(planes,vertexes,seg,floor_height, next_floor_height)
					face_indexes.append(find_item('face',faces,floor_face))
				if next_ceiling_height < ceiling_height:
					floor_face = make_wall_face(planes,vertexes,seg,next_ceiling_height,ceiling_height)
					face_indexes.append(find_item('face',faces,floor_face))

		floor_vertexes = subsector['polygon']
		ceiling_vertexes = list(floor_vertexes);
		ceiling_vertexes.reverse()

		floor_vertex_indexes = []
		for vertex in floor_vertexes:
			#print sector
			vertex.elements[2] = floor_height
			#print vertex
			floor_vertex_indexes.append(find_item('vertex',vertexes,vertex))


		ceiling_vertex_indexes = []
		for vertex in ceiling_vertexes:
			vertex.elements[2] = ceiling_height
			ceiling_vertex_indexes.append(find_item('vertex',vertexes,vertex))

# Make the ceiling and floor faces.

		floor_face = {	'plane id': find_item('plane',planes,Plane(up_vector, floor_height)),
							'texture index': 0,
							'vertices index': floor_vertex_indexes,
							'front side': True
							}
		ceiling_face = {	'plane id': find_item('plane',planes, Plane(down_vector,-ceiling_height)),
								'texture index': 0,
								'vertices index': ceiling_vertex_indexes,
								'front side': True
							}

		for index in ceiling_vertex_indexes:
			vertex = vertexes[index]

		if len(floor_face['vertices index'])>=3:
			face_indexes.append(find_item('face',faces,floor_face))
		if len(ceiling_face['vertices index'])>=3:
			face_indexes.append(find_item('face',faces,ceiling_face))

		leaf = {	'face indexes': face_indexes,
					'visilist start': -1, #visilist_offsets[sector['index']],
					'type': -1
				};
		leaves.append(leaf)
	#def project_down(v):
	#	return map(lambda x: float(x), v.elements)
	#vertexes = map(project_down, vertexes)

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

	#wadtype = 'doom'
	wadtype = 'hexen'

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

		#print name,size
		if not mapnames.match(name) is None:
			if size>0:
				wad_file.seek(filepos)
				print strip_null(wad_file.read(size))

			processing_levels = True
			level_name = name
			levels[level_name] = {}
			continue

		if processing_levels:
			if level_lumps.count(name) > 0:
				#print '\t' + name
				levels[level_name][name] = process_level_lump(name,filepos,wad_file,wadtype)
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

	window_size = 900
	window_padding = 10
	window = pygame.display.set_mode((window_size+2*window_padding,window_size+2*window_padding))
	levelnames = levels.keys()
	levelnames.sort()
	for levelname in levelnames:
	#for levelname in ['MAP01']:
	#for levelname in []:
		window.fill((0,0,0))
		quake = {}
		level = levels[levelname]
		print levelname
		normalize_level(level)['VERTEXES']
		#time.sleep(5)

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
	print 'Finished'
	#while True:
	#	for event in pygame.event.get():
	#		if event.type == pygame.QUIT:
	#			sys.exit(0)

