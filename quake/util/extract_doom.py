#!/usr/bin/python

import matplotlib.pyplot as plt
import math
import struct
import re
import json
import clnum
import pyavltree
import geometry
from pyavltree import AVLTree
from clnum import *
import png

import sys
import pygame
import time
import texture_atlas as tex

def strip_null(s):
	return s.split('\0',1)[0]

level_lumps = ['THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SEGS','SSECTORS','NODES','SECTORS','REJECT','BLOCKMAP']
levels = {}

TRANSPARENT_COLOR = 255

def read_image(name,filepos,wad_file):
	wad_file.seek(filepos)

	HEADER_SIZE = 4*2
	width,height,left,top = struct.unpack('4h',wad_file.read(HEADER_SIZE))

	indexes_size = width*4
	column_array = struct.unpack('{}I'.format(width),wad_file.read(indexes_size))
	data = [TRANSPARENT_COLOR]*(width*height)
	for i in range(width):
		index = column_array[i]
		wad_file.seek(filepos+index)
		rowstart = 0
		while rowstart != 255:
			rowstart = ord(wad_file.read(1))
			if rowstart == 255:
				break
			pixel_count = ord(wad_file.read(1))
			wad_file.read(1) # Dummy value
			column = map(ord,struct.unpack('{}c'.format(pixel_count),wad_file.read(pixel_count)))
			wad_file.read(1) # Dummy value
			#k = rowstart+i*height
			#data[k:k+pixel_count] = column
			for k in range(pixel_count):
				data[(k+rowstart)*width+i] = column[k]
	return {'name': name, 'width': width, 'height': height, 'data': data}

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
						'upper texture': upper.lower(),
						'lower texture': lower.lower(),
						'middle texture': middle.lower(),
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
			floor_texture,ceiling_texture = map(strip_null,[floor_texture,ceiling_texture])
			lump = {	'floor height': floor_height,
						'ceiling height': ceiling_height,
						'floor texture': floor_texture.lower(),
						'ceiling texture': ceiling_texture.lower(),
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
	return mpq(x)

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

def draw_polygon(polygon):
	t = map(lambda i: map(lambda p: p.elements[i], polygon.vertexes),range(2))
	portals = polygon.auxillaries						 

	for i in range(len(polygon.vertexes)):
		xs = [t[0][i],t[0][i-1]]
		ys = [t[1][i],t[1][i-1]]
		if portals[i]:
			plt.plot(xs,ys,'r:')
		else:
			plt.plot(xs,ys,'b',linewidth=3)

def draw_subsector(subsector):
	for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
		linedef = level['LINEDEFS'][seg['linedef']]
		vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],['start vertex','end vertex'])

		t = map(lambda i: map(lambda v: v.elements[i], vertexes),range(2))
		if linedef['flags']&0x04 != 0:
			plt.plot(t[0],t[1],'g',linewidth=2)
		else:
			plt.plot(t[0],t[1],'m',linewidth=2)
		plt.plot(t[:][0],t[:][1],'m*')

def make_line(end,start,up=geometry.Vector(map(rational,[0,0,1]))):
	normal = (end-start).cross(up)
	dist  = normal.dot(end)
	return geometry.Plane(normal,dist)

def normalize_level(level):
	def make_node_plane(node):
		x0 = node['partition line x']
		y0 = node['partition line y']
		x1 = node['partition line delta x']
		y1 = node['partition line delta y']
		v0 = geometry.Vector(map(rational,[x0,y0,0]))
		v1 = geometry.Vector(map(rational,[x1,y1,0]))
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
		vertex['vector'] = geometry.Vector(elements)
		del vertex['x']
		del vertex['y']

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

	def between(start,end,x):
		return start < x and x < end

	def carve_subsector(polygon, subsector):
		right_polygon = polygon
		for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
			if len(right_polygon.vertexes)==0:
				break

			linedef = level['LINEDEFS'][seg['linedef']]
			two_sided = linedef['flags'] & 0x04 != 0

			vertex_keys = ['start vertex','end vertex']
			if seg['direction'] != 0:
				next_sidedef_index = linedef['right sidedef']
				vertex_keys.reverse()
			else:
				next_sidedef_index = linedef['left sidedef']

			vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],vertex_keys)

			splitter = make_line(vertexes[1], vertexes[0])
			next_cell = None
			if two_sided and next_sidedef_index>=0:
				sidedef = level['SIDEDEFS'][next_sidedef_index]
				sector = level['SECTORS'][sidedef['sector']]
				next_cell = sector
			portal = next_cell is not None

			inserted = False
			current_index = 0
			next_polygon_vertexes = []
			next_polygon_auxes = []
			previous = right_polygon.vertexes[-1]
			for (current,current_aux) in zip(right_polygon.vertexes,right_polygon.auxillaries):
				tangent = current-previous
				previous_dot = tangent.dot(previous)
				current_dot = tangent.dot(current)
				assert(current_dot >= previous_dot)
				if splitter.normal_equation(current)==0 and splitter.normal_equation(previous) == 0:
					inserted = True
					v0 = vertexes[0]
					v1 = vertexes[1]
					t0 = tangent.dot(v0)
					t1 = tangent.dot(v1)
					if t0 > t1:
						(t0,t1) = (t1,t0)
						(v0,v1) = (v1,v0)

					assert(t0<=t1)
					if previous_dot < t0 and t1 < current_dot:
						next_polygon_vertexes.append(v0)
						next_polygon_auxes.append(current_aux)
						next_polygon_vertexes.append(v1)
						next_polygon_auxes.append(portal)
					elif previous_dot < t0 and t0 < current_dot:
						next_polygon_vertexes.append(v0)
						next_polygon_auxes.append(current_aux)
						current_aux = portal
					elif previous_dot < t1 and t1 < current_dot:
						next_polygon_vertexes.append(v1)
						next_polygon_auxes.append(portal)
					elif not (t1 <= previous_dot or current_dot <= t0):
						current_aux = portal
			
				next_polygon_vertexes.append(current)
				next_polygon_auxes.append(current_aux)
				previous = current

			if not inserted:
				right_polygon = right_polygon.clip(splitter,portal,True)['positive']
			else:
				right_polygon = geometry.SimplePolygon(next_polygon_vertexes,next_polygon_auxes)
		#return right_polygon

		if use_sector_snapping:
			if len(right_polygon.vertices)>0:
				for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
					seg_vertexes = map(lambda key: level['VERTEXES'][seg[key]],['start vertex','end vertex'])

					for vertex in seg_vertexes:
						vector = vertex['vector']
						closest = right_polygon.vertexes[0]
						closest_dist = (closest-vector).dot(closest-vector)
						for v in right_polygon.vertexes[1:]:
							dist = (v-vector).dot(v-vector)
							if closest_dist > dist:
								closest_dist = dist
							closest = v
						vertex['vector'] = closest
		return right_polygon

	def modify_subsector(polygon, subsector):
		def compare_norms(a,b):
			a_norm_square = a.dot(a)
			b_norm_square = b.dot(b)
			return cmp(a_norm_square,b_norm_square)

		if len(polygon.vertexes)==0:
			return
		for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
			for vname in ['start vertex','end vertex']:
				v = level['VERTEXES'][seg[vname]]['vector']
				u = polygon.vertexes[0]
				for w in polygon.vertexes[1:]:
					if compare_norms(u-v,w-v)>0:
						u = w
				level['VERTEXES'][seg[vname]]['vector'] = u
				
	def winding(points):
		normal = (points[0]-points[1]).cross(points[2]-points[1])
		up = geometry.Vector(map(rational,[0,0,1]))
		return cmp(normal.dot(up),0)

# Split the polygon along the node-splitter. Continue splitting on child nodes and assign the current polygon for child subsectors.
	def split_polygon(node,polygon):
		polygons = polygon.clip(node['plane'],True,True)

		for polygon_side in ['positive','negative']:
			next_polygon = polygons[polygon_side]
			is_right = polygon_side == 'positive'
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				if use_plot:
					draw_polygon(next_polygon)
				split_polygon(child,next_polygon)
			else:
				flat = next_polygon
				flat = carve_subsector(flat,child)
				modify_subsector(flat,child)
				flat = carve_subsector(flat,child)
				child['polygon'] = flat
				if use_plot:
					draw_polygon(flat)

	def draw_subsectors(node,depth):
		for is_right in [True,False]:
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				draw_subsectors(child,depth+1)
			else:
				draw_subsector(child)

	def compute_bound(vertexes):
		result = geometry.quickhull(vertexes)
		result.reverse()
		return result

	if True:
		bounds = level['VERTEXES'][0]['vector'].elements[:2]
		bounds = [list(bounds),list(bounds)]
		#print bounds[0]
		#print min(bounds[0][0],bounds[0][1])
		#print max(bounds[0][0],bounds[0][1])
		for vertex in level['VERTEXES']:
			xy = vertex['vector'].elements[:2]
			#print x,y,bounds
			#print x,bounds[0][0], min(x,bounds[0][0])

			bounds[0][0] = min(bounds[0][0],xy[0])
			bounds[0][1] = min(bounds[0][1],xy[1])
			bounds[1][0] = max(bounds[1][0],xy[0])
			bounds[1][1] = max(bounds[1][1],xy[1])

		polygon = geometry.SimplePolygon(map(geometry.Vector,						\
			[[bounds[0][0],bounds[0][1],rational(0)],									\
				[bounds[1][0],bounds[0][1],rational(0)],								\
				[bounds[1][0],bounds[1][1],rational(0)],								\
				[bounds[0][0],bounds[1][1],rational(0)]]))
	else:
 		polygon = compute_bound(map(lambda v: v['vector'], level['VERTEXES']))
	
	if use_plot:
		plt.clf()
	split_polygon(level['NODES'][-1],polygon)
	if use_plot:
		for linedef in level['LINEDEFS']:
			if True:
				vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],['start vertex','end vertex'])
				t = map(lambda i: map(lambda v: v.elements[i], vertexes),range(2))
				plt.plot(t[0],t[1],'g')
				plt.plot(t[:][0],t[:][1],'m*')

	if use_plot:
		plt.show()

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

class Texture:
	#texture = Texture({'vectors': [[0,0,-1],side_vector],'displacements':[0,0],'miptex index':miptex_index})
	def __init__(self,vectors,displacements,miptex_index=None):
		self.miptex_index = miptex_index
		self.vectors = vectors
		self.displacements = displacements
	
	def __cmp__(a,b):
		return cmp((a.vectors,a.displacements,a.miptex_index),(b.vectors,b.displacements,b.miptex_index))
	def to_dict(self,finder):
		return {'vectors': map(lambda x:x.to_dict(finder),self.vectors),'displacements': self.displacements.to_dict(finder),'miptex index':self.miptex_index}

class Face:
	def __init__(self,vertexes,texture_index=None):
		self.texture_index = texture_index
		three = Face.find_noncolinear(vertexes)
		if three is None:
			self.plane = geometry.Plane(geometry.Vector(map(rational,[0]*len(vertexes[0].elements))),rational(0))
			self.vertexes = []
		else:
			normal = (three[0]-three[1]).cross(three[2]-three[1])
			self.plane = geometry.Plane(normal,normal.dot(three[0]))
			min_vertex = min(vertexes)
			min_index = vertexes.index(min_vertex)
			self.vertexes = vertexes[min_index:]+vertexes[:min_index]
	@staticmethod
	def is_colinear(vertexes):
		vector1 = vertexes[1] - vertexes[0]
		vector2 = vertexes[2] - vertexes[0]
		return vector1.cross(vector2) == geometry.Vector(map(rational,[0,0,0]))

	@staticmethod
	def find_noncolinear(vertexes):
		if len(vertexes)<3:
			return None

		vertex0 = vertexes[0]
		for i in range(1,len(vertexes)-1):
			vertex1 = vertexes[i]
			for j in range(i+1,len(vertexes)):
				vertex2 = vertexes[j]
				if not Face.is_colinear([vertex0,vertex1,vertex2]):
					return [vertex0,vertex1,vertex2]
		return None

	def __cmp__(a,b):
		return cmp((a.plane,a.vertexes),(b.plane,b.vertexes))
	def to_dict(self,finder):
		return { 'plane id': finder('plane',self.plane),
					'vertices index': map(lambda vertex: finder('vertex',vertex), self.vertexes),
					'front side': True,
					'texture index': self.texture_index
				}
class KeyRecord:
	def __init__(self,key,record):
		self.key = key
		self.record = record
	def __cmp__(a,b):
		return cmp(a.key,b.key)
	def __str__(self):
		return '({},{})'.format(self.key,self.record)
class IndexMap:
	def __init__(self,finder):
		self.forward = AVLTree()
		self.reverse = []
		self.finder = finder
	def find_insert(self,item):
		found = self.forward.find(KeyRecord(item,None))
		if found is not None:
			return found.key.record
		#key = handler(item)
		key = item.to_dict(self.finder)
		key_record = KeyRecord(item,len(self.reverse))
		self.forward.insert(key_record)
		self.reverse.append(key)
		return key_record.record
	def __getitem__(self,index):
		return self.reverse[index]

def to_quake(level,texture_indexes):
	def make_wall_face(planes,vertexes,textures,seg,floor_height,ceiling_height,texture):
		def augment_vertex(index,elevation):
			vertex = geometry.Vector(level['VERTEXES'][index]['vector'].elements)
			vertex.elements[2] = elevation
			return vertex
		linedef = level['LINEDEFS'][seg['linedef']]
		if seg['direction'] == 0:
			sidedef = level['SIDEDEFS'][linedef['right sidedef']]
		else:
			sidedef = level['SIDEDEFS'][linedef['left sidedef']]

		texture_name = sidedef['{} texture'.format(texture)]
		side_vector = augment_vertex(seg['end vertex'],0) - augment_vertex(seg['start vertex'],0)
		norm = rational(math.sqrt(float(side_vector.norm_squared())))

		if norm!=0:
			side_vector = side_vector / norm
		else:
			side_vector = side_vector*norm

		try:
			miptex_index = texture_indexes[texture_name]['index']
		except KeyError:
			miptex_index = None

		up_vector = geometry.Vector([0,0,-1])
		displacement = geometry.Vector(map(rational,[sidedef['x offset'],sidedef['y offset']]))
		texture = Texture([side_vector,up_vector],displacement,miptex_index)
		texture_index = textures.find_insert(texture)

		vertex_indexes = []

		face_vertexes = []	
		face_vertexes.append(augment_vertex(seg['start vertex'],ceiling_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],ceiling_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],floor_height))
		face_vertexes.append(augment_vertex(seg['start vertex'],floor_height))
		vertex_indexes = map(lambda face_vertex: vertexes.find_insert(face_vertex), face_vertexes)
		return Face(face_vertexes,texture_index)
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
	
	visilist = []
	visilist_offsets = []
	def item_finder(item_type,item):
		if item_type == 'vertex':
			return vertexes.find_insert(item)
		elif item_type == 'plane':
			return planes.find_insert(item)
		else:
			return None

	up_vector = geometry.Vector(map(rational,[0,0,1]))
	down_vector = geometry.Vector(map(rational,[0,0,-1]))
	planes = IndexMap(item_finder)
	nodes = []
	for node in level['NODES']:
		if use_log:
			print('node:\t'.format(float(node['index'])*100/len(level['NODES'])))

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

		plane_id = planes.find_insert(node['plane'])
		nodes.append({  'plane id': plane_id,		\
			'children nodes': children_nodes,	\
			'children leaves': children_leaves	\
			})

	leaves = []
	vertexes = IndexMap(item_finder)
	faces = IndexMap(item_finder)
	textures = IndexMap(item_finder)
	for subsector in level['SSECTORS']:
		if use_log:
			print('subsector:\t{}'.format(float(subsector['index'])*100/len(level['SSECTORS'])))
		seg_count = subsector['seg count']
		seg_first = subsector['seg offset']
		sector = subsector['sector']

		face_indexes = []
		floor_vertexes = []
		ceiling_vertexes = []
		floor_height = sector['floor height']
		ceiling_height = sector['ceiling height']
		for seg_index in range(seg_first,seg_count+seg_first):
			seg = level['SEGS'][seg_index]
			if level['LINEDEFS'][seg['linedef']]['flags'] & 0x04 == 0: # Not two sided
				face = make_wall_face(planes,vertexes,textures,seg,floor_height, ceiling_height,'middle')
				if len(face.vertexes)>0:
					face_indexes.append(faces.find_insert(face))
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
					floor_face = make_wall_face(planes,vertexes,textures,seg,floor_height, next_floor_height,'lower')
					if len(floor_face.vertexes)>0:
						face_indexes.append(faces.find_insert(floor_face))
				if next_ceiling_height < ceiling_height:
					ceiling_face = make_wall_face(planes,vertexes,textures,seg,next_ceiling_height,ceiling_height,'upper')
					if len(ceiling_face.vertexes)>0:
						face_indexes.append(faces.find_insert(ceiling_face))

		if len(subsector['polygon'].vertexes)>=3:
			floor_vertexes = map(lambda v: geometry.Vector(v.elements[0:2]+[floor_height]), subsector['polygon'].vertexes)
			ceiling_vertexes = map(lambda v: geometry.Vector(v.elements[0:2]+[ceiling_height]), subsector['polygon'].vertexes)
			floor_vertexes.reverse()
			#ceiling_vertexes.reverse()

			def get_flat(texture_name):
				try:
					miptex_index = texture_indexes[texture_name]['index']
				except KeyError:
					miptex_index = None
					print texture_name
					print texture_indexes[texture_name]

				texture = Texture([geometry.Vector([1,0,0]),geometry.Vector([0,1,0])],geometry.Vector([0,0]),miptex_index)
				return textures.find_insert(texture)

			floor_face = Face(floor_vertexes,get_flat(sector['floor texture']))
			ceiling_face = Face(ceiling_vertexes,get_flat(sector['ceiling texture']))
			face_indexes.append(faces.find_insert(floor_face))
			face_indexes.append(faces.find_insert(ceiling_face))

		leaf = {	'face indexes': face_indexes,
					'visilist start': -1, #visilist_offsets[sector['index']],
					'type': -1
				};
		leaves.append(leaf)

	return {	'nodes': nodes,
				'faces': faces.reverse,
				'leaves': leaves,
				'planes': planes.reverse,
				'vertices': vertexes.reverse,
				'textures': textures.reverse,
				'visibility list': None, # visilist
				'root node': len(nodes)-1
				}


wadname = sys.argv[1]

use_plot = False
use_log = False
use_sector_snapping = False

with open(wadname,'r') as wad_file:

	HEADER_SIZE = 4+4+4
	wad_header = wad_file.read(HEADER_SIZE)
	identification,numlumps,infotableofs = struct.unpack('4sII',wad_header)

	if wadname.split('/')[-1] != 'hexen.wad':
		wadtype = 'doom'
	else:
		wadtype = 'hexen'

	print identification
	print numlumps
	print infotableofs
	processing_levels = False
	processing_flats = False
	processing_patches = False
	processing_sprites = False
	mapnames = re.compile('MAP\d\d|E\dM\d')
	textures_names = re.compile('TEXTURE\d')
	patches_start_names = re.compile('P\d?_START')
	patches_end_names = re.compile('P\d?_END')
	flats_start_names = re.compile('F\d?_START')
	flats_end_names = re.compile('F\d?_END')
	sprites_start_names = re.compile('S\d?_START')
	sprites_end_names = re.compile('S\d?_END')

	#patches_start_names = ['P_START','P1_START','P2_START','P3_START']
	#patches_end_names = ['P_END','P1_END','P2_END','P3_END']
	#flats_start_names = ['F_START','F1_START','F2_START']
	#flats_end_names = ['F_END','F1_END','F2_END']
	#sprites_start_names = ['S_START']
	#sprites_end_names = ['S_END']
	patches = {}
	flats = {}
	sprites = {}
	patch_names = []
	texture_definitions = {}
	FLAT_WIDTH = 64
	for i in range(numlumps):
		DIRECTORY_ENTRY_SIZE = 4+4+8
		wad_file.seek(i*DIRECTORY_ENTRY_SIZE+infotableofs)
		directory_entry = wad_file.read(DIRECTORY_ENTRY_SIZE)
		filepos,size,name = struct.unpack('II8s',directory_entry)

		name = strip_null(name)
		#print 'lump: {}'.format(name)

		#print name,size
		if not mapnames.match(name) is None:
			if size>0:
				wad_file.seek(filepos)
				print strip_null(wad_file.read(size))

			processing_levels = True
			level_name = name
			levels[level_name] = {}
			continue

		if patches_start_names.match(name) is not None:
			processing_patches = True
			continue
		elif patches_end_names.match(name) is not None:
			processing_patches = False
			continue
		elif flats_start_names.match(name) is not None:
			processing_flats = True
			continue
		elif flats_end_names.match(name) is not None:
			processing_flats = False
			continue
		elif sprites_start_names.match(name) is not None:
			processing_sprites = True
			continue
		elif sprites_end_names.match(name) is not None:
			processing_sprites = False
			continue
		if processing_patches:
			if use_log:
				print('patch: {}'.format(name))
			patch_name = name.lower()
			patches[patch_name] = read_image(patch_name,filepos,wad_file)
		elif processing_sprites:
			if use_log:
				print('sprite: {}'.format(name))
			sprite_name = name.lower()
			sprites[sprite_name] = read_image(sprite_name,filepos,wad_file)
		elif processing_flats:
			if use_log:
				print('flat: {}'.format(name))
			wad_file.seek(filepos)
			data = map(ord,struct.unpack('{}c'.format(FLAT_WIDTH*FLAT_WIDTH),wad_file.read(FLAT_WIDTH*FLAT_WIDTH)))
			flat_name = name.lower()
			flats[flat_name] = {'name': flat_name,'width': FLAT_WIDTH, 'height': FLAT_WIDTH,'data': data}
		elif name=='PLAYPAL':
			wad_file.seek(filepos)
			playpal = []
			PLAYPAL_COUNT=14
			for i in range(PLAYPAL_COUNT):
				current = []
				for j in range(256):
					r,g,b = map(ord,struct.unpack('3c',wad_file.read(3)))
					current.append((r,g,b))
				playpal.append(current)
		elif name=='PNAMES':
			wad_file.seek(filepos)
			map_patches_count = struct.unpack('I',wad_file.read(4))[0]
			for i in range(map_patches_count):
				patch_name = struct.unpack('8s',wad_file.read(8))[0]
				patch_names.append(strip_null(patch_name).lower())
		elif textures_names.match(name) is not None:
			wad_file.seek(filepos)
			texture_count = struct.unpack('I',wad_file.read(4))[0]
			print texture_count
			offsets = struct.unpack('{}I'.format(texture_count),wad_file.read(4*texture_count))
			for offset in offsets:
				wad_file.seek(filepos+offset)
				TEXTURE_SIZE = 8+4+2+2+4+2
				texture_name,masked,width,height,column_directory,patch_count = \
					struct.unpack('8sIhhIh',wad_file.read(TEXTURE_SIZE))
				texture_name = strip_null(texture_name).lower()
				patches_definition = []
				for i in range(patch_count):
					originx,originy,patch_index,stepdir,colormap = struct.unpack('5h',wad_file.read(5*2))
					patches_definition.append({ 'origin':[originx,originy],'patch index':patch_index })

				texture_definitions[texture_name] = {	\
					'name': texture_name,				\
					'width': width,							\
					'height': height,							\
					'patches': patches_definition }

		elif processing_levels:
			if level_lumps.count(name) > 0:
				#print '\t' + name
				levels[level_name][name] = process_level_lump(name,filepos,wad_file,wadtype)
			else:
				processing_levels = False

	def make_texture_atlas(resources,size,padding):
		atlas = tex.TextureAtlas({'left':0,'top':0,'width':size,'height':size})
		for (_,resource) in resources.items():
			data = resource['data']
			result_width = resource['width']+padding*2
			result_height = resource['height']+padding*2
			result = [TRANSPARENT_COLOR]*result_width*result_height
			for x in range(result_width):
				for y in range(result_height):
					tx = (x-padding)%resource['width']
					ty = (y-padding)%resource['height']
					color = data[ty*resource['width']+tx]
					if color != TRANSPARENT_COLOR:
						result[y*result_width+x] =	color
			resource['data'] = result
			resource['width'] = result_width
			resource['height'] = result_height

		atlas.insert_many(map(lambda key: resources[key],resources))

		data = [TRANSPARENT_COLOR] * size*size
		atlas.render(data,size,size)

		def handler(rect,resource,current):
			name = resource['name']
			entry = {'begin': [rect['left']+padding,rect['top']+padding],					\
						'size':[resource['width']-padding*2,resource['height']-padding*2],				\
						'texture name':name,'index':current['index']}
			current['texture indexes'][name] =  entry
			current['index'] += 1
			return current

		texture_indexes = atlas.traverse(handler,{'index':1,'texture indexes':{}})['texture indexes']

		writer = png.Writer(width=size,height=size,palette=playpal[0])
		with open('{}/{}.png'.format('images','texture'),'w') as image_file:
			writer.write_array(image_file,data)
		return texture_indexes

	#make_texture_atlas(patches,2048)
	#texture_index = make_texture_atlas(dict(sprites.items()+patches.items()+flats.items()),512*8)

	wall_textures = {}
	resources = dict(sprites.items() + patches.items() + flats.items())
	for (name,texture) in texture_definitions.items():
		texture_width = texture['width']
		texture_height = texture['height']

		texture_data = [TRANSPARENT_COLOR] * texture_width * texture_height
		for patch in texture['patches']:
			patch_name = patch_names[patch['patch index']]
			try:
				patch_image = resources[patch_name]
				patch_data = patch_image['data']
				patch_width = patch_image['width']
				patch_height = patch_image['height']

				origin = patch['origin']
				for x in range(patch_width):
					tx = origin[0]+x
					if 0 <= tx and tx < texture_width:
						for y in range(patch_height):
							ty = origin[1]+y
							if 0 <= ty and ty < texture_height:
								texture_data[ty*texture_width+tx] = patch_data[y*patch_width+x]
			except KeyError:
				print patch_name

		wall_textures[name] = {			\
			'name': name,					\
			'width': texture_width,		\
			'height': texture_height,	\
			'data': texture_data }

	print sorted(flats.keys())
	texture_index = make_texture_atlas(dict(wall_textures.items()+flats.items()),4096,2)

	def write_image(resource_type,image):
		writer = png.Writer(width=image['width'],height=image['height'],palette=playpal[0])
		#print image['data']
		#print playpal[0]
		with open('{}/{}.png'.format(resource_type,image['name']),'w') as image_file:
			writer.write_array(image_file,image['data'])

	index = 0
	for key in patches:
		write_image('images/patches',patches[key])

	for key in flats:
		write_image('images/flats',flats[key])
	for key in sprites:
		write_image('images/sprites',sprites[key])

	def output_texture_index(indexes,levelset):
		result = {}
		for (_,entry) in indexes.items():
			result[entry['index']] = entry
		texture_index = {}
		for levelname in levelset:
			filename = levelname.lower()+'.bsp'
			texture_index[filename] = result
		return texture_index

	levelnames = levels.keys()
	levelnames.sort()
	levelset = sys.argv[2:]
	levelset = levelnames

	with open('images/' + 'texture_index' + '.json','w') as fp:
		json.dump(output_texture_index(texture_index,levelset),fp)

	#for levelname in levelnames:
	for levelname in levelset:
		quake = {}
		level = levels[levelname]
		print levelname
		level = normalize_level(level)

		quake = to_quake(level,texture_index)

		player_origin = [0,0,0]
		for thing in level['THINGS']:
			if thing['type'] == 1:
				player_origin = [thing['x'],thing['y'],0]
				break
		def find_node(level,pos):
			current_node = level['NODES'][-1]
			is_node = True
			while is_node:
				is_right = current_node['plane'].normal_equation(pos)>0
					  				
				(is_node,current_node) = select_node_child(current_node,is_right,level)
			return current_node

		player_origin[2] = float(find_node(level,geometry.Vector(map(rational,player_origin)))['sector']['floor height']+10)

		quake['player start'] = {	'origin': player_origin,
											'angle': 0
											}

		quake['filename'] = levelname.lower() + '.bsp'
		#quake['textures'] = [{	'displacements': [0,0],
		#								'miptex index': null,
		#								'vectors': [[1,0,0],[0,1,0]]
		#								}]

		#print len(quake['vertices'])
		#print quake['vertices']
		with open('doom/' + levelname.lower() + '.json','w') as fp:
			json.dump(quake,fp)

	print 'Finished'

