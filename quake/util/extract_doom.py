#!/usr/bin/python

import matplotlib.pyplot as plt
import struct
import re
import json
import clnum
import pyavltree
import geometry
from pyavltree import AVLTree
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

def draw_polygon(polygon):
	t = map(lambda i: map(lambda (p,_): p.elements[i], polygon),range(2))
	portals = map(lambda (_,portal): portal, polygon)

	for i in range(len(polygon)):
		xs = [t[0][i],t[0][(i-1)%len(polygon)]]
		ys = [t[1][i],t[1][(i-1)%len(polygon)]]
		if portals[i]:
			plt.plot(xs,ys,'r:')
		else:
			plt.plot(xs,ys,'b',linewidth=3)

	#if is_right:
	#	plt.fill(t[0],t[1],'r',fill=False,linestyle='dotted')
	#else:
	#	plt.fill(t[0],t[1],'b',fill=False,linestyle='dotted')

	#for k in range(len(polygon)):
	#	t = map(lambda i: map(lambda j: polygon[j%len(polygon)].elements[i], [k-1,k]),range(2))
	#	if depth&1 == 0:
	#		plt.plot(t[0],t[1],'r-.')
	#	else:
	#		plt.plot(t[0],t[1],'b-.')

	#pygame.draw.polygon(window,(((depth&1)^1)*color_level,0,(depth&1)*color_level),pointlist)
	#time.sleep(10/1000.0)
	#pygame.display.flip()

def draw_subsector(subsector,bounds):
	extents = max(bounds[1][0]-bounds[0][0],bounds[1][1]-bounds[0][1])
	scale_x = extents
	scale_y = extents
	for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
		linedef = level['LINEDEFS'][seg['linedef']]
		#vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],['start vertex','end vertex'])
		vertexes = map(lambda key: level['VERTEXES'][seg[key]]['vector'],['start vertex','end vertex'])
		#px = float((vertexes[0].elements[0]-bounds[0][0])/scale_x)*window_size+window_padding
		#py = float(1-(vertexes[0].elements[1]-bounds[0][1])/scale_y)*window_size+window_padding
		#cx = float((vertexes[1].elements[0]-bounds[0][0])/scale_x)*window_size+window_padding
		#cy = float(1-(vertexes[1].elements[1]-bounds[0][1])/scale_y)*window_size+window_padding

		#pygame.draw.line(window,(0,255,0),(px,py),(cx,cy))
		if linedef['flags']&0x04 != 0:
			t = map(lambda i: map(lambda v: v.elements[i], vertexes),range(2))
		#plt.plot([px,cx],[-py,-cy],'g')
			plt.plot(t[0],t[1],'g')
			plt.plot(t[:][0],t[:][1],'m*')
	#time.sleep(10/1000.0)
	#pygame.display.flip()

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
		#if subsector['index'] == 166:
		#	plt.clf()
		#	draw_polygon(polygon)
		#	plt.show()
		for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
		#for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(min(2,subsector['seg count']))):
			linedef = level['LINEDEFS'][seg['linedef']]
			two_sided = linedef['flags'] & 0x04 != 0

			vertex_keys = ['start vertex','end vertex']
			if seg['direction'] != 0:
				next_sidedef_index = linedef['right sidedef']
				vertex_keys.reverse()
			else:
				next_sidedef_index = linedef['left sidedef']

			#vertexes = map(lambda key: level['VERTEXES'][seg[key]]['vector'],['start vertex','end vertex'])
			vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],vertex_keys)
			splitter = make_line(vertexes[1], vertexes[0])
			next_cell = None
			if two_sided and next_sidedef_index>=0:
				sidedef = level['SIDEDEFS'][next_sidedef_index]
				sector = level['SECTORS'][sidedef['sector']]
				next_cell = sector
			portal = next_cell is not None
			#portal = False

			inserted = False
			current_index = 0
			next_polygon = []
			(previous,_) = right_polygon[-1]
			#while current_index<len(right_polygon):
				#(previous,_) = right_polygon[(current_index-1)%len(right_polygon)]
			for (current,current_aux) in right_polygon:
				#(current,current_aux) = right_polygon[current_index]
				#line = make_line(current,previous)
				tangent = current-previous
				previous_dot = tangent.dot(previous)
				current_dot = tangent.dot(current)
				assert(current_dot > previous_dot)
				#if line.normal_equation(vertexes[0]) == 0 and line.normal_equation(vertexes[1]) == 0:
				#if splitter.normal.dot(tangent) == 0 and splitter.normal_equation(previous) == 0:
				if splitter.normal_equation(current)==0 and splitter.normal_equation(previous) == 0:
					inserted = True
					v0 = vertexes[0]
					v1 = vertexes[1]
					t0 = tangent.dot(v0)
					t1 = tangent.dot(v1)
					if t0 > t1:
						(t0,t1) = (t1,t0)
						(v0,v1) = (v1,v0)

					assert(t0<t1)
					if previous_dot < t0 and t1 < current_dot:
						#right_polygon.insert(current_index,(v1,portal))
						#right_polygon.insert(current_index,(v0,current_aux))
						#current_index+=2
						next_polygon.append((v0,current_aux))
						next_polygon.append((v1,portal))
					elif previous_dot < t0 and t0 < current_dot:
						#right_polygon[current_index] = (current,portal)
						#right_polygon.insert(current_index,(v0,current_aux))
						#current_index+=1
						next_polygon.append((v0,current_aux))
						current_aux = portal
					elif previous_dot < t1 and t1 < current_dot:
						#right_polygon.insert(current_index,(v1,portal))
						#current_index+=1
						next_polygon.append((v1,portal))
					elif not (t1 <= previous_dot or current_dot <= t0):
						current_aux = portal
					#elif previous_dot == t0 and current_dot < t1:
					#	current_aux = portal
					#elif previous_dot > t0 and current_dot == t1:
					#	current_aux = portal
			 		#elif previous_dot == t0 and current_dot == t1:
					#	current_aux = portal
			 		#elif previous_dot > t0 and t1 > current_dot:
					#	current_aux = portal

				next_polygon.append((current,current_aux))
				previous = current
						#right_polygon[current_index] = (current,portal)
				#current_index+=1
			#inserted = False
			if not inserted:
				(right_polygon,_) = clip_polygon(right_polygon,splitter,rational(1),portal,True)
			else:
				right_polygon = next_polygon
			#if subsector['index'] == 166:
			#	plt.clf()
			#	draw_polygon(right_polygon)
			#	vertex_keys = ['start vertex','end vertex']
			#	vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],vertex_keys)
			#	t = map(lambda i: map(lambda (p): p.elements[i], vertexes),range(2))
			#	plt.plot(t[0],t[1],'m',linewidth=3)
			#	plt.show()
				#plt.text(s.elements[0],s.elements[1],str(subsector['index']))
		#(s,_) = right_polygon[0]
		#for (v,_) in right_polygon[1:]:
		#	s+=v
		#s /= len(right_polygon)
			
		return right_polygon
	def modify_subsector(polygon, subsector):
		def compare_norms(a,b):
			a_norm_square = a.dot(a)
			b_norm_square = b.dot(b)
			return cmp(a_norm_square,b_norm_square)

		if len(polygon)==0:
			return
		for seg in map(lambda index: level['SEGS'][index+subsector['seg offset']],range(subsector['seg count'])):
			#vertexes = map(lambda key: level['VERTEXES'][seg[key]]['vector'],['start vertex','end vertex'])
			#for v in vertexes:
			for vname in ['start vertex','end vertex']:
				v = level['VERTEXES'][seg[vname]]['vector']
				u = polygon[0]
				for w in polygon[1:]:
					if compare_norms(u-v,w-v)>0:
						u = w
				if (u-v).dot(u-v)<1:
					level['VERTEXES'][seg[vname]]['vector'] = u
				
	def winding(points):
		normal = (points[0]-points[1]).cross(points[2]-points[1])
		up = geometry.Vector(map(rational,[0,0,1]))
		return cmp(normal.dot(up),0)

	def split_polygon(node,depth,polygon):
		#print polygon
		#(right_polygon,left_polygon,right_intersections,left_intersections) = clip_polygon(polygon,node['plane'],rational(1),True,True)
		(right_polygon,left_polygon) = clip_polygon(polygon,node['plane'],rational(1),True,True)
		#print winding(map(lambda (v,_): v,right_polygon)),winding(map(lambda (v,_): v,left_polygon))

		#for right_index in right_intersections:
		#	(vertex,_) = right_polygon[right_index]
		#	right_polygon[right_index] = (vertex,True)

		#for left_index in left_intersections:
		#	(vertex,_) = left_polygon[left_index]
		#	left_polygon[left_index] = (vertex,True)

		for (next_polygon,is_right) in zip([right_polygon,left_polygon],[True,False]):
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				draw_polygon(next_polygon)
				split_polygon(child,depth+1,next_polygon)
			else:
				flat = next_polygon
				#modify_subsector(flat,child)
				flat = carve_subsector(flat,child)
				#print winding(map(lambda (v,_): v,flat))
				child['polygon'] = flat
				draw_polygon(flat)

	def draw_subsectors(node,depth):
		for is_right in [True,False]:
			(is_node,child) = select_node_child(node,is_right,level)
			if is_node:
				draw_subsectors(child,depth+1)
			else:
				draw_subsector(child,bounds)


	def compute_bound(vertexes):
		result = geometry.quickhull(vertexes)
		result.reverse()
		return result

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

	#polygon = map(Vector,													\
	#	[[bounds[0][0],bounds[0][1],rational(0)],		\
	#		[bounds[1][0],bounds[0][1],rational(0)],		\
	#		[bounds[1][0],bounds[1][1],rational(0)],		\
	#		[bounds[0][0],bounds[1][1],rational(0)]]))
	polygon = map(lambda v: (v,True),compute_bound(map(lambda v: v['vector'], level['VERTEXES'])))
	#pygame.init()
	plt.clf()
	split_polygon(level['NODES'][-1],0,polygon)
	#draw_subsectors(level['NODES'][-1],0)
	#for linedef in level['LINEDEFS']:
	#	if linedef['flags']&0x04 == 0:
	#	#if True:
	#		vertexes = map(lambda key: level['VERTEXES'][linedef[key]]['vector'],['start vertex','end vertex'])
	#		t = map(lambda i: map(lambda v: v.elements[i], vertexes),range(2))
	#	#plt.plot([px,cx],[-py,-cy],'g')
	#		plt.plot(t[0],t[1],'g')
	#		plt.plot(t[:][0],t[:][1],'m*')

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

class Face:
	def __init__(self,vertexes):
		three = Face.find_noncolinear(vertexes)
		if three is None:
			self.plane = geometry.Plane(geometry.Vector(map(rational,[0]*len(vertexes[0].elements))),rational(0))
			self.vertexes = []
		else:
			normal = (three[2]-three[1]).cross(three[0]-three[1])
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
		print vertexes
		return None

	def __cmp__(a,b):
		return cmp((a.plane,a.vertexes),(b.plane,b.vertexes))
	def to_dict(self,finder):
		return { 'plane id': finder('plane',self.plane),
					'texture index': 0,
					'vertices index': map(lambda vertex: finder('vertex',vertex), self.vertexes),
					'front side': True
				}
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

def clip_polygon(polygon,splitter,side,right_aux_default,left_aux_default):
	if len(polygon) == 0:
		return (polygon,polygon)
	right = []
	left = []
	(previous,_) = polygon[-1]

	previous_dot = splitter.normal_equation(previous) * side
	for (current,current_aux) in polygon:
		current_dot = splitter.normal_equation(current) * side

		if current_dot*previous_dot < rational(0):
			intersection_result = intersect_segment(splitter,current,previous)
			if intersection_result:
				(t,intersection) = intersection_result
				#print t,intersection_dot
				if rational(0) <= t and t <= rational(1): 
					right_aux = right_aux_default
					left_aux = left_aux_default
					if previous_dot>rational(0):
						right_aux = current_aux
					if previous_dot<rational(0):
						left_aux = current_aux
					right.append((intersection,right_aux))
					left.append((intersection,left_aux))
		if current_dot >= rational(0):
			#if current_dot == 0 and previous_dot == 0:
			if current_dot == 0 and previous_dot <= 0:
				aux = right_aux_default
			else:
				aux = current_aux
			right.append((current,aux))

		if current_dot <= rational(0):
			#if current_dot == 0 and previous_dot == 0:
			if current_dot == 0 and previous_dot >= 0:
				aux = left_aux_default
			else:
				aux = current_aux 
			left.append((current,aux))

		(previous_dot,previous) = (current_dot,current)
	return (right,left)

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

def to_quake(level):
	#def find_item(item_type, items,item):
	#	return items.find_insert(item)
	def make_wall_face(planes,vertexes,seg,floor_height,ceiling_height):
		def augment_vertex(index,elevation):
			vertex = geometry.Vector(level['VERTEXES'][index]['vector'].elements)
			vertex.elements[2] = elevation
			return vertex

		vertex_indexes = []

		face_vertexes = []	
		face_vertexes.append(augment_vertex(seg['start vertex'],floor_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],floor_height))
		face_vertexes.append(augment_vertex(seg['end vertex'],ceiling_height))
		face_vertexes.append(augment_vertex(seg['start vertex'],ceiling_height))
		vertex_indexes = map(lambda face_vertex: vertexes.find_insert(face_vertex), face_vertexes)
		#if seg['direction'] != 0:
		#	vertex_indexes.reverse()
		
		#normal = cross_product(map(lambda i: subtract_vectors(vertexes[i],vertex0),vertex_indexes[1:3]))
		#dist = dot_product(normal,vertex0)
		#normal = (face_vertexes[1]-face_vertexes[0]).cross(face_vertexes[2]-face_vertexes[0])
		#dist = normal.dot(face_vertexes[0])
		return Face(face_vertexes)
		#return { 'plane id': find_item('plane',planes, Plane(normal, dist)),
		#			'texture index': 0,
		#			'vertices index': vertex_indexes,
		#			'front side': True
		#			}
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

	def item_finder(item_type,item):
		if item_type == 'vertex':
			return vertexes.find_insert(item)
		elif item_type == 'plane':
			return planes.find_insert(item)
		else:
			return None

	up_vector = geometry.Vector(map(rational,[0,0,1]))
	down_vector = geometry.Vector(map(rational,[0,0,-1]))
	# 'planes'
	# 'normal','dist'
	#planes = IndexMap(lambda item_type,item: None)
	planes = IndexMap(item_finder)
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

		partition_line_vertex0 = geometry.Vector(map(rational,[node['partition line x'], node['partition line y'],0]))
		partition_line_vertex1 = partition_line_vertex0 + \
			geometry.Vector(map(rational,[node['partition line delta x'], node['partition line delta y'],0]))
		plane = make_line(partition_line_vertex1, partition_line_vertex0)

		#print dist,normal
		#plane_id = find_item('plane',planes, plane)
		plane_id = planes.find_insert(plane)
		node['plane'] = planes[plane_id]
		nodes.append({  'plane id': plane_id,		\
			'children nodes': children_nodes,	\
			'children leaves': children_leaves	\
			})


	
	leaves = []
	#vertexes = []
	#vertexes = IndexMap(lambda item_type,item: None)
	vertexes = IndexMap(item_finder)
	#faces = []
	faces = IndexMap(item_finder)
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
				#print face.vertexes
				if len(face.vertexes)>0:
					#face_indexes.append(find_item('face',faces,face))
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
					floor_face = make_wall_face(planes,vertexes,seg,floor_height, next_floor_height)
					if len(floor_face.vertexes)>0:
						face_indexes.append(faces.find_insert(floor_face))
						#face_indexes.append(find_item('face',faces,floor_face))
				if next_ceiling_height < ceiling_height:
					ceiling_face = make_wall_face(planes,vertexes,seg,next_ceiling_height,ceiling_height)
					if len(ceiling_face.vertexes)>0:
						face_indexes.append(faces.find_insert(ceiling_face))
						#face_indexes.append(find_item('face',faces,ceiling_face))

		if len(subsector['polygon'])>=3:
			floor_vertexes = map(lambda v: geometry.Vector(v.elements[0:2]+[floor_height]), subsector['polygon'])
			ceiling_vertexes = map(lambda v: geometry.Vector(v.elements[0:2]+[ceiling_height]), subsector['polygon'])
			ceiling_vertexes.reverse()

			for vertex in floor_vertexes:
				vertex.elements[2] = floor_height
			for vertex in ceiling_vertexes:
				vertex.elements[2] = ceiling_height
			floor_face = Face(floor_vertexes)
			ceiling_face = Face(ceiling_vertexes)
			face_indexes.append(faces.find_insert(floor_face))
			face_indexes.append(faces.find_insert(ceiling_face))

		leaf = {	'face indexes': face_indexes,
					'visilist start': -1, #visilist_offsets[sector['index']],
					'type': -1
				};
		leaves.append(leaf)
	#def project_down(v):
	#	return map(lambda x: float(x), v.elements)
	#vertexes = map(project_down, vertexes)

	return {	'nodes': nodes,
				'faces': faces.reverse,
				'leaves': leaves,
				'planes': planes.reverse,
				'vertices': vertexes.reverse,
				'visibility list': None, # visilist
				'root node': len(nodes)-1
				}

with open('doom.wad','r') as wad_file:
	mapnames = re.compile('MAP\d\d|E\dM\d')

	HEADER_SIZE = 4+4+4
	wad_header = wad_file.read(HEADER_SIZE)
	identification,numlumps,infotableofs = struct.unpack('4sII',wad_header)

	wadtype = 'doom'
	#wadtype = 'hexen'

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
	#window = pygame.display.set_mode((window_size+2*window_padding,window_size+2*window_padding))
	levelnames = levels.keys()
	levelnames.sort()
	for levelname in levelnames:
	#for levelname in ['MAP01']:
	#for levelname in []:
		#window.fill((0,0,0))
		quake = {}
		level = levels[levelname]
		print levelname
		normalize_level(level)['VERTEXES']
		#time.sleep(5)

		continue

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

