
from clnum import *
import random
#import numpy as np
import matplotlib.pyplot as plt
import math

graphics = False
def sign(x):
	return cmp(x,0)
def rational(x):
	return mpq(x,1)
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


class SimplePolygon:
	def __init__(self,vertexes,auxillaries=None):
		self.vertexes = list(vertexes)
		if auxillaries is not None:
			self.auxillaries = list(auxillaries)
		else:
			self.auxillaries = [None] * len(vertexes)
	def reverse(self):
		self.vertexes.reverse()
		self.auxillaries.reverse()

# Return the two polygons that result from the split.
	def clip(polygon,splitter,right_aux_default=None,left_aux_default=None):
		if len(polygon.vertexes) == 0:
			return (polygon,polygon)
		right_vertexes = []
		right_auxes = []
		left_vertexes = []
		left_auxes = []
		previous = polygon.vertexes[-1]

		previous_dot = splitter.normal_equation(previous) 
		for (current,current_aux) in zip(polygon.vertexes,polygon.auxillaries):
			current_dot = splitter.normal_equation(current) 

			if current_dot*previous_dot < rational(0):
				intersection_result = intersect_segment(splitter,current,previous)
				if intersection_result:
					(t,intersection) = intersection_result
					#print t,intersection_dot
					if rational(0) <= t and t <= rational(1): 
						right_aux = right_aux_default
						left_aux = left_aux_default
						if previous_dot>0:
							right_aux = current_aux
						if previous_dot<0:
							left_aux = current_aux
						right_vertexes.append(intersection)
						right_auxes.append(right_aux)
						left_vertexes.append(intersection)
						left_auxes.append(left_aux)
			if current_dot >= rational(0):
				#if current_dot == 0 and previous_dot == 0:
				if current_dot == 0 and previous_dot <= 0:
					aux = right_aux_default
				else:
					aux = current_aux
				right_vertexes.append(current)
				right_auxes.append(aux)

			if current_dot <= rational(0):
				#if current_dot == 0 and previous_dot == 0:
				if current_dot == 0 and previous_dot >= 0:
					aux = left_aux_default
				else:
					aux = current_aux 
				left_vertexes.append(current)
				left_auxes.append(aux)

			(previous_dot,previous) = (current_dot,current)
		return {	'positive' : SimplePolygon(right_vertexes,right_auxes),			\
					'negative': SimplePolygon(left_vertexes,left_auxes)}

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
	def __cmp__(a,b):
		return cmp(a.elements,b.elements)
	def __mul__(a,s):
		return Vector(map(lambda x: x*s, a.elements))
	def __div__(a,s):
		return Vector(map(lambda x: x/s, a.elements))
	def to_dict(self,finder):
		return map(float,self.elements)

class Plane:
	def __init__(self,normal,dist):
		self.normal = normal
		self.dist = dist
		#self.norm_squared = normal.dot(normal)

	def normal_equation(self,vector):
		return self.normal.dot(vector) - self.dist

	def __cmp__(a,b):
		#print a,b
		a_dist = abs(a.dist)
		b_dist = abs(b.dist)
		a_normal = map(lambda x: x*b_dist, a.normal.elements)
		b_normal = map(lambda x: x*a_dist, b.normal.elements)
		a_normal.append(sign(a.dist)*b_dist)
		b_normal.append(sign(b.dist)*a_dist)
		return cmp(a_normal,b_normal)

	def __str__(self):
		return '{' + str(self.normal) + '.p=' +  str(self.dist)+ '}'
	def to_dict(self,finder):
		return { 'normal': self.normal.to_dict(finder), 'dist': float(self.dist)}


def make_line(start,end):
	tangent = end-start;
	normal = Vector([tangent.elements[1],-tangent.elements[0]])
	dist  = normal.dot(end)
	return Plane(normal,dist)

def quickhull(points):
	def recurse(start,end,candidates):
		line = make_line(start,end)
		positives = []
		positive_max = None
		positive_max_value = 0
		for v in candidates:
			dot_value = line.normal_equation(v)
			if dot_value>0:
				positives.append(v)
				if dot_value>positive_max_value:
					positive_max_value = dot_value
					positive_max = v
		if len(positives)>0:
			p = positive_max
			c = start
			p = positive_max
			c = end
			return recurse(positive_max,end,positives)+[positive_max]+recurse(start,positive_max,positives)
		else:
			return []


	def sort_vertical(vertexes,direction):
		key_y = lambda v: v.elements[1]
		max_index = len(vertexes)-1
		for i in range(len(vertexes)-1):
			if (vertexes[i].elements[1]-vertexes[i+1].elements[1])*direction>0:
				max_index = i
				break
		# Sort the right quadrant.
		for i in range(0,max_index-1):
			if (vertexes[i].elements[1]-vertexes[i+1].elements[1])*direction>0:
				t = vertexes[i:i+1]
				t.reverse()
				vertexes[i:i+1] = t
		# Sort the left quadrant.
		for i in range(max_index+1,len(vertexes)-1):
			if (vertexes[i].elements[1]-vertexes[i+1].elements[1])*direction<0:
				t = vertexes[i:i+1]
				t.reverse()
				vertexes[i:i+1] = t

	key_x = lambda v: v.elements[0]
	working = sorted(points)
	p = working[0]
	c = working[-1]
	upper = sorted(recurse(working[-1],working[0],working),key=key_x)
	upper += [working[-1]]
	sort_vertical(upper,1)

	lower = sorted(recurse(working[0],working[-1],working),key=lambda v: -key_x(v))
	lower += [working[0]]
	sort_vertical(lower,-1)
	result = upper+lower
	return SimplePolygon(result)

def test_convex_hull_2d(count):
	points = []
	for i in range(count):
		elements = []
		for j in range(2):
			den = random.randint(1,2**48)
			#num = random.randint(0,den-1)
			num = int(random.random() * den)
			#num = int(random.gauss(1,0.25)*den)

			elements.append(mpq(num,den))
		p = Vector(elements)
		#points.append(p)

	for i in range(count):
		den = 2**48
		angle = i*math.pi*2/count
		r = random.gauss(1,0.1)
		cs = (math.cos(angle)*r+1)/2
		sn = (math.sin(angle)*r+1)/2
		cs = int(den*cs)
		sn = int(den*sn)

		p = Vector([mpq(cs,den),mpq(sn,den)])
		points.append(p)


	plt.clf()
	#hull = convex_hull_2d(points)
	hull = quickhull(points)
	if graphics:
		for c in points:
			plt.plot(float(c.elements[0]),float(c.elements[1]),'r.')
		#c = hull[-1]
		#plt.plot([float(c.elements[0])],[float(c.elements[1])],'b*')
		p = hull[-1]
		for c in hull:
			plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
			plt.plot([float(c.elements[0])],[float(c.elements[1])],'b*')
			p = c
		plt.show()

#graphics = True
#while True:
#	test_convex_hull_2d(500)

