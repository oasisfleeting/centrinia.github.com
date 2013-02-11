
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

def sign(x):
	if x > 0:
		return 1
	elif x < 0:
		return -1
	else:
		return 0

def make_line(start,end):
	tangent = end-start;
	normal = Vector([tangent.elements[1],-tangent.elements[0]])
	dist  = normal.dot(end)
	return Plane(normal,dist)

def convex_hull_2d(points):
	# Return -1 if it is counterclockwise, 1 if it is clockwise, and 0 if it is degenerate
	def triangle_direction(vertexes):
		v0 = vertexes[0] - vertexes[1]
		v1 = vertexes[2] - vertexes[1]
		return sign(v0.elements[0]*v1.elements[1]-v0.elements[1]*v1.elements[0])
	def fan_binsearch(polygon, point):
		low = 0
		high = len(polygon)
		while low+1<high:
			mid = low+(high-low)/2
			comparison = triangle_direction([polygon[0], polygon[mid], point])
			if comparison<0:
				low = mid
			else:
				high = mid

		if low == 0:
			return (low,low+1)
		if high == len(polygon):
			return (high-1,0)

		if triangle_direction([polygon[low], polygon[high], point])<0:
			return None
		else:
 			return (low,high)

	def fan_bitonic_binsearch(polygon,start,side):
		sample_vector = polygon[0]-start
		def objective(end):
			vec = end-start
			cs = vec.dot(sample_vector)
			sn = (vec.elements[0]*sample_vector.elements[1]-vec.elements[1]*sample_vector.elements[0])*side
			#nm = sqrt(float(vec.dot(vec))*float(sample_vector.dot(sample_vector)))
			sn = float(sn)
			cs = float(cs)

			rad = math.atan2(sn,cs)
			if rad>math.pi:
				rad -= math.pi*2
			return rad

			# Point toward the left
			# This normal must be normalized.
			#normal = Vector((end-start).elements[1::-1])
			#normal.elements[0] = -normal.elements[0]
			##return (normal.dot(sample_vector),normal.dot(normal))
			#return float(normal.dot(sample_vector))/sqrt(float(normal.dot(normal)))

		index = 0
		#print objective(polygon[index])
		for i in range(1,len(polygon)):
			mid_dot = objective(polygon[i])
			midp1_dot = objective(polygon[index])
			comparison = cmp(mid_dot,midp1_dot)
			if comparison > 0:
				index = i
		p = polygon[index]
		index2 = None
		print 'index: ' + str(index)
		done = False
		while not done:
			for i in range(len(polygon)):
				c = polygon[i]
				if triangle_direction([p,start,c]) > 0:
					print p,start,c
					index2 = i
					p = polygon[index2]
			if index2 is not None:
				done = index == index2
				index = index2
			else:
				done = True
				index2 = index

		print 'index2: ' + str(index2)
		if index2 != index:
			print 'index:' + str(index) + ',' + str(index2)
			index=index2
		p = polygon[index]
		for i in range(len(polygon)):
			c = polygon[i]
			if triangle_direction([p,start,c]) > 0:
				print p,start,c
		return index

		low = 0
		high = len(polygon)

		zero_dot = objective(polygon[0])
		zerom1_dot = objective(polygon[-1])
		zerop1_dot = objective(polygon[1])
		if zerom1_dot < zero_dot and zero_dot > zerop1_dot:
			low = 0
			high = low

		zero_dot = objective(polygon[-1])
		zerom1_dot = objective(polygon[-2])
		zerop1_dot = objective(polygon[0])
		if zerom1_dot < zero_dot and zero_dot > zerop1_dot:
			low = len(polygon)-1
			high = low

		hit_valley = False
		while low+1<high:
			mid = low + (high-low)/2
			midm1_dot = objective(polygon[(mid-1)%len(polygon)])
			mid_dot = objective(polygon[mid%len(polygon)])
			midp1_dot = objective(polygon[(mid+1)%len(polygon)])
			#comparison = cmp(sign(mid_dot[0])*mid_dot[0]**2*midp1_dot[1],sign(midp1_dot[0])*midp1_dot[0]**2*mid_dot[1])
			#comparison = cmp(mid_dot-midp1_dot,0)
			if cmp(midm1_dot,mid_dot) < 0 and cmp(mid_dot,midp1_dot)<0:
				low = mid+1
			elif cmp(midm1_dot,mid_dot) > 0 and cmp(mid_dot,midp1_dot)>0:
				high = mid
			elif cmp(midm1_dot,mid_dot) < 0 and cmp(mid_dot,midp1_dot)>0:
				low = mid
				high = mid
				break
			elif cmp(midm1_dot,mid_dot) > 0 and cmp(mid_dot,midp1_dot) < 0:
				# If the objective curve increases at polygon[0], then the maximum is in (0,mid)
				zero_dot = objective(polygon[0])
				zerom1_dot = objective(polygon[-1])
				zerop1_dot = objective(polygon[1])
				if zerom1_dot < zero_dot and zero_dot < zerop1_dot:
				#if objective(polygon[0])<objective(polygon[-1]):
					low = 0
					high = mid
				elif zerom1_dot > zero_dot and zero_dot > zerop1_dot:
					low = mid+1
					high = len(polygon)
		 		elif zerom1_dot < zero_dot and zero_dot > zerop1_dot:
					low = mid
					high = mid
				else:
					low = 1
					high = len(polygon)
				hit_valley = True
				#print "valley"
				#print low,high
			else:
				low = mid
				high = mid
				#print "unknown"
				#print low,high

		low %= len(polygon)
		#print '\nmax,objectives:'
		#print str(objective(polygon[low]))
		#print float(polygon[low].elements[0]),float(polygon[low].elements[1])

		if index != low:
			p = polygon[-1]
			print index,low,high,len(polygon)
			for c in polygon:
				if objective(polygon[low])<objective(c):
				  print '\t' + str(objective(c)) + ',' + str(objective(c)-objective(p)) + ',' + str(objective(polygon[index]))
				  p = c
				  #print '\t' + str(objective(p))
			print hit_valley
			print '\n'
			low = index


		return low

	def insert_point(polygon, point):
		bounds = fan_binsearch(polygon,point)
		if bounds is None:
			return
		(left,right) = bounds
		left %= len(polygon)
		right %= len(polygon)

		#print triangle_direction([polygon[left],polygon[right],point]),							\
		#	triangle_direction([polygon[right],polygon[(right+1)%len(polygon)],point]),	\
		#	triangle_direction([polygon[(left-1)%len(polygon)],polygon[left],point])

		#def objective2(p,end,start):
		#	# Point toward the right
		#	normal = Vector((end-start).elements[1::-1])
		#	normal.elements[1] = -normal.elements[1]
		#	return cmp(normal.dot(p-start),0)

		
		if graphics:
			plt.clf()

		#left_extreme = -fan_bitonic_binsearch(polygon[::-1],point) % len(polygon)
		#right_extreme = fan_bitonic_binsearch(polygon,point)
			plt.title('left')
			plt.plot([float(point.elements[0]),float(polygon[left].elements[0])],[float(point.elements[1]),float(polygon[left].elements[1])],'m')
			plt.plot([float(point.elements[0]),float(polygon[right].elements[0])],[float(point.elements[1]),float(polygon[right].elements[1])],'c')
		left_extreme = fan_bitonic_binsearch(polygon[left::-1]+polygon[:left:-1],point,-1)
		left_extreme = (left - left_extreme) % len(polygon)
		if graphics:
			plt.plot([float(point.elements[0]),float(polygon[left_extreme].elements[0])],[float(point.elements[1]),float(polygon[left_extreme].elements[1])],'r-.')
			p = polygon[-1]
			for c in polygon:
				plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
				p = c
			plt.show()

		if graphics:
			plt.title('right')
			plt.plot([float(point.elements[0]),float(polygon[left].elements[0])],[float(point.elements[1]),float(polygon[left].elements[1])],'m')
			plt.plot([float(point.elements[0]),float(polygon[right].elements[0])],[float(point.elements[1]),float(polygon[right].elements[1])],'c')
		right_extreme = fan_bitonic_binsearch(polygon[right:]+polygon[:right],point,1)
		right_extreme = (right_extreme + right) % len(polygon)
		if graphics:
			plt.plot([float(point.elements[0]),float(polygon[right_extreme].elements[0])],[float(point.elements[1]),float(polygon[right_extreme].elements[1])],'g-.')
			p = polygon[-1]
			for c in polygon:
				plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
				p = c
			plt.show()

		
		#plt.plot([float(point.elements[0]),float(polygon[left_extreme].elements[0])],[float(point.elements[1]),float(polygon[left_extreme].elements[1])],'b-.')
		#plt.plot([float(point.elements[0]),float(polygon[right_extreme].elements[0])],[float(point.elements[1]),float(polygon[right_extreme].elements[1])],'b-.')
		#print (left,right)
		#print left_extreme,right_extreme
		#f = []
		#for p in polygon:
		#	#f.append(triangle_direction([p,polygon[left_extreme],point]))
		#	f.append(objective2(p,polygon[right_extreme],point))
		#print f

		#plt.plot(float(point.elements[0]),float(point.elements[1]),'g*')


		le = polygon[left_extreme]
		re = polygon[right_extreme]
		if left_extreme<=right_extreme:
			polygon.insert(right_extreme, point)
			del polygon[left_extreme+1:right_extreme]
		else:
			del polygon[left_extreme+1:]
			del polygon[:right_extreme]
			polygon.insert(0, point)

		new_hull = [le,point,re]
		filter_pending(new_hull,pending)

	pending = list(points)
	hull = []
	hull.append(pending.pop())
	hull.append(pending.pop())

	def filter_pending(new_hull,pending):
		#pending_count = len(pending)
		for pending_point in pending:
			if inside_hull(new_hull,pending_point):
				pending.remove(pending_point)
		#print pending_count-len(pending)


	def inside_hull(hull,point):
		previous = hull[-1]
		for current in hull:
			if triangle_direction([previous,current,point]) > 0:
				return False
			previous = current
	 	return True

	v = pending.pop()
	if triangle_direction(hull+[v]) < 0:
		hull.append(v)
	else:
		hull.insert(1,v)

	filter_pending(hull, pending)
	

	#p = hull[-1]
	#for c in hull:
		#plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b-.')
		#p = c
	while len(pending) >0:
		point = pending.pop()
		insert_point(hull,point)

	failed = []
	for x in points:
		p = hull[-1]
		for c in hull:
			if triangle_direction([p,c,x])>0:
				failed.append(x)
			p = c

	if len(failed)>0:
		print "Failed"
		plt.clf()
		for c in failed:
			plt.plot(float(c.elements[0]),float(c.elements[1]),'b*')
		for c in hull:
			plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
			p = c
		for c in points:
			plt.plot(float(c.elements[0]),float(c.elements[1]),'r.')
		plt.show()
	return hull

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
			plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b-.')
			p = positive_max
			c = end
			plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b--')
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
	plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b:')
	upper = sorted(recurse(working[-1],working[0],working),key=key_x)
	upper += [working[-1]]
	sort_vertical(upper,1)

	lower = sorted(recurse(working[0],working[-1],working),key=lambda v: -key_x(v))
	lower += [working[0]]
	sort_vertical(lower,-1)

	#lower = []
	result = upper+lower
	return result

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

