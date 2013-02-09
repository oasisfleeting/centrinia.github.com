
from clnum import *
import random
#import numpy as np
import matplotlib.pyplot as plt
import math

def sign(x):
	return cmp(x,0)

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
			sn = float(vec.elements[0]*sample_vector.elements[1]-vec.elements[1]*sample_vector.elements[0])*side
			nm = sqrt(float(vec.dot(vec))*float(sample_vector.dot(sample_vector)))
			sn /= nm
			cs /= nm

			rad = math.atan2(sn,cs)
			if rad>=math.pi:
				rad -= math.pi*2
			return rad

			# Point toward the left
			# This normal must be normalized.
			#normal = Vector((end-start).elements[1::-1])
			#normal.elements[0] = -normal.elements[0]
			##return (normal.dot(sample_vector),normal.dot(normal))
			#return float(normal.dot(sample_vector))/sqrt(float(normal.dot(normal)))

		#index = 0
		#print objective(polygon[index])
		#for i in range(0,len(polygon)):
		#	mid_dot = objective(polygon[i])
		#	midp1_dot = objective(polygon[index])
		#	comparison = cmp(mid_dot,midp1_dot)
		#	if comparison > 0:
		#		index = i
		#return index


		low = 0
		high = len(polygon)
		while low+1<high:
			mid = low + (high-low)/2
			midm1_dot = objective(polygon[(mid-1)%len(polygon)])
			mid_dot = objective(polygon[mid])
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
				if objective(polygon[0])<objective(polygon[1]):
					low = 0
					high = mid
				else:
					low = mid+1
					high = len(polygon)
				print "valley"
				print low,high
			else:
				low = mid
				high = mid
				print "unknown"
				print low,high

		low %= len(polygon)
		#print '\nmax,objectives:'
		#print str(objective(polygon[low]))
		#print float(polygon[low].elements[0]),float(polygon[low].elements[1])

		p = polygon[-1]
		for c in polygon:
			if objective(polygon[low])<objective(c):
			  print '\t' + str(objective(c)) + ',' + str(objective(c)-objective(p))
			  p = c
			  #print '\t' + str(objective(p))

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

		
		plt.plot([float(point.elements[0]),float(polygon[left_extreme].elements[0])],[float(point.elements[1]),float(polygon[left_extreme].elements[1])],'b-.')
		plt.plot([float(point.elements[0]),float(polygon[right_extreme].elements[0])],[float(point.elements[1]),float(polygon[right_extreme].elements[1])],'b-.')
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

		l = len(pending)
		news = [le,point,re]
		for t in pending:
			reject = True
			p = news[-1]
			for c in news:
				if triangle_direction([p,c,t]) > 0:
					reject = False
				p = c
			if reject:
		 		pending.remove(t)
		print l-len(pending)
	pending = list(points)
	result = []
	result.append(pending.pop())
	result.append(pending.pop())

	v = pending.pop()
	if triangle_direction(result+[v]) < 0:
		result.append(v)
	else:
		result.insert(1,v)


	p = result[-1]
	for c in result:
		#plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b-.')
		plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
		p = c
	while len(pending) >0:
		point = pending.pop()
		insert_point(result,point)
		
	for x in points:
		p = result[-1]
		for c in result:
			if triangle_direction([p,c,x])>0:
				print "fail:"
				print p,c,x
			p = c


	return result

def rational(x):
	return mpq(x,1)

def test_convex_hull_2d(count):
	points = []
	for i in range(count):
		elements = []
		for j in range(2):
			den = random.randint(1,1000000)
			#num = random.randint(0,den-1)
			num = int(random.gauss(1,0.5)*den)

			elements.append(mpq(num,den))
		p = Vector(elements)
		points.append(p)
	hull = convex_hull_2d(points)
	p = hull[-1]
	for c in hull:
		plt.plot([float(p.elements[0]),float(c.elements[0])],[float(p.elements[1]),float(c.elements[1])],'b')
		p = c
	for c in points:
		plt.plot(float(c.elements[0]),float(c.elements[1]),'ro')

	plt.show()
graphics = False
test_convex_hull_2d(2000)

