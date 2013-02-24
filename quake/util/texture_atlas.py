
class TextureAtlas:
	def __init__(self,rect):
		self.children = [None,None]
		self.rect = rect
		self.image = None

	def insert(self,image):
		if self.children[0] is not None and self.children[1] is not None:
			if self.children[0].insert(image):
				return True
			elif self.children[1].insert(image):
				return True
			else:
		 		return False
		if self.image is not None:
			return False
 		elif image['width']>self.rect['width'] or image['height']>self.rect['height']:
			return False
 		elif image['width']==self.rect['width'] and image['height']==self.rect['height']:
			self.image = image
			return True

		remaining_width = self.rect['width'] - image['width']
		remaining_height = self.rect['height'] - image['height']
		if remaining_width > remaining_height:
 			self.children[0] =	\
				TextureAtlas({'left':self.rect['left'],'top': self.rect['top'],'width':image['width'],'height':self.rect['height']})
 			self.children[1] =	\
				TextureAtlas({'left':self.rect['left']+image['width'],'top': self.rect['top'],'width':remaining_width,'height':self.rect['height']})
		else:
			self.children[0] =	\
				TextureAtlas({'left':self.rect['left'],'top': self.rect['top'],'width':self.rect['width'],'height':image['height']})
 			self.children[1] =	\
				TextureAtlas({'left':self.rect['left'],'top': self.rect['top']+image['height'],'width':self.rect['width'],'height':remaining_height})
		return self.children[0].insert(image)

	def insert_many(self,images):
		sorted_images = sorted(images,key=lambda x: -x['width']*x['height'])
		for image in sorted_images:
			#print image['width'],image['height']
			self.insert(image)
	def __str__(self):
		x = ''
		if self.image is not None:
			#x += '({},{}),({},{})\n'.format(self.image['width'],self.image['height'])
			x += str(self.rect) + '\n'
		#x += '['
		for child in self.children:
			if child is not None:
				x += str(child) 
		#x += ']'
		return x
	def render(self,data,width,height):
		#if self.image is not None:
		#	for i in range(self.image['width']):
		#		for j in range(self.image['height']):
		#			x = self.rect['left'] + i
		#			y = self.rect['top'] + j
		#			data[y*width+x] = self.image['data'][j*self.image['width']+i]
		#for child in self.children:
		#	if child is not None:
		#		child.render(data,width,height)
		def handler(rect,resource,_):
			for i in range(resource['width']):
				for j in range(resource['height']):
					x = rect['left'] + i
					y = rect['top'] + j
					data[y*width+x] = resource['data'][j*resource['width']+i]
		self.traverse(handler)

	def traverse(self,handler,accumulator=None):
		if self.image is not None:
			accumulator = handler(self.rect,self.image,accumulator)
		for child in self.children:
			if child is not None:
				accumulator = child.traverse(handler,accumulator)
		return accumulator
