"use strict";

var texture_images;
var log_draw_count;
var log_traverse_count;
var canvas_element;
var canvas_element2;
var canvas_context;
var webgl_context;

var level;
var texture_indexes;

var use_individual_vertices = true;
var maximum_draw_count = null;
var use_wireframe = false;
var use_texturing = true;
var use_lighting = true;
//var use_canvas = false;
//var use_webgl = !use_canvas;
var use_canvas = true;
var use_webgl = true;

var use_noclip = true;
var use_blending = false;

var config = {
	'key bindings' : [
		{
			'binding': 'turn left',
			'key' : 'left cursor key',
			'modifiers' : []
		},
		{
			'binding': 'move forward',
			'key' : 'up cursor key',
			'modifiers' : []
		},
		{
			'binding': 'turn right',
			'key' : 'right cursor key',
			'modifiers' : []
		},
		{
			'binding': 'move backward',
			'key' : 'down cursor key', 
			'modifiers' : []
		},
		{
			'binding': 'move left',
			'key' : 'left cursor key',
			'modifiers' : ['alt key']
		},
		{
			'binding': 'move up',
			'key' : 'up cursor key',
			'modifiers' : ['alt key']
		},
		{
			'binding': 'move right',
			'key' : 'right cursor key',
			'modifiers' : ['alt key']
		},
		{
			'binding': 'move down',
			'key' : 'down cursor key',
			'modifiers' : ['alt key']
		}
	],
	'keycodes' : {
		'modifiers' : {
			18 : 'alt key'
		},
		'keys' : {
			37 : 'left cursor key',
			38 : 'up cursor key',
			39 : 'right cursor key',
			40 : 'down cursor key'
		}
	},
	'key refresh' : 35,
	'analog stick radius': 100,
	'analog stick scale': 2,
	'physics' : {
		//'gravity': 6.0e-2
		'gravity': 0
	},
	'movement' : {
		'strafe distance' : 5,
		'forward distance' : 5,
		'backward distance' : 3,
		'vertical distance' : 5,
		'turn angle' : 5 // In degrees.
	},
	'viewbob' : {
		'amplitude' : 0.0,
		'frequency' : 0.07
	}
};

function clamp(value, low,high) {
	if(value < low) {
		return low;
	} else if(high < value) {
		return high;
	} else {
		return value;
	}
}

//{{{
var clone = function() {
	var result = this instanceof Array ? [] : {};
	for (var member in this) {
		if(this[member] && typeof this[member] == 'object') {
			result[member] = this[member].clone();
		} else {
			result[member] = this[member];
		}
	}
	return result;
}

Object.defineProperty(Object.prototype, 'clone', {value: clone, enumerable: false});

function object_map(obj, handler) {
	var result = {};
	for(var member in Object.keys(obj)) {
		var t = handler(member,obj[member]);
		if(t) {
			result[member] = t;
		}
	}
	return result;
}

function to_array(obj) {
	var result = [];
	for(var member in Object.keys(obj)) {
		result[member] = obj[member];
	}
	return result;
}

Math.mod = function(x,y) {
	return x - y*Math.floor(x/y);
}

Array.prototype.equals = function (b) {
	if(this.length != b.length) {
		return false;
	}

	for(var i=0;i<this.length;i++) {
		if(this[i] != b[i]) {
			return false;
		}
	}
	return true;
}

Array.prototype.shuffle = function () {
	for (var i = this.length - 1; i > 0; i--) {
		var j = Math.floor(Math.random() * (i + 1));
		var tmp = this[i];
		this[i] = this[j];
		this[j] = tmp;
	}

	return this;
}//}}}

/*
// A two dimensional vector.//{{{
function Vector2(coord)
{
	this.coord = coord;
	this.add = function(vec)
	{
		return new Vector2([this.coord[0]+vec.coord[0],this.coord[1]+vec.coord[1]]);
	};
	this.subtract = function(vec)
	{
		return new Vector2([this.coord[0]-vec.coord[0],this.coord[1]-vec.coord[1]]);
	};
	this.dot = function(vec)
	{
		return this.coord[0]*vec.coord[0]+this.coord[1]*vec.coord[1];
	};
	this.norm = function() {
		var x0 = this.coord[0];
		var x1 = this.coord[1];
		return Math.sqrt(x0 * x0 + x1 * x1);
	}
	this.distance = function(vec) {
		return this.subtract(vec).norm();
	}
	this.normalized = function() {
		var x0 = this.coord[0];
		var x1 = this.coord[1];
		var norm = this.norm();
		return new Vector2([x0/norm,x1/norm]);
	};
	this.angle_to = function (vec) {
		var d = vec.normalized().dot(this.normalized());
// |d| may be greater than unity because of rounding errors.
		d = Math.abs(d) < 1 ? d : (d < 0 ? -1 : 1);
		return Math.acos(d);
	};
	this.rotate = function (angle) {
		var cs = Math.cos(angle);
		var sn = Math.sin(angle);
		var x0 = cs * this.coord[0] - sn*this.coord[1];
		var x1 = sn * this.coord[0] + cs*this.coord[1];
		return new Vector2([x0,x1]);
	}
	this.scale = function (s) {
		return new Vector2([this.coord[0]*s,this.coord[1]*s]);
	}
}//}}}
*/
// A viewing frustum.//{{{
function Viewer(fov,direction,viewpoint)
{
// Update the frustum.//{{{
	this.update = function(fov,viewpoint,direction,pitch_angle,intersection_handler) {
		if(!intersection_handler(this.viewpoint,viewpoint)) {
		  return;
		}
		this.viewpoint = viewpoint;

		if(typeof direction == 'number') {
			this.direction_vector = (Vector.create([1,0,0])).rotate2d(direction);
			this.direction_angle = direction;
		} else {
			this.direction_vector = direction;

			this.direction_angle = this.direction_vector.angle_to(Vector.create([1,0,0]));
			if(this.direction_vector.coord[1] < 0) {
				this.direction_angle = 2*Math.PI - this.direction_angle;
			}
		}
		this.pitch_angle = pitch_angle
	};//}}}
// Move the viewpoint forward.//{{{
	this.move_forward = function (step, intersection_handler)
	{
		this.stepDistance++;
		var viewbob = Math.sin(this.stepDistance*2*Math.PI*config['viewbob']['frequency'])*config['viewbob']['amplitude'];

		var pitch = -Math.sin(this.pitch_angle);
		var cs = Math.cos(this.pitch_angle);

		var vector = this.direction_vector.scale(step*cs);
		var new_viewpoint = this.viewpoint.add(vector).add(Vector.create([0,0,viewbob+pitch*step]));
		this.update(this.fov,new_viewpoint,this.direction_vector,this.pitch_angle, intersection_handler);
	};//}}}
// Move the viewpoint to the left.//{{{
	this.move_left = function (step, intersection_handler)
	{
		this.stepDistance++;
		var viewbob = Math.sin(this.stepDistance*2*Math.PI*config['viewbob']['frequency'])*config['viewbob']['amplitude'];


		var vector = this.direction_vector.scale(step);
		var t = vector.coord[0];
		vector.coord[0] = vector.coord[1];
		vector.coord[1] = t;

		vector.coord[0] *= -1;

		var new_viewpoint = this.viewpoint.add(vector).add(Vector.create([0,0,viewbob]));
		this.update(this.fov,new_viewpoint,this.direction_vector,this.pitch_angle,intersection_handler);
	};//}}}
// Move the viewpoint up.//{{{
	this.move_up = function (step,intersection_handler)
	{
		var vector = Vector.create([0,0,step]);
		var new_viewpoint = this.viewpoint.add(vector);
		this.update(this.fov,new_viewpoint,this.direction_vector,this.pitch_angle,intersection_handler);
	};//}}}
// Turn the frustum to the left.//{{{
	this.turn_left = function (step)
	{
		this.update(this.fov,this.viewpoint,this.direction_vector.rotate2d(step),this.pitch_angle, function (o,v) { return true; });
	};//}}}
// Directly set the viewpoint.//{{{
	this.set_viewpoint = function(viewpoint) {
		this.update(this.fov,viewpoint,this.direction_vector,this.elevation, function(o,v) { return true; });
	};//}}}
	this.update_elevations = function(elevations) {
	  for(var i=0;i<2;i++) {
			this.screen_elevations[0][i] = this.screen_elevations[0][i] > elevations[0][i] ? this.screen_elevations[0][i] : elevations[0][i];
			this.screen_elevations[1][i] = this.screen_elevations[1][i] < elevations[1][i] ? this.screen_elevations[1][i] : elevations[1][i];
	  }
	}
	this.fov = fov;
	this.super_fov = fov;
	this.stepDistance = 0;
	this.viewpoint = viewpoint;
	this.update(fov,viewpoint,direction,0,function (o,v) { return true;});
}//}}}

function Wall(endpoint0,endpoint1,elevations,color,textures)
{
	this.elevations = elevations.sort(function (a,b) { return (a<b ? 1 : 0) - (a > b ? -1 : 0); } );
	this.line = new Segment(endpoint0,endpoint1);
	this.color = color;
	this.textures = textures;
}

function rgb(color) {
	return 'rgb(' + ~~color[0] + ',' + ~~color[1] + ',' + ~~color[2] + ')';
}

function Plane(definition) {
	definition.object = this;
	this.normal = Vector.create(definition['normal']);
	this.displacement = definition['dist'];

	var normal_length = this.normal.norm();
	this.normal = this.normal.scale(1/normal_length);
	this.displacement /= normal_length;
}
Plane.prototype.normal_equation = function (point) {
	return this.normal.dot(point)-this.displacement;
};

Plane.prototype.intersect_segment = function (endpoints) {
	//var n = this.normal.coord.concat([-this.displacement]);
	var n = this.normal.copy().set_at(3, -this.displacement);
	var direction = endpoints[1].subtract(endpoints[0]);
	var denominator = direction.dot(n);
	if(denominator != 0) {
		var t = -n.dot(endpoints[0]) / denominator;
		var vector = endpoints[0].mix(endpoints[1], t);
		return {
			't' : t,
			'vector' : vector
		};
	} else {
		return null;
	}
};
Plane.prototype.closest_to_point = function (point) {
	var t = this.normal.dot(point) - this.displacement;

	return point.subtract(this.normal.scale(t));
};

function Face(definition,level,buffers) {
	var indexes = definition['vertices index'];

	var head = indexes[0];
	var tail1 = indexes.slice(1,indexes.length-1);
	var tail2 = indexes.slice(2,indexes.length);

	this.vertices = indexes.map(function (index) {
		return Vector.create(level['vertices'][index]);
	});

	this.plane = level['planes'][definition['plane id']];

	if(!this.plane.object) {
		this.plane.object = new Plane(this.plane);
	}
	/*var face_normal;
	var j=0;
	do {
		//vec3.subtract(level['vertices'][indexes[j]],level['vertices'][indexes[j+1]],v0);
		var v0 = this.vertices[j].subtract(this.vertices[j+1]);
		var i=2+j;
		do {
			var v1 = this.vertices[i].subtract(this.vertices[j+1]);
			//vec3.subtract(level['vertices'][indexes[i]],level['vertices'][indexes[j+1]],v1);
			face_normal = v0.cross(v1).normalize();
			i++;
		} while(i < indexes.length && (face_normal.norm() < 1-10));
		j++;
	} while(j+2 < indexes.length && face_normal.norm() <1e-10);
	this.normal = face_normal;*/

	this.normal = this.plane.object.normal;
	if(!definition['front side']) {
		this.normal = this.normal.scale(-1);
	}

	/*if(face_normal.dot(this.normal) < 0) {
		console.log(face_normal);
		console.log(this.normal);
	}*/
		
	/*if(Math.abs(this.normal.dot(this.plane.object.normal)) < (1-1e-5)) {
	console.log(this.normal.dot(this.plane.object.normal));
	console.log(this.normal.coord);
	console.log(this.plane.object.normal.coord);
	console.log(this);
	}*/

	//this.normal = this.plane.object.normal;

	var face_color = Vector.create(
		this.normal.normalize().coord.map(function (c) { return (c+1)/2; })
	);
	/*var face_color = Vector.create(
		[Math.random(),Math.random(),Math.random()]
	);*/
	face_color.coord.push(1);

	/* TODO: Use Vector
	this.edge_planes = [];
	for(var i=0;i<this.vertices.length;i++) {
		var v = new Array(3);
		var tangent_normal = new Array(3);
		vec3.subtract(this.vertices[(this.vertices.length+i-1) % this.vertices.length], this.vertices[i], v);
		vec3.cross(v, this.plane.object.normal,tangent_normal);
		vec3.normalize(tangent_normal);
		var edge_plane = new Plane({ 'normal' : tangent_normal, 'dist' : vec3.dot(tangent_normal, this.vertices[i])})
		edge_plane.vertex = this.vertices[i];
		this.edge_planes.push(edge_plane);
	}*/

	var levelname = level['filename'];
	var texture_index = definition['texture index'];
	var texinfo = level['textures'][texture_index];
	if(texture_indexes[levelname]) {
		var atlas_entry = texture_indexes[levelname][texinfo['miptex index']];
	} else {
		var atlas_entry = texture_indexes['start.bsp'][texinfo['miptex index']];
	}
	function compute_texture_coord(vertex,vector,dist) {
		return vertex.dot(vector)+dist;
	}

	function get_index(index) {
		if(use_individual_vertices) {
			var result = buffers['vertex'].length;
			var vertex = Vector.create(level['vertices'][index]);
			buffers['vertex'].push(vertex.coord);
			buffers['color'].push(face_color.coord);
			buffers['normal'].push(this.normal.coord);

			// [x,y,w,h]
			var texrange = atlas_entry['begin'].concat(atlas_entry['size']);
			texrange[0] /= texrange[2];
			texrange[1] /= texrange[3];

			if(atlas_entry['texture name'].substr(0,3) == 'sky') {
				texrange[2] = -texrange[2];
				texrange[3] = -texrange[3];
			}
			buffers['texture range'].push(texrange);

			//s = dotproduct(Vertex,vectorS) + distS;    
			//t = dotproduct(Vertex,vectorT) + distT;
			var texcoord = new Array(2); // [s,t]
			for(var i=0;i<2;i++) {
				var vector = Vector.create(texinfo['vectors'][i]);
				var dist = texinfo['displacements'][i];
				texcoord[i] = compute_texture_coord(vertex,vector,dist) / atlas_entry['size'][i];
			}
			buffers['texture coordinate'].push(texcoord.map(function (x) { return isFinite(x) ? x : 0; }).concat([0,0]));
			return result;
		} else {
			return index;
		}
	}
	this.triangles = new Array(indexes.length-2);
	this.edges = new Array(indexes.length);
	var head_index = get_index.call(this,head);
	for(var i=0;i<tail1.length;i++) {
		var tail1_index = get_index.call(this,tail1[i]);
		var tail2_index = get_index.call(this,tail2[i]);
		this.triangles[i] = [head_index, tail1_index, tail2_index];
		this.edges[i+1] = [tail1_index, tail2_index];
	}
	this.edges[0] = [head_index,this.triangles[0][0]];
	this.edges[indexes.length-1] = [this.triangles[this.triangles.length-1][2],head_index];

}
Face.prototype.inside_face = function (point) {
	var triangle_clockwise = function (a,b,c) {
		var normal = c.subtract(a).cross(b.subtract(a));
		return this.normal.dot(normal) > 0;
	}
	var low = 0;
	var high = this.vertices.length;

	do {
		var mid = (low+high) >> 1;
		if(triangle_clockwise.apply(this, [this.vertices[0], this.vertices[mid], point])) {
			low = mid;
		} else {
			high = mid;
		}
	} while(low+1 < high);
	if(low == 0 || high == this.vertices.length) {
		return false;
	}
	return triangle_clockwise.apply(this, [this.vertices[low],this.vertices[high], point]);
}
Face.prototype.intersects_segment = function (start,end) {
	var endpoints = [start.copy().set_at(3,1),end.copy().set_at(3,1)];
	var intersection = this.plane.object.intersect_segment(endpoints);
	if(!intersection) {
		return false;
	}
	if(intersection['t'] < 0 || intersection['t'] > 1) {
		return false;
	}
	return this.inside_face(intersection['vector']);
}
Face.prototype.intersects_sphere = function (center,radius) {
	if(Math.abs(this.plane.object.normal_equation(center)) > radius) {
		return false;
	}
	var segment_intersects_sphere = function (start,end, center,radius) {
		var normal = end.subtract(start).normalize();
		var m = start.subtract(center);
		var c = m.dot(m) - radius*radius;
		if(c<=0) {
			return true;
		}
		var b = normal.dot(m);
		if(b > 0) {
			return false;
		}
		var discriminant = b*b-c;
		if(discriminant < 0) {
			return false;
		}
		var t = -b-Math.sqrt(discriminant);
		if(t<0) {
			t = 0;
		}
		return t < end.distance(start);
	}
	for(var i=0,j=this.vertices.length-1;i<this.vertices.length;j=i,i++) {
		var start = this.vertices[j];
		var end = this.vertices[i];

		if(segment_intersects_sphere(start, end, center,radius)) {
			return true;
		}
	}
	var closest_point = this.plane.object.closest_to_point(center);
	return this.inside_face(closest_point);
};

function Leaf(leaf_definition,level,parent_node) {
	this.type = leaf_definition['type'];
	this.depth = parent_node.depth+1;
	this.parent_node = parent_node;
	this.visilist_start = leaf_definition['visilist start'];
	this.face_indexes = leaf_definition['face indexes'];
	this.faces = [];
	leaf_definition.object = this;
}

function Node(node_definition,level,parent_node) {
	if(parent_node) {
		this.parent_node = parent_node;
		this.depth = parent_node.depth+1;
	} else {
		this.parent_node = null;
		this.depth = 0;
	}
	node_definition.object = this;
	this.plane = new Plane(level['planes'][node_definition['plane id']]);

	var this_node = this;
	this.child_nodes = node_definition['children nodes'].map(function (index) {
		var definition = level['nodes'][index];
		if(definition != null) {
			return new Node(definition,level,this_node);
		} else {
			return null;
		}
	});
	this.child_leaves = node_definition['children leaves'].map(function (index) {
		var definition = level['leaves'][index];
		if(definition) {
			var leaf = new Leaf(definition,level,this_node);
			leaf.index = index;
			return leaf;
		} else {
			return null;
		}
	});
}
// Visit every leaf and node in infix order and call the handler on them.
Node.prototype.dfs = function (point,leaf_handler,node_handler,back_to_front) {
	var d = this.plane.normal_equation(point);
	var side = d > 0 ? 0 : 1;
	if(back_to_front) {
		side = 1-side;
	}
	if(node_handler) {
		node_handler(this);
	}
	for(var i=0;i<2;i++) {
		var index = side^i;
		var node = this.child_nodes[index];

		if(node == null) {
			var leaf = this.child_leaves[index];
			if(leaf_handler) {
				leaf_handler(leaf);
			}
		} else {
			node.dfs(point,leaf_handler,node_handler,back_to_front);
		}
	}
};
// Find the leaf that contains this point.
Node.prototype.find_leaf = function (point) {
	var node = this;
	var leaf = null;

	do {
		var d = node.plane.normal_equation(point);
		var side = d > 0 ? 0 : 1;
		leaf = node.child_leaves[side];
		node = node.child_nodes[side];
	} while(node != null);
	return leaf;
}
Node.prototype.leaves = function () {
	var accumulator = [];
	this.child_nodes.forEach(function (node) {
		if(node != null) {
			$.extend(accumulator,node.leaves());
		}
	});
	this.child_leaves.forEach(function (leaf) {
		if(leaf != null) {
			accumulator[leaf.index] = leaf;
		}
	});
	return accumulator;
};

function clip_polygon_cascade(vertices, planes) {
	var pipeline = planes.map(function (plane) {
		return {
			'plane' : plane,
			'last' : null,
			'previous' : null
		};
	});
	pipeline.push({
		'plane' : null,
		'next' : null,
		'vertices' : []
	});

	for(var i=1;i<pipeline.length;i++) {
		pipeline[i-1]['next'] = pipeline[i];
	}

	function dot(plane,v) {
		return plane.normal.dot(v) - plane.displacement*v.coord[3];
	}
	var advance_pipeline = function(worker,vertex) {
		if(worker['next']) {
			if(!worker['previous']) {
				worker['previous'] = vertex;
				worker['first'] = vertex;
			} else {
				var working = [worker['previous'],vertex];

				// working_vertices = [start,end]
				var inside = working.map(function (vertex) {
					//return worker['plane'].normal_equation(vertex) >= 0;
					return dot(worker['plane'],vertex) >= 0;
				});

				if(inside[0] ^ inside[1]) {
					var intersection = worker['plane'].intersect_segment(working);
					if(!intersection) {
						//console.log(working);
						intersection = {'vector':working[1]};
					}
					advance_pipeline(worker['next'],intersection['vector']);
				}
				if(inside[1]) {
					advance_pipeline(worker['next'],working[1]);
				}
				worker['previous'] = working[1];
			}
		} else {
			worker['vertices'].push(vertex);
		}
	};
	var flush_pipeline = function (worker) {
		if(worker['next']) {
			advance_pipeline(worker,worker['first']);
			flush_pipeline(worker['next']);
		}
	}
	
	vertices.forEach(function (vertex) {
		advance_pipeline(pipeline[0],vertex);
	});
	flush_pipeline(pipeline[0]);
	return pipeline[pipeline.length-1]['vertices'];
}

// Globals.//{{{
$(document).ready(function() {
	var canvas_name = 'canvas';
	//canvas_element = document.getElementById(canvas_name);
	//canvas_element2 = document.getElementById('canvas2');
	canvas_element = $('#canvas');
	canvas_element2 = $('#canvas2');
	if(canvas_element2.length==0) {
		canvas_element2 = canvas_element;
	}
	canvas_element = canvas_element[0];
	canvas_element2 = canvas_element2[0];


	var texture_atlas;
	if(use_webgl) {
// Initialize WebGL.//{{{
		try {
			webgl_context = canvas_element2.getContext('experimental-webgl');
			webgl_context.width = canvas_element2.width;
			webgl_context.height = canvas_element2.height;
		} catch (e) {}
		if(!webgl_context) {
			use_webgl = false;
			use_canvas = true;
		} else {
			var get_shader = function (context,type,filename) {
				var shader;
				var str;
				$.ajax({async: false,
					type: 'GET',
					url: 'shaders/' + filename,
					data: null,
					success: function(d) {
						str = d;
					}
					,dataType: 'text'
				});
				if (type == 'x-shader/x-fragment') {
					shader = context.createShader(context.FRAGMENT_SHADER);
				} else if (type == 'x-shader/x-vertex') {
					shader = context.createShader(context.VERTEX_SHADER);
				} else {
					return null;
				}

				context.shaderSource(shader, str);
				context.compileShader(shader);

				if (!context.getShaderParameter(shader, context.COMPILE_STATUS)) {
					alert(context.getShaderInfoLog(shader));
					return null;
				}

				return shader;
			}
			var initialize_shaders = function (context) {
				var vertex_shader;
				var vertex_shader = get_shader(context, 'x-shader/x-vertex', 'vertex.glsl');
				var fragment_shader = get_shader(context, 'x-shader/x-fragment', 'fragment.glsl');

				var shader_program = webgl_context.createProgram();
				context.attachShader(shader_program, vertex_shader);
				context.attachShader(shader_program, fragment_shader);
				context.linkProgram(shader_program);

				if(!context.getProgramParameter(shader_program,context.LINK_STATUS)) {
					alert('Could not link shaders' + context.getProgramInfoLog(shader_program));
					return null;
				}
			   context.useProgram(shader_program);

				return shader_program;
			}

			webgl_context.viewportWidth = canvas_element.width;
			webgl_context.viewportHeight = canvas_element.height;

			var webgl_shader_program = initialize_shaders(webgl_context);

			webgl_shader_program.vertexPositionAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexPosition");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexPositionAttribute);

			//webgl_shader_program.vertexColorAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexColor");
			//webgl_context.enableVertexAttribArray(webgl_shader_program.vertexColorAttribute);

			webgl_shader_program.vertexNormalAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexNormal");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexNormalAttribute);

			webgl_shader_program.vertexTextureCoordinateAttribute =
				webgl_context.getAttribLocation(webgl_shader_program, "aVertexTextureCoord");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexTextureCoordinateAttribute);

			webgl_shader_program.vertexTextureRangeAttribute =
				webgl_context.getAttribLocation(webgl_shader_program, "aVertexTextureRange");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexTextureRangeAttribute);

			webgl_shader_program.use_lighting_uniform = webgl_context.getUniformLocation(webgl_shader_program, "use_lighting");
			webgl_shader_program.useTexturingUniform = webgl_context.getUniformLocation(webgl_shader_program, "uUseTexturing");

			webgl_shader_program.texture_size_uniform = webgl_context.getUniformLocation(webgl_shader_program, "texture_size");
			webgl_shader_program.samplerUniform = webgl_context.getUniformLocation(webgl_shader_program, "uSampler");
			webgl_shader_program.pMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uPMatrix");
			webgl_shader_program.mvMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uMVMatrix");
			webgl_shader_program.nMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uNMatrix");

			var webgl_vertex_buffer;
			var webgl_color_buffer;
			var webgl_normal_buffer;
			var webgl_texture_coordinate_buffer;
			var webgl_texture_range_buffer;
			var webgl_index_buffer;

			//webgl_context.disable(webgl_context.CULL_FACE);
			webgl_context.enable(webgl_context.CULL_FACE);
			webgl_context.cullFace(webgl_context.FRONT);
			//webgl_context.cullFace(webgl_context.FRONT_AND_BACK);
			if(use_blending) {
				webgl_context.disable(webgl_context.DEPTH_TEST);
				//webgl_context.enable(webgl_context.DEPTH_TEST);

				webgl_context.enable(webgl_context.BLEND);
				webgl_context.blendFunc(webgl_context.ONE, webgl_context.ZERO);
			}else {
				webgl_context.enable(webgl_context.DEPTH_TEST);
				webgl_context.disable(webgl_context.BLEND);
			}
		}//}}}
	}
	if(use_canvas) {
		canvas_context = canvas_element.getContext('2d');
	}

	var state = {
	'player':null,
	'transformations': {
		'modelview': null,
		'perspective': null,
		'normal': null
	},
	'rendering': {
		'leaf': null,
		'indexes':null
		},
	'input' : {
		'touches': {
			'movement' : {
				'start' : null,
				'current' : null
			},
			'look' : {
				'start' : null,
				'current' : null
			},
		}
	}
	};
// Redraw the screen.//{{{
	var redraw = function() {
		function setup_scene() {
			var perspective_matrix = function (fov, aspect, near, far) {
				var t = near*Math.tan(fov*Math.PI/360);
				var r = t*aspect;
				return Matrix.frustum([[-r,r],[-t,t],[near,far]]);
			};
			//mat4.perspective(60, webgl_context.viewportWidth / webgl_context.viewportHeight, 0.1, 100.0, pMatrix);
			//mat4.perspective(60, canvas_element.width / canvas_element.height, 1, 10000.0, pMatrix);
			//var pMatrix = perspective_matrix(60, canvas_element.width / canvas_element.height, 1, 10000);
			state['transformations']['perspective'] =
				perspective_matrix(60, canvas_element.width / canvas_element.height, 0.01, 100);

			state['transformations']['modelview'] = Matrix.identity(4);
			state['transformations']['modelview'] = state['transformations']['modelview'].scale([1/100,1/100,1/100]);
			//state['transformations']['modelview'] = state['transformations']['modelview'].rotate(roll_angle, [Vector.create([1,0,0]),Vector.create([0,1,0])]);
			state['transformations']['modelview'] = state['transformations']['modelview'].rotate(state['player'].pitch_angle-Math.PI/2, [Vector.create([0,1,0]),Vector.create([0,0,1])]);
			//state['transformations']['modelview'] = state['transformations']['modelview'].rotate(-Math.PI/2, [Vector.create([0,1,0]),Vector.create([0,0,1])]);
			state['transformations']['modelview'] = state['transformations']['modelview'].rotate(Math.PI/2-state['player'].direction_angle, [Vector.create([1,0,0]),Vector.create([0,1,0])]);
			state['transformations']['modelview'] = state['transformations']['modelview'].translate(state['player'].viewpoint.scale(-1));

// Clear the automap.
// Clear the canvas.
			if(use_webgl && webgl_context) {
				var setMatrixUniforms = function () {
					//var n = mv.qr()['Q'].slice(0,0,3,3);
					//var n = mv.slice(0,0,3,3).qr()['Q'].transpose();
					state['transformations']['normal'] = state['transformations']['modelview'].slice(0,0,3,3).inverse();
					//state['transformations']['normal'] = state['transformations']['modelview'].qr()['Q'].slice(0,0,3,3).transpose();

					webgl_context.uniform1i(webgl_shader_program.use_lighting_uniform, use_lighting);
					webgl_context.uniform1i(webgl_shader_program.useTexturingUniform, use_texturing);
					webgl_context.uniformMatrix4fv(webgl_shader_program.pMatrixUniform, false, state['transformations']['perspective'].transpose().data);
					webgl_context.uniformMatrix4fv(webgl_shader_program.mvMatrixUniform, false, state['transformations']['modelview'].transpose().data);
					webgl_context.uniformMatrix3fv(webgl_shader_program.nMatrixUniform, false, state['transformations']['normal'].data);
				}

				webgl_context.viewport(0, 0, webgl_context.viewportWidth, webgl_context.viewportHeight);
				webgl_context.clearColor(0.5, 0.5, 0.5, 1.0);
				webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);


				webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_vertex_buffer);
				webgl_context.vertexAttribPointer(webgl_shader_program.vertexPositionAttribute,
					webgl_vertex_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

				/*webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_color_buffer);
				webgl_context.vertexAttribPointer(webgl_shader_program.vertexColorAttribute,
					webgl_color_buffer.item_size, webgl_context.FLOAT, false, 0, 0);*/

				webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_normal_buffer);
				webgl_context.vertexAttribPointer(webgl_shader_program.vertexNormalAttribute,
					webgl_normal_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

				// The texture range.
				webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_texture_range_buffer);
				webgl_context.vertexAttribPointer(webgl_shader_program.vertexTextureRangeAttribute,
					webgl_texture_range_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

				// The texture coordinates.
				webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_texture_coordinate_buffer);
				webgl_context.vertexAttribPointer(webgl_shader_program.vertexTextureCoordinateAttribute,
					webgl_texture_coordinate_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

				// The sampler.
				webgl_context.activeTexture(webgl_context.TEXTURE0);
				while(!texture_atlas);
				webgl_context.bindTexture(webgl_context.TEXTURE_2D, texture_atlas);
				webgl_context.uniform1i(webgl_shader_program.samplerUniform, 0);


				webgl_context.uniform2fv(webgl_shader_program.texture_size_uniform, 
					[texture_atlas.image.width,texture_atlas.image.height]);

				setMatrixUniforms();
			}
		}
		function setup_indexes(leaves) {
			var indexes = [];
			var primitive_key = use_wireframe ? 'edges' : 'triangles';
			leaves.forEach(function (leaf) {
				leaf.face_indexes.forEach(function(face_index) {
					if(use_canvas) {
						indexes.push(level['faces'][face_index].object['edges'].map(function (primitive_indexes) {
							return primitive_indexes[0];
						}));
						/*faces[face_index]['triangles'].forEach(function(primitive_indexes) {
							indexes.push(primitive_indexes);
						});*/
					} else {
						level['faces'][face_index].object[primitive_key].forEach(function(primitive_indexes) {
							/*primitive_indexes.forEach(function (vertex_index) {
								indexes.push(vertex_index);
							});*/
							indexes.push(primitive_indexes);
						});
					}
				});
			});
			return indexes;
		}
		function render_leaves(indexes) {
			if(use_webgl && webgl_context) {
				var flattened_indexes = [].concat.apply([],indexes);

				webgl_context.bindBuffer(webgl_context.ELEMENT_ARRAY_BUFFER, webgl_index_buffer);

				webgl_context.bufferData(
					webgl_context.ELEMENT_ARRAY_BUFFER,
					new Uint16Array(flattened_indexes),webgl_context.STATIC_DRAW);
				webgl_index_buffer.item_count = flattened_indexes.length;
				if(use_wireframe) {
					webgl_context.drawElements(
						webgl_context.LINES, webgl_index_buffer.item_count,
						webgl_context.UNSIGNED_SHORT, 0);
				} else {
					webgl_context.drawElements(
					webgl_context.TRIANGLES,
					webgl_index_buffer.item_count, webgl_context.UNSIGNED_SHORT, 0);
				}
			}
			if(use_canvas && canvas_context) {
				//var mat = mat4.create();
				//mat4.multiply(pMatrix,mvMatrix,mat);
				var mat = state['transformations']['perspective'].multiply(state['transformations']['modelview']);

				var cx = canvas_element.width/2;
				var cy = canvas_element.height/2;

				var rgb = function (c) {
					var color = c.map(function (x) { return Math.floor(Math.abs(x)*255) % 256; });
					return 'rgb(' + ~~color[0] + ',' + ~~color[1] + ',' + ~~color[2] + ')';
				};
				var to_screen = function (projected) {
					//console.log('(x,y,z) : ' + projected[0] + ',' + projected[1] + ',' + projected[2] + ')');
					//console.log('(x,y) : ' + (1+projected[0])*cx + ',' + (1-projected[1])*cy + ')');
					var w = projected[3];
					return [(1+projected[0]/w)*cx,(1-projected[1]/w)*cy];
					//return [(1+projected[2])*cx,(1-projected[1])*cy];
				};

				// Clip the polygon inside [-1,1]^3
				var clip_polygon = function (vertices) {
					var planes = [];
					var w = 1;
					[-1,1].forEach(function(bound) {
						[0,1,2].forEach(function (dimension)  {
							var normal = new Array(3);
							var dist = -w;
							for(var i=0;i<3;i++) {
								normal[i] = i==dimension ? bound : 0;
							}
							planes.push(new Plane({
								'normal' : normal,
								'dist' : dist
							}));
						});
					});
					return clip_polygon_cascade(vertices,planes);
				};

				var draw_polygon = function (vertices,color) {
					//var projected = vertices.map(project_vertex);

					// v[n] = p[n] - p[n-1]
					// N[n] = v[n] x v[n+1]
					/*var v0 = vertices[0].subtract(vertices[vertices.length-1]);
					var v1 = vertices[1].subtract(vertices[0]);
					vec3.cross(v0,v1,n0); 
					vec3.scale(n0,-1);
					vec3.subtract(vertices[0], position,v1);*/

					// We may need to reverse the vertices if they do not follow the right hand rule.
// Backface culling.
					/*if(vec3.dot(n0,v1) > 0)*/ {
						var projected = vertices.map(function (v) { 
							//var p = Vector.create(v.coord.concat([1]));
							return mat.multiply_vector(v.copy().set_at(3,1));
						});
						/*if(projected.map(function (v) { 
							return v.slice(0,3).every(function(x) {
								return -v[3] <= x && x <= v[3]; 
							}) ? 1 : 0;
						}).reduce(function (acc,x) { return acc+x; },0) >=1)*/ {
							projected = clip_polygon(projected);

					//console.log('(r,g,b) : ' + color[0] + ',' + color[1] + ',' + color[2]);
							if(projected.length<3) {
									  //console.log(projected);
							} else {
									  canvas_context.fillStyle = rgb(color);
									  canvas_context.strokeStyle = rgb(color);
									  canvas_context.beginPath();


									  for(var j=0;j<projected.length;j++) {
												 var pos = to_screen(projected[j].coord);
												 if(j==0) {
															canvas_context.moveTo(pos[0],pos[1]);
												 } else {
															canvas_context.lineTo(pos[0],pos[1]);
												 }
									  }


									  canvas_context.closePath();
									  if(use_wireframe) {
												 canvas_context.stroke();
									  } else {
												 canvas_context.fill();
									  }
							}
						}
					}
				};

				var from_indexes = function (triangle_index) {
						  //var vertices = indexes.slice(triangle_index,triangle_index+3).
						  var vertices = triangle_index.map(
												function (index) {
														  return Vector.create(webgl_vertices[index]); 
												});
						  //var color = webgl_normals[triangle_index[0]].map(function (x) { return (x+1)/2; });
						  var color = webgl_colors[triangle_index[0]];

						  draw_polygon(vertices,color);
				}

				if(use_canvas && canvas_context){
						  canvas_context.clearRect(0,0,canvas.width,canvas.height);
				}
				indexes.forEach(from_indexes);
			}
		}

		function setup_pvs(leaf) {
			var visible_leaves;
			if(visilist && !use_wireframe) {
				var list_index = leaf.visilist_start;
				visible_leaves = [leaf];
				for(var i=1;i<level['leaves'].length && level['leaves'][i]['visilist start']>=0;list_index++) {
					if(visilist[list_index] == 0) {
						i += 8*visilist[list_index+1];
						list_index++;
					} else {
						for(var j=0;j<8;j++,i++) {
							if(((visilist[list_index] >> j) & 1) != 0) {
								var leaf = level['leaves'][i].object;
								visible_leaves.push(leaf);
							}
						}
					}
				}
			} else {
				visible_leaves = leaves;
			}
		// Find the children of the common ancestor of two leaves. The return value is an array
		// where the first value is an ancestor of a and the second is an ancestor of b.
			function common_ancestor(a,b) {
				var a_ancestor = a;
				var b_ancestor = b;
				while(a_ancestor.depth > b_ancestor.depth) {
					a_ancestor = a_ancestor.parent_node;
				}
				while(b_ancestor.depth > a_ancestor.depth) {
					b_ancestor = b_ancestor.parent_node;
				}
				while(a_ancestor.parent_node != null && a_ancestor.parent_node != b_ancestor.parent_node) {
					a_ancestor = a_ancestor.parent_node;
					b_ancestor = b_ancestor.parent_node;
				}
				var ancestor = a_ancestor.parent_node;
				var left_side = ancestor.child_nodes[0];
				if(left_side == null) {
					left_side = ancestor.child_leaves[0];
				}
				return {'ancestor': ancestor,'side': left_side == a_ancestor ? 0 : 1 };
			}
			// Sort the leaves.
			visible_leaves.sort(function (a,b) {
				var ancestor_information = common_ancestor(a,b);
				var ancestor = ancestor_information['ancestor'];
				var side = ancestor.plane.normal_equation(state['player'].viewpoint) > 0 ? 0 : 1;
				return (side == ancestor_information['side']) ? -1 : 1;
			});
			visible_leaves.reverse();
	
			return visible_leaves;
		}

		var leaf = bsp.find_leaf(state['player'].viewpoint);

		setup_scene();
		if(leaf != state['rendering']['leaf'] || redo_indexes) {
			var visible_leaves = setup_pvs(leaf);
			state['rendering']['indexes'] = setup_indexes(visible_leaves);
			state['rendering']['leaf'] = leaf;
			redo_indexes = false;
		}
		render_leaves(state['rendering']['indexes']);

	};//}}}



	use_wireframe = $('#wireframe_option').prop('checked');

	//use_webgl = $('#webgl_option').prop('checked');
	//use_canvas = $('#canvas_option').prop('checked');

	var bsp = null;
	var faces = null;
	var visilist = null;
	var leaves = null;
	var redo_indexes = false;
	level = null;
	texture_indexes = null;

	var webgl_vertices;
	var webgl_colors;
	var webgl_normals;
	var webgl_texture_range;
	var webgl_texture_coordinate;
// Select and load a map.//{{{
	function select_map()
	{
		var map_name = $('#map_option').val();
		var map_directory = map_name.split('/')[0];
		bsp = null;

		$.ajax({async: false,
					type: 'GET',
					url: 'data/' + map_name,
					data: null,
					success: function(d) {level=d;},
					dataType: 'json'});

		$.ajax({async: false,
					type: 'GET',
					url: 'data/' + map_directory + '/texture_index.json',
					data: null,
					success: function(d) {texture_indexes=d;},
					dataType: 'json'});


		var root_index = 0;
		if(level['root node']) {
			root_index = level['root node'];
		}
		bsp = new Node(level['nodes'][root_index],level);

		//leaves = level['leaves'].map(function (definition) { return definition.object; });

		leaves = bsp.leaves();

		webgl_vertices = [];
		webgl_normals = [];
		webgl_colors = [];
		webgl_texture_coordinate = [];
		webgl_texture_range = [];

		level['faces'].forEach(
			function (definition) {
				definition.object = new Face(definition,level,
				{
					'vertex' : webgl_vertices,
					'normal' : webgl_normals,
					'color' : webgl_colors,
					'texture range' : webgl_texture_range,
					'texture coordinate' : webgl_texture_coordinate
				});
			}
		);
		visilist = level['visibility list'];
// Set up the vertex and color buffers.//{{{
		function initialize_buffer(context, data, datatype,item_size) {
			var buffer = context.createBuffer();
			context.bindBuffer(context.ARRAY_BUFFER, buffer);
			var flattened_data;
			if(data[0].length) {
				/*var flattened_data = data.reduce(function (acc, v) { 
					return acc.concat(v);
				});*/
				flattened_data = [].concat.apply([], data);
				buffer.item_size = data[0].length;
				buffer.item_count = data.length;
			} else {
				flattened_data = data;
				buffer.item_size = item_size;
				buffer.item_count = data.length / buffer.item_size;
			}
			context.bufferData(context.ARRAY_BUFFER,new datatype(flattened_data),context.STATIC_DRAW);
			return buffer;
		} 
		if(use_webgl && webgl_context) {
// Load the vertices into WebGL.
			webgl_vertex_buffer = initialize_buffer(webgl_context, webgl_vertices, Float32Array,3);

// Load the colors into WebGL.
			webgl_color_buffer = initialize_buffer(webgl_context, webgl_colors, Float32Array,3);
			webgl_normal_buffer = initialize_buffer(webgl_context, webgl_normals, Float32Array,3);
			webgl_texture_range_buffer = initialize_buffer(webgl_context, webgl_texture_range, Float32Array,4);
			webgl_texture_coordinate_buffer = initialize_buffer(webgl_context, webgl_texture_coordinate, Float32Array,4);
			webgl_index_buffer = webgl_context.createBuffer();
		}

		function handle_loaded_texture(texture) {
			webgl_context.bindTexture(webgl_context.TEXTURE_2D, texture);
			//webgl_context.pixelStorei(webgl_context.UNPACK_FLIP_Y_WEBGL, true);
			webgl_context.texImage2D(
				webgl_context.TEXTURE_2D, 0, 
				webgl_context.RGBA, webgl_context.RGBA, webgl_context.UNSIGNED_BYTE, texture.image);
			webgl_context.texParameteri(
				webgl_context.TEXTURE_2D, webgl_context.TEXTURE_MAG_FILTER, webgl_context.LINEAR);
			webgl_context.texParameteri(
				webgl_context.TEXTURE_2D, webgl_context.TEXTURE_MIN_FILTER, webgl_context.LINEAR);
			webgl_context.bindTexture(webgl_context.TEXTURE_2D, null);
		}
		/*function handle_loaded_cubemap(texture) {
			webgl_context.bindTexture(webgl_context.TEXTURE_CUBE_MAP_NEGATIVE_X, texture) {
		}*/

		function init_texture() {
			if(webgl_context) {
				var texture_atlas = webgl_context.createTexture();
				texture_atlas.image = new Image();
				texture_atlas.image.onload = function() {
					handle_loaded_texture(texture_atlas)
				}

				texture_atlas.image.src = "data/" + map_directory + "/texture.png";
				return texture_atlas
			}
		}
		texture_atlas = init_texture();

		level['leaves'].forEach(function (leaf_definition) {
			if(leaf_definition.object) {
				//leaf_definition.object.color = [Math.random(),Math.random(),Math.random(),1];
				leaf_definition.object.faces = [];
				leaf_definition['face indexes'].forEach(
					function (face_index) {
						var face_definition = level['faces'][face_index];
						leaf_definition.object.faces.push(face_definition);
					}
				);
			}
		});

		var player_origin = level['player start']['origin'];
		var player_angle = Math.PI*level['player start']['angle']/180;
		var player = new Viewer(Math.PI/3,player_angle,Vector.create(player_origin));
		return player;
	}

	state['player'] = select_map();
	function redraw2() {
		if($('#webgl_option').length == 0) {
				use_webgl = true;
				use_canvas = !use_webgl;
				redraw();
		} else {
			if($('#canvas_option').prop('checked')) {
				redo_indexes = true;
				use_webgl = false;
				use_canvas = !use_webgl;
				redraw();
			}

			if($('#webgl_option').prop('checked')) {
				redo_indexes = true;
				use_webgl = true;
				use_canvas = !use_webgl;
				redraw();
				use_canvas = true;
			}
		}
	}

	var lastTime = 0;

	var roll_angle = 0;
	var pitch_angle = 0;
	var animate = function () {
     	var timeNow = new Date().getTime();
      if (lastTime != 0) {
			
			var elapsed = timeNow - lastTime;
			/*pitch_angle = Math.sin((Math.sqrt(2)/3)*timeNow/1000.0)*Math.PI*2*(10/360);
			roll_angle = Math.sin((Math.sqrt(3)/5)*timeNow/1000.0)*Math.PI*2*(6/360);*/


// Gravity
			if(!use_noclip) {
				state['player'].move_up(-config['physics']['gravity']*elapsed,test_intersection);
			}
		}
		lastTime = timeNow;
	}

//redraw2();
	function tick()
	{
		animate();
		redraw2();
		window.setTimeout(tick, 1000/24);
	}
	tick();

	$('#canvas_option').change(function() {
		if(canvas_context) {
			canvas_context.clearRect(0,0,canvas.width,canvas.height);
		}
	});
	$('#webgl_option').change(function() {
		if(webgl_context) {
			webgl_context.clearColor(0.5, 0.5, 0.5, 1.0);
			webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);
		}
	});
	$('#noclip_option').change(function() {
		use_noclip = $('#noclip_option').prop('checked');
	});
	$('#lighting_option').change(function() {
		use_lighting = $('#lighting_option').prop('checked');
	});
	$('#texturing_option').change(function() {
		use_texturing = $('#texturing_option').prop('checked');
	});
	$('#wireframe_option').change(function() {
		use_wireframe = $('#wireframe_option').prop('checked');
		redo_indexes = true;
	});
	$('#map_option').change(function() {
		state['player'] = select_map();
	});

	$('#canvas').bind('touchstart',function (e) {
		e.preventDefault();
		for(var i=0; i < e.originalEvent.touches.length;i++) {
			var touch = e.originalEvent.touches[i];
			var element = $(this).offset();
			var x = touch.pageX - element.left;
			var y = touch.pageY - element.top;
			if(0 < x && x < $(this).width()/2) {
				state['input']['touches']['movement']['start'] = [x,y];
			} else if ($(this).width()/2 < x && x < $(this).width()) {
				state['input']['touches']['look']['start'] = [x,y];
			}
		}
	});
	$('#canvas').bind('touchmove',function (e) {
		var clamp_stick = function (coord, radius) {
			var input_radius = Math.sqrt(coord[0]*coord[0]+coord[1]*coord[1]);
			if(input_radius < radius) {
				return coord;
			} else {
				return coord.map(function (x) { return x*radius/input_radius; });
			}
		};
		e.preventDefault();
		for(var i=0; i < e.originalEvent.touches.length;i++) {
			var touch = e.originalEvent.touches[i];
			var element = $(this).offset();
			var x = touch.pageX - element.left;
			var y = touch.pageY - element.top;
			if(0 < x && x < $(this).width()/2) {
				var start = state['input']['touches']['movement']['start'];
				if(start) {
					var coord = clamp_stick([x-start[0],y-start[1]],config['analog stick radius']);
					state['input']['touches']['movement']['current'] = coord;
				} else {
					state['input']['touches']['movement']['current'] = null;
				}
			} else if ($(this).width()/2 < x && x < $(this).width()) {
				var start = state['input']['touches']['look']['start'];
				if(start) {
					var coord = clamp_stick([x-start[0],y-start[1]],config['analog stick radius']);
					state['input']['touches']['look']['current'] = coord;
				} else {
					state['input']['touches']['look']['current'] = null;
				}
			}
		}
	});
	var touchend = function (e) {
		e.preventDefault();
		for(var i=0; i < e.originalEvent.changedTouches.length;i++) {
			var touch = e.originalEvent.changedTouches[i];
			var element = $(this).offset();
			var x = touch.pageX - element.left;
			var y = touch.pageY - element.top;
			if(0 < x && x < $(this).width()/2) {
				state['input']['touches']['movement']['start'] = null;
				state['input']['touches']['movement']['current'] = null;
			} else if ($(this).width()/2 < x && x < $(this).width()) {
				state['input']['touches']['look']['start'] = null;
				state['input']['touches']['look']['current'] = null;
			}
		}
	};
	$('#canvas').bind('touchend',touchend);
	$('#canvas').bind('touchleave',touchend);
	$('#canvas').bind('touchcancel',touchend);


// Return true if there is no intersection with the map.
	function test_intersection(previous, viewpoint) {
		if(use_noclip) {
			return true;
		}
		var leaf = bsp.find_leaf(viewpoint);
		var radius = 20;
		if(!leaf.faces.every(function (face) {
			return !face.object.intersects_sphere(viewpoint,radius);
		})) {
			return false;
		}
		var previous_leaf = bsp.find_leaf(previous);
		if(!previous_leaf.faces.every(function (face) {
			return !face.object.intersects_segment(previous,viewpoint);
		})) {
			return false;
		}
		return true;
	}
	(function () {
		//"use strict";
		var depressed = [];
		window.onkeydown = function (event) {
				event.preventDefault();
			if(Object.keys(config['keycodes']['keys']).indexOf(event.which.toString()) >= 0 ||
				Object.keys(config['keycodes']['modifiers']).indexOf(event.which.toString()) >= 0 
			) {
			}
			if (depressed.indexOf(event.which) < 0) {
				depressed.push(event.which);
			}
		};
		window.onkeyup = function (event) {
			var index = depressed.indexOf(event.which);
			if (index >= 0) {
				depressed.splice(index, 1);
			}
		};
		window.setInterval(function () {
			if(state['input']['touches']['movement']['start'] != null && state['input']['touches']['movement']['current'] != null) {
				var change = state['input']['touches']['movement']['current'];

				//var vertical_scale = -2.0e-2;
				//var horizontal_scale = -2.0e-2;
				var vertical_scale = -config['analog stick scale']/config['analog stick radius'];
				var horizontal_scale = -config['analog stick scale']/config['analog stick radius'];
				var up_amount = change[1] * vertical_scale;
				if(up_amount > 0) {
					state['player'].move_forward(config['movement']['forward distance']*up_amount, test_intersection);
				} else if(up_amount<0) {
					state['player'].move_forward(config['movement']['backward distance']*up_amount, test_intersection);
				}

				var left_amount = change[0] * horizontal_scale;
				state['player'].move_left(config['movement']['strafe distance']*left_amount, test_intersection);
			}


			if(state['input']['touches']['look']['start'] != null && state['input']['touches']['look']['current'] != null) {
				var change = state['input']['touches']['look']['current'];
				//var vertical_scale = 3.0e-2;
				//var horizontal_scale = -2.0e-2;
				var vertical_scale = config['analog stick scale']/config['analog stick radius'];
				var horizontal_scale = -config['analog stick scale']/config['analog stick radius'];
				var up_amount = change[1] * vertical_scale;
				state['player'].pitch_angle = clamp(state['player'].pitch_angle + up_amount *2*Math.PI/360,-Math.PI/2,Math.PI/2);

				var left_amount = change[0] * horizontal_scale;
				//state['player'].move_left(config['look']['strafe distance']*left_amount, test_intersection);
				state['player'].turn_left(config['movement']['turn angle'] * left_amount * 2 * Math.PI / 360);

			}


			var keys = [];
			var modifiers = [];

			depressed.forEach(function (keycode) {
				var key = config['keycodes']['keys'][keycode];
				if(key) {
					keys.push(key);
				}
				var modifier = config['keycodes']['modifiers'][keycode];
				if(modifier) {
					modifiers.push(modifier);
				}
			});
			modifiers = modifiers.sort();
			var bindings = [];
			config['key bindings'].forEach(function  (binding) {
				var key_index = keys.indexOf(binding['key']);
				if(key_index >= 0) {
					if(binding['modifiers'].sort().equals(modifiers)) {
						bindings.push(binding);
					}
				}
			});
			bindings.forEach(function (binding) {
			switch(binding['binding']) {
				//{{{
				case 'move left': {
						state['player'].move_left(config['movement']['strafe distance'], test_intersection);
				}
				break;
				case 'move right': {
						state['player'].move_left(-config['movement']['strafe distance'], test_intersection);
				}
				break;
				case 'move up': {
						state['player'].move_up(config['movement']['vertical distance'], test_intersection);
				}
				break;
				case 'move down': {
						state['player'].move_up(-config['movement']['vertical distance'], test_intersection);
				}
				break;
				case 'move forward': {
						state['player'].move_forward(config['movement']['forward distance'], test_intersection);
				}
				break;
				case 'move backward': {
						state['player'].move_forward(-config['movement']['backward distance'], test_intersection);
				}
				break;
				case 'turn left': {
						state['player'].turn_left(config['movement']['turn angle'] * 2 * Math.PI / 360);
				}
				break;
				case 'turn right': {
						state['player'].turn_left(-config['movement']['turn angle'] * 2 * Math.PI / 360);
				}
				break;
				//}}}
				}
			});
		}, config['key refresh']);
	}());
});//}}}
