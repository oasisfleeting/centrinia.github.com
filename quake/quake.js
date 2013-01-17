

var texture_images;
var log_draw_count;
var log_traverse_count;
var canvas_element;
var canvas_context;
var webgl_context;

var use_individual_vertices = true;
var maximum_draw_count = null;
var use_wireframe = false;
var use_texturing = true;
var use_canvas = true;
var use_webgl = use_canvas;
//var use_canvas = false;
//var use_webgl = true;
var use_noclip = false;
var use_blending = true;

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

Array.prototype.shuffle = function () {
	for (var i = this.length - 1; i > 0; i--) {
		var j = Math.floor(Math.random() * (i + 1));
		var tmp = this[i];
		this[i] = this[j];
		this[j] = tmp;
	}

	return this;
}//}}}

// A line segment.//{{{
function Segment(endpoint0,endpoint1)
{
	this.endpoints = [];
	this.endpoints[0] = endpoint0;
	this.endpoints[1] = endpoint1;

// This vector is normal to the perpendicular line.
	this.parallel = this.endpoints[0].subtract(this.endpoints[1]);
	this.endpoint_displacements = [];
	this.endpoint_displacements[0] = this.parallel.dot(this.endpoints[0]);
	this.endpoint_displacements[1] = this.parallel.dot(this.endpoints[1]);
	if(this.endpoint_displacements[0]>this.endpoint_displacements[1]) {
		var t = this.endpoint_displacements[0];
		this.endpoint_displacements[0] = this.endpoint_displacements[1];
		this.endpoint_displacements[1] = t;
	}

// Compute the equation for the line. By convention the first endpoint is on the left when standing on
// the positive halfplane w.r.t. the normal.
	this.normal = new Vector2([-this.parallel.coord[1],this.parallel.coord[0]]);
	this.displacement = this.normal.dot(this.endpoints[0]);

	this.midpoint = function () {
		return this.endpoints[0].add(this.endpoints[1]).scale(1/2);
	};
	this.length = function () {
		return this.endpoints[0].distance(this.endpoints[1]);
	};
// Determine if a point is inside a line segment.//{{{
	this.inside = function (vec)
	{
		var d = this.parallel.dot(vec);
		return this.endpoint_displacements[0] <= d && d <= this.endpoint_displacements[1];
	};//}}}
// Intersect this line with the argument and return the intersection point.
	this.intersect = function (segment)
	{
		var det = this.normal.coord[0]*segment.normal.coord[1]-this.normal.coord[1]*segment.normal.coord[0];
		var a0 = this.displacement*segment.normal.coord[1]-this.normal.coord[1]*segment.displacement;
		var a1 = this.normal.coord[0]*segment.displacement-this.displacement*segment.normal.coord[0];
		return new Vector2([a0/det,a1/det]);
	};
	this.normal_equation = function (vec)
	{
		return this.normal.dot(vec) - this.displacement;
	};
	this.intersects_circle = function (center,radius) {
//  http://mathworld.wolfram.com/Circle-LineIntersection.html
		var endpoints = this.endpoints.map(function (p) { return p.subtract(center); });
		if(endpoints[0].norm() < radius || endpoints[1].norm() < radius){
		  return true;
		}
		var d = [0,1].map(function (i) { return endpoints[1].coord[i] - endpoints[0].coord[i]});
		var d_r_squared = d[0]*d[0]+d[1]*d[1];
		var D = endpoints[0].coord[0]*endpoints[1].coord[1] - endpoints[1].coord[0]*endpoints[0].coord[1];
		var discriminant = radius*radius*d_r_squared - D*D;
		if(discriminant <= 0) {
			return false;
		}
		var t = Math.sqrt(discriminant);
		var p0 = new Vector2([D*d[1], -D*d[0]]);
		var p1 = new Vector2([(d[1] < 0 ? -1 : 1) *d[0]*t, Math.abs(d[1])*t]);
		var z0 = p0.add(p1).scale(1/d_r_squared).add(center);
		var z1 = p0.subtract(p1).scale(1/d_r_squared).add(center);
		return this.inside(z0) || this.inside(z1);
	}
}//}}}

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

// A viewing frustum.//{{{
function Viewer(elevation,fov,direction,viewpoint,screen_elevations)
{
// Compute the location in the viewing arc that the point is in. Return a value in [0,1] if the point is in the viewing//{{{
// frustum. The left edge of the frustum corresponds to 0 and the right edge corresponds to 1. */
	this.arc_fraction = function (point)
	{
		/*var t = point.subtract(this.viewpoint);
		var angle = Math.atan2(t.coord[1],t.coord[0]);

		var result = (angle-(this.direction-this.fov/2))/this.fov;
		return result - Math.floor(result);*/

		var t = point.subtract(this.viewpoint);
		var right_ray = this.super_frustum_right.endpoints[0].subtract(this.viewpoint);
		//return this.map_coordinate_interval.map(right_ray.angle_to(t)/this.super_fov);
		return right_ray.angle_to(t)/this.super_fov;
	};//}}}
// Update the frustum.//{{{
	this.update = function(fov,viewpoint,direction,elevation, intersection_handler) {
		if(!intersection_handler(viewpoint, elevation)){
		  return;
		}
		this.viewpoint = viewpoint;
		this.elevation = elevation;

		if(typeof direction == 'number') {
			this.direction_vector = (new Vector2([1,0])).rotate(direction);
			this.direction_angle = direction;
		} else {
			this.direction_vector = direction;
			this.direction_angle = this.direction_vector.angle_to(new Vector2([1,0]));
			if(this.direction_vector.coord[1] < 0) {
				this.direction_angle = 2*Math.PI - this.direction_angle;
			}
		}

		this.frustum = [];

		var left_ray = this.direction_vector.rotate(this.fov/2);
		this.frustum[0] = new Segment(this.viewpoint, left_ray.add(this.viewpoint));

		var right_ray = this.direction_vector.rotate(-this.fov/2);
		this.frustum[1] = new Segment(right_ray.add(this.viewpoint), this.viewpoint);

		this.super_direction_vector = this.direction_vector;
		this.super_frustum_right = this.frustum[1];
	};//}}}
// Determine if the given point is inside the frustum.//{{{
	this.inside_frustum = function(point)
	{
		return this.frustum[0].normal_equation(point)>0 && this.frustum[1].normal_equation(point)>0;
	};//}}}
// Move the viewpoint forward.//{{{
	this.move_forward = function (step, intersection_handler)
	{
		//var viewbobAmplitude = 2.0;
		var viewbobAmplitude = 0.0;
		var viewbobFrequency = 1/10;
		this.stepDistance++;
		var viewbob = Math.cos(this.stepDistance*2*Math.PI*viewbobFrequency)*viewbobAmplitude;

		var vector = this.direction_vector.scale(step);
		this.update(this.fov,this.viewpoint.add(vector),this.direction_vector,this.elevation+viewbob, intersection_handler);
	};//}}}
// Move the viewpoint to the left.//{{{
	this.move_left = function (step, intersection_handler)
	{
		var viewbobAmplitude = 0.0;
		//var viewbobAmplitude = 0.0;
		var viewbobFrequency = 1/10;
		this.stepDistance++;
		var viewbob = Math.cos(this.stepDistance*2*Math.PI*viewbobFrequency)*viewbobAmplitude;

		var vector = new Vector2([-step*this.direction_vector.coord[1],step*this.direction_vector.coord[0]]);
		this.update(this.fov,this.viewpoint.add(vector),this.direction_vector,this.elevation+viewbob, intersection_handler);
	};//}}}
// Move the viewpoint up.//{{{
	this.move_up = function (step,intersection_handler)
	{
		this.update(this.fov,this.viewpoint,this.direction_vector,this.elevation+step,intersection_handler);
	};//}}}
// Turn the frustum to the left.//{{{
	this.turn_left = function (step)
	{
		this.update(this.fov,this.viewpoint,this.direction_vector.rotate(step),this.elevation, function (v,e) { return true; });
	};//}}}
// Directly set the viewpoint.//{{{
	this.set_viewpoint = function(viewpoint) {
		this.update(this.fov,viewpoint,this.direction_vector,this.elevation, function(v,e) { return true; });
	};//}}}
	this.update_elevations = function(elevations) {
	  for(var i=0;i<2;i++) {
			this.screen_elevations[0][i] = this.screen_elevations[0][i] > elevations[0][i] ? this.screen_elevations[0][i] : elevations[0][i];
			this.screen_elevations[1][i] = this.screen_elevations[1][i] < elevations[1][i] ? this.screen_elevations[1][i] : elevations[1][i];
	  }
	}
	this.fov = fov;
	this.super_fov = fov;
// The first index corresponds to top or bottom and the second index corresponds to left or right.
	if(screen_elevations) {
		this.screen_elevations = screen_elevations;
	} else {
// Use the default elevations for the top and bottom of the screen.
		this.screen_elevations = [[0,0],[canvas_element.height,canvas_element.height]];
	}
	this.stepDistance = 0;
	this.update(fov,viewpoint,direction,elevation, function (v,e) { return true;});
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
	this.normal = definition['normal'];
	this.displacement = definition['dist'];
	this.normal_equation = function (point) {
		var result = -this.displacement;
		for(var i=0;i<3;i++) {
			result += point[i]*this.normal[i];
		}
		return result;
	};
}
	Plane.prototype.intersect_line = function (start,end) {
		var n = this.normal.concat([-this.displacement]);
		var direction = vec4_sub(start,end);
		var denominator = vec4_dot(direction,n);
		if(denominator != 0) {
			var vector = new Array(4);
			t = vec4_dot(n,start) / denominator;
			for(var i=0;i<4;i++) {
				vector[i] = start[i] * (1-t) + end[i]*(t);
			}
			return {
				't' : t,
				'vector' : vector
			};
		} else {
			return null;
		}
	};

function Leaf(leaf,level,parent_node) {
	this.type = leaf['type'];
	this.depth = parent_node.depth+1;
	this.parent_node = parent_node;
	this.visilist_start = leaf['visilist start'];
	this.face_indexes = leaf['face indexes'];
}

function Node(node,level,parent_node) {
	if(parent_node) {
		this.parent_node = parent_node;
		this.depth = parent_node.depth+1;
	} else {
		this.parent_node = null;
		this.depth = 0;
	}
	this.plane = new Plane(level['planes'][node['plane id']]);

	var this_node = this;
	this.child_nodes = node['children nodes'].map(function (index) {
		var definition = level['nodes'][index];
		if(definition != null) {
			return new Node(definition,level,this_node);
		} else {
			return null;
		}
	});
	this.child_leaves = node['children leaves'].map(function (index) {
		var definition = level['leaves'][index];
		if(definition != null) {
			leaf = new Leaf(definition,level,this_node);
			leaf.index = index;
			return leaf;
		} else {
			return null;
		}
	});
	// Visit every leaf and node in infix order and call the handler on them.
	this.dfs = function (point,leaf_handler,node_handler,back_to_front) {
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
	this.find_leaf = function (point) {
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
	this.leaves = function () {
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
	}
}
				function vec4_scale(a,b) {
					var c = new Array(4);
					for(var i=0;i<4;i++) {
						c[i] = a[i]*b;
					}
					return c;
				}
				function vec4_dot(a,b) {
					var c = 0;
					for(var i=0;i<4;i++) {
						c += a[i]*b[i];
					}
					return c;
				}
				function vec4_add(a,b) {
					var c = new Array(4);
					for(var i=0;i<4;i++) {
						c[i] = a[i]+b[i];
					}
					return c;
				}
				function vec4_sub(a,b) {
					var c = new Array(4);
					for(var i=0;i<4;i++) {
						c[i] = a[i]-b[i];
					}
					return c;
				}
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
		return vec3.dot(plane.normal,v) - plane.displacement*v[3];
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
					var intersection = Plane.prototype.intersect_line.apply(worker['plane'],working);
					if(!intersection) {
						console.log(working);
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
	canvas_element = document.getElementById(canvas_name);
	canvas_element2 = document.getElementById('canvas2');

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
			webgl_context.viewportWidth = canvas_element.width;
			webgl_context.viewportHeight = canvas_element.height;
			function get_shader(context,type,filename) {
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
			function initialize_shaders(context) {
				var vertex_shader;
				var vertex_shader = get_shader(context, 'x-shader/x-vertex', 'vertex.glsl');
				var fragment_shader = get_shader(context, 'x-shader/x-fragment', 'fragment.glsl');

				var shader_program = webgl_context.createProgram();
				context.attachShader(shader_program, vertex_shader);
				context.attachShader(shader_program, fragment_shader);
				context.linkProgram(shader_program);

				if(!context.getProgramParameter(shader_program,context.LINK_STATUS)) {
					alert('Could not link shaders' + context.getShaderInfoLog(shader_program));
					return null;
				}
			   context.useProgram(shader_program);

				return shader_program;
			}

			var webgl_shader_program = initialize_shaders(webgl_context);

			webgl_shader_program.vertexPositionAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexPosition");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexPositionAttribute);
			webgl_shader_program.vertexColorAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexColor");
			webgl_context.enableVertexAttribArray(webgl_shader_program.vertexColorAttribute);

			webgl_shader_program.pMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uPMatrix");
			webgl_shader_program.mvMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uMVMatrix");
			webgl_shader_program.dummyUniform = webgl_context.getUniformLocation(webgl_shader_program, "uDummy");

			var webgl_vertex_buffer;
			var webgl_color_buffer;
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

	use_wireframe = $('#wireframe_option').prop('checked');

	//use_webgl = $('#webgl_option').prop('checked');
	//use_canvas = $('#canvas_option').prop('checked');

	var bsp = null;
	var faces = null;
	var leaves = null;
	var visilist = null;
	var level = null;

	var webgl_vertices;
	var webgl_colors;
// Select and load a map.//{{{
	function select_map()
	{
		var map_name = $('#map_option').val();
		bsp = null;

		$.ajax({async: false,
					type: 'GET',
					url: 'maps/' + map_name,
					data: null,
					success: function(d) {level=d;},
					dataType: 'json'});


		bsp = new Node(level['nodes'][0],level);
		leaves = bsp.leaves();

		webgl_vertices = [];
		webgl_normals = [];
		webgl_colors = [];

		if(!use_individual_vertices) {
			level['vertices'].forEach(function (vertex) {
				webgl_vertices.push(vertex);
				webgl_colors.push([Math.random(),Math.random(),Math.random()]);
			});
		}
		/*leaves = level['leaves'].map(function (definition) {
			return new Leaf(definition,level);
		});*/


		faces = level['faces'].map(
			function (definition) {
				var indexes = definition['vertices index'];

				var head = indexes[0];
				var tail1 = indexes.slice(1,indexes.length-1);
				var tail2 = indexes.slice(2,indexes.length);
				var triangles = new Array(indexes.length-2);
				var edges = new Array(indexes.length);
				var face_color = [Math.random(),Math.random(),Math.random()];
				var normal = new Array(3);
				var v0 = new Array(3);
				var v1 = new Array(3);
				vec3.subtract(level['vertices'][indexes[indexes.length-1]],level['vertices'][indexes[0]],v0);
				//vec3.subtract(level['vertices'][indexes[2]],level['vertices'][indexes[1]],v0);
				vec3.subtract(level['vertices'][indexes[1]],level['vertices'][indexes[0]],v1);
				vec3.cross(v0,v1,normal);
						vec3.normalize(normal);

				function get_index(index) {
					if(use_individual_vertices) {
						var result = webgl_vertices.length;

						webgl_vertices.push(level['vertices'][index]);
						webgl_colors.push(face_color);

						webgl_normals.push(normal);
						return result;
					} else {
						return index;
					}
				}
				head_index = get_index(head);
				for(var i=0;i<tail1.length;i++) {
					var tail1_index = get_index(tail1[i]);
					var tail2_index = get_index(tail2[i]);
					triangles[i] = [head_index, tail1_index, tail2_index];
					edges[i+1] = [tail1_index, tail2_index];
				}
				edges[0] = [head_index,triangles[0][0]];
				edges[indexes.length-1] = [triangles[triangles.length-1][2],head_index];
				return { 'triangles': triangles, 'edges': edges};
			}
		);
		visilist = level['visibility list'];
// Set up the vertex and color buffers.//{{{
		function initialize_buffer(context, data, datatype,item_size) {
			buffer = context.createBuffer();
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
			webgl_index_buffer = webgl_context.createBuffer();
		}

		leaves.forEach(function (leaf) {
			leaf.color = [Math.random(),Math.random(),Math.random(),1];
		});

		player_origin = level['player start']['origin'];
		player_angle = Math.PI*level['player start']['angle']/180;
		player = new Viewer(player_origin[2],Math.PI/3,player_angle,new Vector2([player_origin[0], player_origin[1]]));
		return player;
	}

	//var player = new Viewer(0, Math.PI/3, Math.PI/2, new Vector2([0,0]));
	var player = select_map();
// Redraw the screen and automap.//{{{
	var redraw = function() {
		log_draw_count=0;
		log_traverse_count=0;

		var mvMatrix = mat4.create();
		var pMatrix = mat4.create();
		//mat4.perspective(60, webgl_context.viewportWidth / webgl_context.viewportHeight, 0.1, 100.0, pMatrix);
		mat4.perspective(60, canvas_element.width / canvas_element.height, 0.1, 100.0, pMatrix);

		/*mat4.multiply(
		[	0,0,-1,0,
			0,1,0,0,
			1,0,0,0,
			0,0,0,1
		],pMatrix,pMatrix);*/

		mat4.identity(mvMatrix);
		mat4.scale(mvMatrix,[1/100,1/100,1/100]);
		mat4.rotate(mvMatrix, -Math.PI/2, [1,0,0]);
		mat4.rotate(mvMatrix, Math.PI/2-player.direction_angle, [0,0,1]);
		mat4.translate(mvMatrix, [-player.viewpoint.coord[0], -player.viewpoint.coord[1], -player.elevation]);
	
		//mat4.identity(mvMatrix);
//pMatrix=[-0.017107263207435608, 1.6590503052034653e-19, 0.0015674764290452003, 0.0015643446240574121, -0.002709524240344763, -1.0474832115039693e-18, -0.009896657429635525, -0.009876883588731289, 0, 0.017320508137345314, -6.13529011016107e-19, -6.123031810470917e-19, 10.528736114501953, -0.5822814106941223, 1.9998770952224731, 2.195681571960449];
// Clear the automap.
// Clear the canvas.
		if(use_webgl && webgl_context) {
			webgl_context.viewport(0, 0, webgl_context.viewportWidth, webgl_context.viewportHeight);
			webgl_context.clearColor(0.5, 0.5, 0.5, 1.0);
			webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);



			function setMatrixUniforms(p,mv) {
			var pMatrix = mat4.create();
				webgl_context.uniformMatrix4fv(webgl_shader_program.pMatrixUniform, false, p);
				webgl_context.uniformMatrix4fv(webgl_shader_program.mvMatrixUniform, false, mv);
			}

			webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_vertex_buffer);
			webgl_context.vertexAttribPointer(webgl_shader_program.vertexPositionAttribute,
					webgl_vertex_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

			webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_color_buffer);
			webgl_context.vertexAttribPointer(webgl_shader_program.vertexColorAttribute,
					webgl_color_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

			setMatrixUniforms(pMatrix,mvMatrix);
		}
		if(use_canvas && canvas_context){
			canvas_context.clearRect(0,0,canvas.width,canvas.height);
		}

		var position = [player.viewpoint.coord[0], player.viewpoint.coord[1], player.elevation];
		var leaf = bsp.find_leaf(position);
		
		var visible_leaves;
		if(visilist) {
			visible_leaves = [leaf];
			var list_index = leaf.visilist_start;
			for(var i=1;i<leaves.length;list_index++) {
				if(visilist[list_index] == 0) {
					i += 8*visilist[list_index+1];
					list_index++;
				} else {
					for(var j=0;j<8;j++,i++) {
						if(((visilist[list_index] >> j) & 1) != 0) {
							var leaf = leaves[i];
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
			var side = ancestor.plane.normal_equation(position) > 0 ? 0 : 1;
			return (side == ancestor_information['side']) ? -1 : 1;
		});
		visible_leaves.reverse();

		function render_leaves(leaves) {
			var indexes = [];
			var primitive_key = use_wireframe ? 'edges' : 'triangles';
			leaves.forEach(function (leaf) {
				leaf.face_indexes.forEach(function(face_index) {
					if(use_canvas) {
						indexes.push(faces[face_index]['edges'].map(function (primitive_indexes) {
							return primitive_indexes[0];
						}));
						/*faces[face_index]['triangles'].forEach(function(primitive_indexes) {
							indexes.push(primitive_indexes);
						});*/
					} else {
						faces[face_index][primitive_key].forEach(function(primitive_indexes) {
							/*primitive_indexes.forEach(function (vertex_index) {
								indexes.push(vertex_index);
							});*/
							indexes.push(primitive_indexes);
						});
					}
				});
			});
			//indexes =[33399, 33402, 33403];
			//indexes=[[29487, 29488, 29490, 29491]];
		console.log('indexes length: ' + indexes.length);

			if(use_webgl && webgl_context) {
				webgl_context.bindBuffer(webgl_context.ELEMENT_ARRAY_BUFFER, webgl_index_buffer);
				flattened_indexes = [].concat.apply([],indexes);

				webgl_context.bufferData(webgl_context.ELEMENT_ARRAY_BUFFER,new Uint16Array(flattened_indexes),webgl_context.STATIC_DRAW);
				webgl_index_buffer.item_count = flattened_indexes.length;
				if(use_wireframe) {
					webgl_context.drawElements(webgl_context.LINES, webgl_index_buffer.item_count, webgl_context.UNSIGNED_SHORT, 0);
				} else {
					webgl_context.drawElements(webgl_context.TRIANGLES, webgl_index_buffer.item_count, webgl_context.UNSIGNED_SHORT, 0);
				}
			}
			if(use_canvas && canvas_context) {
				var mat = mat4.create();
				mat4.multiply(pMatrix,mvMatrix,mat);
//mat=[-0.017107263207435608, 1.6590503052034653e-19, 0.0015674764290452003, 0.0015643446240574121, -0.002709524240344763, -1.0474832115039693e-18, -0.009896657429635525, -0.009876883588731289, 0, 0.017320508137345314, -6.13529011016107e-19, -6.123031810470917e-19, 10.528736114501953, -0.5822814106941223, 1.9998770952224731, 2.195681571960449];

				var cx = canvas_element.width/2;
				var cy = canvas_element.height/2;

				function rgb(c) {
					var color = c.map(function (x) { return Math.floor(Math.abs(x)*255) % 256; });
					return 'rgb(' + ~~color[0] + ',' + ~~color[1] + ',' + ~~color[2] + ')';
				}
				/*function project_vertex(vertex) {
					var v = vertex.concat([1]);
					var projected = new Array(4);
					mat4.multiplyVec4(mat,v,projected);
					//projected[2] = -projected[2];
					var w = projected[3];
					//vec3.scale(projected,1/w);
					return projected;
					//return projected.map(function (x) { return x/w;});
				}*/
				function to_screen(projected) {
					//console.log('(x,y,z) : ' + projected[0] + ',' + projected[1] + ',' + projected[2] + ')');
					//console.log('(x,y) : ' + (1+projected[0])*cx + ',' + (1-projected[1])*cy + ')');
					var w = projected[3];
					return [(1+projected[0]/w)*cx,(1-projected[1]/w)*cy];
					//return [(1+projected[2])*cx,(1-projected[1])*cy];
				}

				// Return the real number d such that the intersection is start*(d-1)+end*d
				function line_plane_intersect_parameter(start,end,normal,dist) {
					// v = ((p0-l0) . n / (l . n))*l + l0
					// v = (((p0.n) - l0.n) / (l . n))*l + l0
					// The equation of a 2D plane in projective space is n.x-w*d=0 = [n,-d].[x,w]
					// [n,-d].([l1*t,w1]+[l0*(1-t),w0]) = 0
					// Adding two vectors in projective space is given by [x0,w0] + [x1,w1] = [x0*w1+x1*w0,w0*w1]
					var n = normal.concat([-dist]);
					var direction = vec4_sub(start,end);
					//var denominator = vec3.dot(direction,normal);
					var denominator = vec4_dot(direction,n);
					if(denominator != 0) {
						//var t = (dist - vec3.dot(start,normal)) / denominator;
						t = vec4_dot(n,start) / denominator;
						//var vector = new Array(3);
						//vec3.scale(direction,t,vector);
						//vec3.add(start,vector,vector);
						var vector = new Array(4);
						for(var i=0;i<4;i++) {
							vector[i] = start[i] * (1-t) + end[i]*(t);
						}
						return {
							't' : t,
							'vector' : vector
						};
					} else {
						return null;
					}
				}
				// Slice a polygon with a plane. The ordered list of vertices on the positive side of the plane will be modified.
				function slice_polygon(vertices,normal,dist) {
					var plane = new Plane({'normal':normal,'dist':dist});
					function dot(v) {
						return vec3.dot(normal,v) - dist*v[3];
					}
					for(var start = 0; 
						start < vertices.length &&
						dot(vertices[start]) < 0;
						start++);

					if(start == vertices.length) {
							vertices.splice(0,vertices.length);
							return;
					}
					start = (start+1) % vertices.length;
					for(var i=0;i<vertices.length;) {
						var i_start = (i+start) % vertices.length;


						/*if(dot(vertices[(i_start-1+vertices.length) % vertices.length]) < 0) {
							console.log(vertices[(i_start-1+vertices.length)%vertices.length]);
						}*/

						for(var culled_count=0;
							(dot(vertices[(i_start + culled_count) % vertices.length]) < 0) && 
							(culled_count < vertices.length);
							culled_count++);
						if(culled_count == vertices.length) {
							vertices.splice(0,vertices.length);
							return;
						}
						if(culled_count > 0) {
							var vs = [
								{ 'index' : -1, 'next' : 1},
								{ 'index' : culled_count, 'next' : -1}
							].map(function (i) {
								var index = (i['index']+i_start+vertices.length)%vertices.length;
								// This is on the positive side of the plane.
								var v0 = vertices[index];
								var v1 = vertices[(index+i['next'] + vertices.length)%vertices.length];
								//var vi = plane.intersect_line(v0,v1);
								var vi = Plane.prototype.intersect_line.apply(plane,[v0,v1]);
								//var vi = line_plane_intersect_parameter(v0,v1,normal,dist);
								if(vi == null) {
									v = v1;
								} else {
									var v = vi['vector'];
								}
								// We do not allow for results that are on the negative side of the plane.
								/*var displacement = dot(v);
								if(displacement < 0) {
								// Interpolate between (v,v0) at 1+displacement*2
									var t=-displacement*2;
									v = vec4_add(vec4_scale(v,1-t),vec4_scale(v0,t));
								}*/
								return v;
							});
							if(vs.every(function (v) { return v != null; })) {
								if(culled_count+i_start<vertices.length) {
									vertices.splice(i_start,culled_count,vs[0],vs[1]);
								} else {
									var l = vertices.length;
									vertices.splice(i_start,l-i_start,vs[0],vs[1]);
									vertices.splice(0,culled_count-l+i_start);
									start = (start-(culled_count-l+i_start)+vertices.length) % vertices.length;
								}
								i+=2;
							} else {
								console.log(vs);
								i++;
							}
						} else {
							i++;
						}
					}
				}
				// Clip the polygon in side [-1,1]^3
				function clip_polygon(vertices) {
					var planes = [];
					var w = 1;
					[-1,1].forEach(function(bound) {
						[2].forEach(function (dimension)  {
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
					})
					return clip_polygon_cascade(vertices,planes);
				}
				function draw_triangle(vertices,color) {
					//var projected = vertices.map(project_vertex);
					var v0 = Array(3);
					var v1 = Array(3);
					var n0 = Array(3);

					// v[n] = p[n] - p[n-1]
					// N[n] = v[n] x v[n+1]
					vec3.subtract(vertices[0], vertices[vertices.length-1],v0);
					vec3.subtract(vertices[1], vertices[0],v1);
					vec3.cross(v0,v1,n0); 
						vec3.scale(n0,-1);
					vec3.subtract(vertices[0], position,v1);
						// We may need to reverse the vertices if they do not follow the right hand rule.
						var c = Array(3);

						// c . p[0] = c . p[-1]
						/*vec3.cross(v0,n0,c);
						vec3.scale(c,-1);
						var dist = vec3.dot(c,vertices[0]);
						if(vec3.dot(c,vertices[1]) - dist < 0) {
							vertices.reverse();
						}*/
// Backface culling.
					/*if(vec3.dot(n0,v1) > 0)*/ {
						var projected = vertices.map(function (v) { 
							var p = v.concat([1]);

							mat4.multiplyVec4(mat,p);
							return p;
						});
						/*if(projected.map(function (v) { 
							return v.slice(0,3).every(function(x) {
								return -v[3] <= x && x <= v[3]; 
							}) ? 1 : 0;
						}).reduce(function (acc,x) { return acc+x; },0) >=1)*/ {
							projected = clip_polygon(projected);
							projected.forEach(function (v) {
								if(!v.slice(0,3).every(function (x) {
									return Math.abs(x) <= 1;
								})) {
									//console.log(v);
								}
							});

					//console.log('(r,g,b) : ' + color[0] + ',' + color[1] + ',' + color[2]);
							if(projected.length<3) {
								//console.log(projected);
							} else {
							canvas_context.fillStyle = rgb(color);
							canvas_context.strokeStyle = rgb(color);
							canvas_context.beginPath();


							for(var j=0;j<projected.length;j++) {
								var pos = to_screen(projected[j]);
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
				}
				function from_indexes(triangle_index) {
					//var vertices = indexes.slice(triangle_index,triangle_index+3).
					var vertices = triangle_index.map(
					function (index) {
						return webgl_vertices[index]; 
					});
					var color = webgl_colors[triangle_index[0]];
					draw_triangle(vertices,color);
				}
				indexes.forEach(from_indexes);
			}
		}

		/*var dimness = 0;
		visible_leaves.forEach(function (leaf) {
			if(leaf.type != -1) {
				webgl_context.uniform4fv(webgl_shader_program.dummyUniform, leaf.color);
				render_leaves([leaf]);
			}
		});*/
		render_leaves(visible_leaves);

		/*bsp.dfs(position, function(leaf) {
			if(leaf.type == -5) {
			render_leaves([leaf]);
			}
		},null,true);*/

		//console.log('draws: ' + log_draw_count + '; traversals ' + log_traverse_count);
	};//}}}

	redraw2 = function() {
	if($('#canvas_option').prop('checked')) {
		use_webgl = false;
		use_canvas = !use_webgl;
		redraw();
	}

	if($('#webgl_option').prop('checked')) {
		use_webgl = true;
		use_canvas = !use_webgl;
		redraw();
		use_canvas = true;
	}
	}
	redraw2();

	$('#canvas_option').change(function() {
		if(canvas_context) {
			canvas_context.clearRect(0,0,canvas.width,canvas.height);
		}
		redraw2();
	});
	$('#webgl_option').change(function() {
		if(webgl_context) {
			webgl_context.clearColor(0.0, 0.0, 0.0, 1.0);
			webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);
		}
		redraw2();
	});
	$('#wireframe_option').change(function() {
		use_wireframe = $('#wireframe_option').prop('checked');
		redraw2();
	});
	$('#map_option').change(function() {
		player = select_map();
		redraw2();
	});
	function test_intersection(viewpoint,elevation) {
		return true;
	}
	/*function test_intersection(viewpoint,elevation) {
		if(use_noclip) {
		  return true;
		}
		var player_radius = 16;
		return !bsp.intersects(viewpoint, function (wall) {
			if(wall.elevations.length == 4) {
			  return false;
			}
			return wall.line.intersects_circle(viewpoint, player_radius);
		});
	}*/

	$(document).keydown(function(e){
		if (e.keyCode == 37) {
// Left.
			if(e.altKey) {
				player.move_left(5, test_intersection);
			} else {
				player.turn_left(3*2*Math.PI/360);
			}
			redraw2();
			e.preventDefault();
		} else if(e.keyCode == 38) {
// Up.
			if(e.altKey) {
				player.move_up(5, function (v,e) { return true;});
			} else {
				player.move_forward(5, test_intersection);
			}
			redraw2();
			e.preventDefault();
		} else if(e.keyCode == 39) {
// Right.
			if(e.altKey) {
				player.move_left(-5, test_intersection);
			} else {
				player.turn_left(-3*2*Math.PI/360);
			}
			redraw2();
			e.preventDefault();
		} else if(e.keyCode == 40) {
// Down.
			if(e.altKey) {
				player.move_up(-5, function (v,e) { return true;});
			}else {
				player.move_forward(-5, test_intersection);
			}
			redraw2();
			e.preventDefault();
		} 
	});
});//}}}
