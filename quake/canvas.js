

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
var use_webgl = false;
var use_noclip = false;
var use_blending = false;

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
		var viewbobAmplitude = 2.0;
		var viewbobFrequency = 1/10;
		this.stepDistance++;
		var viewbob = Math.cos(this.stepDistance*2*Math.PI*viewbobFrequency)*viewbobAmplitude;

		var vector = this.direction_vector.scale(step);
		this.update(this.fov,this.viewpoint.add(vector),this.direction_vector,this.elevation+viewbob, intersection_handler);
	};//}}}
// Move the viewpoint to the left.//{{{
	this.move_left = function (step, intersection_handler)
	{
		var viewbobAmplitude = 2.0;
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

// Perform texture mapping.//{{{
// http://stackoverflow.com/questions/4774172/image-manipulation-and-texture-mapping-using-html5-canvas
function textureMap(ctx, texture, pts) {
    var tris = [[0, 1, 2], [2, 3, 0]]; // Split in two triangles
    for (var t=0; t<2; t++) {
        var pp = tris[t];
        var x0 = pts[pp[0]].x, x1 = pts[pp[1]].x, x2 = pts[pp[2]].x;
        var y0 = pts[pp[0]].y, y1 = pts[pp[1]].y, y2 = pts[pp[2]].y;
        var u0 = pts[pp[0]].u, u1 = pts[pp[1]].u, u2 = pts[pp[2]].u;
        var v0 = pts[pp[0]].v, v1 = pts[pp[1]].v, v2 = pts[pp[2]].v;

        // Set clipping area so that only pixels inside the triangle will
        // be affected by the image drawing operation
        ctx.save(); ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1);
        ctx.lineTo(x2, y2); ctx.closePath(); ctx.clip();

        // Compute matrix transform
        var delta = u0*v1 + v0*u2 + u1*v2 - v1*u2 - v0*u1 - u0*v2;
        var delta_a = x0*v1 + v0*x2 + x1*v2 - v1*x2 - v0*x1 - x0*v2;
        var delta_b = u0*x1 + x0*u2 + u1*x2 - x1*u2 - x0*u1 - u0*x2;
        var delta_c = u0*v1*x2 + v0*x1*u2 + x0*u1*v2 - x0*v1*u2
                      - v0*u1*x2 - u0*x1*v2;
        var delta_d = y0*v1 + v0*y2 + y1*v2 - v1*y2 - v0*y1 - y0*v2;
        var delta_e = u0*y1 + y0*u2 + u1*y2 - y1*u2 - y0*u1 - u0*y2;
        var delta_f = u0*v1*y2 + v0*y1*u2 + y0*u1*v2 - y0*v1*u2
                      - v0*u1*y2 - u0*y1*v2;

        // Draw the transformed image
        ctx.transform(delta_a/delta, delta_d/delta,
                      delta_b/delta, delta_e/delta,
                      delta_c/delta, delta_f/delta);
        ctx.drawImage(texture, 0, 0);
        ctx.restore();
    }
}//}}}

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
	}
}

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

// Globals.//{{{
$(document).ready(function() {
	var canvas_name = 'canvas';
	canvas_element = document.getElementById(canvas_name);

	if(use_webgl) {
// Initialize WebGL.//{{{
		try {
			webgl_context = canvas_element.getContext('experimental-webgl');
			webgl_context.width = canvas_element.width;
			webgl_context.height = canvas_element.height;
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

	var bsp = null;
	var faces = null;
	var leaves = null;
	var visilist = null;
	var level = null;

	function load_texture_list(filename)
	{
		var result;
		$.ajax({async: false,
					type: 'POST',
					url: 'resources/' + filename,
					data: null,
					success: function(d) { result = d; },
					dataType: 'json'});
		return result;
	}
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
				var result = new Array(indexes.length-2);
				var face_color = [Math.random(),Math.random(),Math.random()];
				
				function get_index(index) {
					if(use_individual_vertices) {
						var result = webgl_vertices.length;

						webgl_vertices.push(level['vertices'][index]);
						webgl_colors.push(face_color);
						return result;
					} else {
						return index;
					}
				}
				head_index = get_index(head);
				for(var i=0;i<tail1.length;i++) {
					var tail1_index = get_index(tail1[i]);
					var tail2_index = get_index(tail2[i]);
					result[i] = [head_index, tail1_index, tail2_index];
				}
				return result;
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
		if(use_webgl) {
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

		mat4.identity(mvMatrix);
		mat4.scale(mvMatrix,[1/100,1/100,1/100]);
		mat4.rotate(mvMatrix, -Math.PI/2, [1,0,0]);
		mat4.rotate(mvMatrix, Math.PI/2-player.direction_angle, [0,0,1]);
		mat4.translate(mvMatrix, [-player.viewpoint.coord[0], -player.viewpoint.coord[1], -player.elevation]);
	
// Clear the automap.
// Clear the canvas.
		if(use_webgl) {
			webgl_context.clearColor(0.0, 0.0, 0.0, 1.0);
			webgl_context.viewport(0, 0, webgl_context.viewportWidth, webgl_context.viewportHeight);
			webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);

			function setMatrixUniforms(p,mv) {
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
		} else if(use_canvas){
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
			leaves.forEach(function (leaf) {
				leaf.face_indexes.forEach(function(face_index) {
					faces[face_index].forEach(function(triangle_indexes) {
						triangle_indexes.forEach(function (vertex_index) {
							indexes.push(vertex_index);
						});
					});
				});
			});
		//console.log('indexes length: ' + indexes.length);

			if(use_webgl) {
				webgl_context.bindBuffer(webgl_context.ELEMENT_ARRAY_BUFFER, webgl_index_buffer);
				webgl_context.bufferData(webgl_context.ELEMENT_ARRAY_BUFFER,new Uint16Array(indexes),webgl_context.STATIC_DRAW);
				webgl_index_buffer.item_count = indexes.length;
				webgl_context.drawElements(webgl_context.TRIANGLES, webgl_index_buffer.item_count, webgl_context.UNSIGNED_SHORT, 0);
			} else if(use_canvas) {
				function rgb(c) {
					var color = c.map(function (x) { return Math.floor(x*255) % 256; });
					return 'rgb(' + color[0] + ',' + color[1] + ',' + color[2] + ')';
				}
				//var mat = mat4.multiply(pMatrix, mvMatrix);
				var mat = mat4.create();
				mat4.multiply(pMatrix,mvMatrix,mat);
				var cx = canvas_element.width/2;
				var cy = canvas_element.height/2;
				for(var i=0;i<indexes.length;i+=3) {
					var triangle_vertices = [];

					var v0 = Array(3);
					var v1 = Array(3);
					var c = Array(3);

					vec3.subtract(
						webgl_vertices[indexes[i+0]],
						webgl_vertices[indexes[i+1]],
						v0);
					vec3.subtract(
						webgl_vertices[indexes[i+2]],
						webgl_vertices[indexes[i+0]],
						v1);
					vec3.cross(v0,v1,c);
					vec3.subtract(
						webgl_vertices[indexes[i+0]],
						position,
						v0);
					if(vec3.dot(c,v0)<0) {

						for(var j=0;j<3;j++) {
							var vertex = webgl_vertices[indexes[i+j]].concat([1]);
							//var projected = mat4.multiplyVec4(mat,vertex);
							var projected = new Array(4);
							mat4.multiplyVec4(mat,vertex,projected);
							var f;
							var z;
							z = projected[3];
							projected = projected.map(function (x) { return (x / z); });
							//z = projected[2];
							//projected = projected.map(function (x) { return (x / z); });
	
							//if(Math.abs(projected[0]) < 1 && Math.abs(projected[1]) < 1)
							projected[0] = (projected[0]+1)*cx;
							projected[1] = (-projected[1]+1)*cy;
							triangle_vertices.push([projected[0], projected[1],projected[2]]);
						}
						if(triangle_vertices.map(function (x) {
							return -1<=x[2]&&x[2] <= 1 && 0 <= x[0]&&x[0] <= cx*2&& 0 <= x[1]&&x[1] <= cy*2 ? 1 : 0;
						}).reduce(function (acc,x) { return acc+x; },0) >= 1) {
						canvas_context.fillStyle = rgb(webgl_colors[i]);
						canvas_context.beginPath();
						canvas_context.moveTo(triangle_vertices[0][0], triangle_vertices[0][1]);
						canvas_context.lineTo(triangle_vertices[1][0], triangle_vertices[1][1]);
						canvas_context.lineTo(triangle_vertices[2][0], triangle_vertices[2][1]);
						canvas_context.fill();
						}
					}
				}
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
	redraw();
	$('#map_option').change(function() {
		player = select_map();
		redraw();
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
			redraw();
			e.preventDefault();
		} else if(e.keyCode == 38) {
// Up.
			if(e.altKey) {
				player.move_up(5, function (v,e) { return true;});
			} else {
				player.move_forward(5, test_intersection);
			}
			redraw();
			e.preventDefault();
		} else if(e.keyCode == 39) {
// Right.
			if(e.altKey) {
				player.move_left(-5, test_intersection);
			} else {
				player.turn_left(-3*2*Math.PI/360);
			}
			redraw();
			e.preventDefault();
		} else if(e.keyCode == 40) {
// Down.
			if(e.altKey) {
				player.move_up(-5, function (v,e) { return true;});
			}else {
				player.move_forward(-5, test_intersection);
			}
			redraw();
			e.preventDefault();
		} 
	});
});//}}}
