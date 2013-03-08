/* hypercube.js */

function Matrix(rows,columns)
{
	this.rows = rows;
	this.columns = columns;
	this.data = new Array(rows*columns);
	for(var i=0;i<this.data.length;i++) {
		this.data[i] = 0;
	}
	this.multiply = function (mat2) {
		var result = new Matrix(this.rows,mat2.columns);
		for(var i=0;i<this.rows;i++) {
			for(var k=0;k<this.columns;k++) {
				for(var j=0;j<mat2.columns;j++) {
					result.data[i*mat2.columns+j] +=
						this.data[i*this.columns+k] * mat2.data[k*mat2.columns+j];
				}
			}
		}
		return result;
	}
	this.transpose = function ()
	{
		var result = new Matrix(this.columns,this.rows);
		for(var i=0;i<this.rows;i++) {
			for(var j=0;j<this.columns;j++) {
				result.data[j*result.columns+i] = this.data[i*this.columns+j];
			}
		}
		return result;
	}

	this.qr = function () {
		function householder(x) {
			var x_norm2_squared = 0;
			for(var i=1;i<x.length;i++) {
				x_norm2_squared += x[i]*x[i];
			}
			var x_norm = Math.sqrt(x[0]*x[0]+x_norm2_squared);
			var alpha = x[0] + (x[0] > 0 ? x_norm : -x_norm);

			var u_norm_squared = alpha*alpha+x_norm2_squared;

			return {'alpha': alpha, 'norm squared' : u_norm_squared};
		}

		var q = new Matrix(this.rows,this.rows);
		var r = new Matrix(this.rows,this.columns);
		for(var i=0;i<this.rows*this.columns;i++) {
			r.data[i] = this.data[i];
		}
		for(var i=0;i<this.rows;i++) {
			q.data[i*(this.rows+1)] = 1;
		}
		for(var i=0;i<r.columns;i++) {
			var x = new Array(r.rows-i);
			for(var j=i;j<r.rows;j++) {
				x[j-i] = r.data[j*r.columns+i];
			}
			// v = [alpha,x(2:)] / sqrt(u_norm_squared)
			var reflector = householder(x);
			// Q_{k+1} = Q_k*(I-2*v*v')'; compute 2*Q_k*v/||v|| first
			var qv = new Array(q.rows);
			for(var j=0;j<qv.length;j++) {
				qv[j] = q.data[(j)*q.columns+(i)] * reflector['alpha'];
				for(var k=1;k<x.length;k++) {
					qv[j] += q.data[(j)*q.columns+(i+k)] * x[k];
				}
				qv[j] *= 2 / reflector['norm squared'];
			}

			// Update Q_{k+1} = Q_k - (2*Q_k*v)*v'
			for(var j=0;j<qv.length;j++) {
				q.data[(j)*q.columns+(i)] -= qv[j]*reflector['alpha'];
				for(var k=1;k<x.length;k++) {
					q.data[(j)*q.columns+(i+k)] -= qv[j]*x[k];
				}
			}

			// R_{k+1} = (I-2*v*v')*R_k; compute 2*v'*R_k/||v|| first.
			var rv = new Array(r.columns-i);
			for(var j=0;j<rv.length;j++) {
				rv[j] = r.data[(i)*r.columns+(i+j)] * reflector['alpha'];
				for(var k=1;k<x.length;k++) {
					rv[j] += r.data[(i+k)*r.columns+(i+j)] * x[k];
				}
				rv[j] *= 2 / reflector['norm squared'];
			}
			// Update R_{k+1} = R_k - v*(2*v'*R_k)
			for(var j=0;j<rv.length;j++) {
				r.data[(i)*r.columns+(i+j)] -= reflector['alpha']*rv[j];
				for(var k=1;k<x.length;k++) {
					r.data[(i+k)*r.columns+(i+j)] -= x[k]*rv[j];
				}
			}
		}
		return {'Q' : q, 'R' : r};
	}
	this.slice = function (r,c,h,w) {
		var result = new Matrix(h,w);
		for(var i=0;i<h;i++) {
			for(var j=0;j<w;j++) {
				result.data[i*w+j] = this.data[(i+r)*this.columns+(j+c)];
			}
		}
		return result;
	}
}
Matrix.identity = function (size) {
	var result = new Matrix(size,size);
	for(var i=0;i<size;i++) {
		result.data[i*(size+1)] = 1;
	}
	return result;
}
Matrix.rotation = function (spanner, angle) {
	var q = spanner.qr()['Q'];
	var rotator = new Matrix(spanner.rows,spanner.rows);
	var cs = Math.cos(angle);
	var sn = Math.sin(angle);
	var givens = [cs,-sn,sn,cs];
	for(var i=0;i<2;i++) {
		for(var j=0;j<spanner.rows;j++) {
			for(var k=0;k<2;k++) {
				rotator.data[j*spanner.rows+i] += q.data[j*spanner.rows+k]*givens[k*2+i];
			}
		}
	}
	for(var i=2;i<spanner.rows;i++) {
		for(var j=0;j<spanner.rows;j++) {
			rotator.data[j*spanner.rows+i] = q.data[j*spanner.rows+i];
		}
	}
	return rotator.multiply(q.transpose());
}

Math.mod = function (x,y) {
	return x-y*Math.floor(x/y);
}

$(document).ready(function() {
	var canvas_name = 'canvas';
	var canvas = document.getElementById(canvas_name);


	var webgl_context = canvas.getContext('experimental-webgl');
	if(!webgl_context) {
		alert('Unable to use WebGL.');
		return;
	}
	webgl_context.width = canvas.width;
	webgl_context.height = canvas.height;

	webgl_context.viewportWidth = canvas.width;
	webgl_context.viewportHeight = canvas.height;
	function get_shader(context,type,filename,predefines) {
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

		context.shaderSource(shader, predefines + str);
		context.compileShader(shader);

		if (!context.getShaderParameter(shader, context.COMPILE_STATUS)) {
			alert(context.getShaderInfoLog(shader));
			return null;
		}

		return shader;
	}
	function initialize_shaders(context,predefines) {
		var vertex_shader;
		var vertex_shader = get_shader(context, 'x-shader/x-vertex', 'vertex.glsl',predefines);
		var fragment_shader = get_shader(context, 'x-shader/x-fragment', 'fragment.glsl',predefines);

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

	webgl_context.enable(webgl_context.DEPTH_TEST);
	var webgl_vertex_buffer;
	var webgl_index_buffer;
	var ndmv;
	var webgl_shader_program;
	var spanners;
	var angles;

	function initialize_buffer(context, type, data, datatype) {
		buffer = context.createBuffer();
		context.bindBuffer(type, buffer);
		if(data[0].length) {
			var flattened_data = data.reduce(function (acc, v) { return acc.concat(v); });
			buffer.item_size = data[0].length;
		} else {
			buffer.item_size = 1;
			flattened_data = data;
		}
		context.bufferData(type,new datatype(flattened_data),context.STATIC_DRAW);
		buffer.item_count = data.length;
		return buffer;
	}
	var angle_speeds;
	function set_dimension(dimension) {
		webgl_shader_program = initialize_shaders(webgl_context, '#define dimension ' + dimension + '\n');

		webgl_shader_program.vertexPositionAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexIndex");
		webgl_context.enableVertexAttribArray(webgl_shader_program.vertexPositionAttribute);

		webgl_shader_program.vertexColorAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexColor");
		webgl_context.enableVertexAttribArray(webgl_shader_program.vertexColorAttribute);

		webgl_shader_program.pMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uPMatrix");
		webgl_shader_program.mvMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uMVMatrix");
		webgl_shader_program.ndMVMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "ndMVMatrix");


		select_model(dimension);

		spanners = new Array(dimension);
		angles = new Array(spanners.length);
		angle_speeds = new Array(spanners.length);
		var speed_scale = 1/50000;
		for(var i=0;i<spanners.length;i++) {
			spanners[i] = new Matrix(dimension,2);
			for(var j=0;j<dimension*2;j++) {
				spanners[i].data[j] = Math.random()*2-1;
			}
			angles[i] = (Math.random()*2-1)*Math.PI*2;
			angle_speeds[i] = (Math.random()*2-1)*Math.PI*2*speed_scale;
		}	
		ndmv = Matrix.identity(dimension);
	} 

	function select_model(dimensions) {
		var vertices = new Array(1<<dimensions);
		var colors = new Array(1<<dimensions);
		var wireframe_indices = new Array(); //new Array(dimensions*(dimensions-1) << (dimension-2));
		for(var i=0;i<(1<<dimensions);i++) {
			vertices[i] = new Array(4);
			colors[i] = new Array(4);

			for(var j=0;j<4;j++) {
				vertices[i][j] = 0;
				colors[i][j] = Math.random();
			}
			vertices[i][0] = i;
			colors[i][3] = 1;
		}

		var facet_count = 1 << (dimensions-2);
		for(var i=0;i<dimensions-1;i++) {
			for(var j=i+1;j<dimensions;j++) {
				for(var k=0;k<facet_count;k++) {
					var index_0 = k & ((1 << i) - 1);
					var index_1 = (k & ((1 << (j-1)) - 1)) ^ index_0;
					var index_2 = k ^ (index_0 | index_1);
					index_1 <<= 1;
					index_2 <<= 2;
					var facet_index = index_0 | index_1 | index_2;

					var append_vertex = function(l) {
						var gray_code = (l >> 1) ^ l;
						var vertex_index = facet_index | ((gray_code & 2) << (j-1)) | ((gray_code&1) << i);
						wireframe_indices.push(vertex_index);
					}
					for(var l=0;l<4;l++) {
						append_vertex(l);
						append_vertex((l+1)&3);
					}
				}
			}
		}

		webgl_vertex_buffer = initialize_buffer(webgl_context, webgl_context.ARRAY_BUFFER,
			vertices, Float32Array);
		webgl_color_buffer = initialize_buffer(webgl_context, webgl_context.ARRAY_BUFFER,
			colors, Float32Array);
		webgl_index_buffer = initialize_buffer(webgl_context, webgl_context.ELEMENT_ARRAY_BUFFER,
			wireframe_indices, Uint16Array);
	}
	var redraw = function() {
// Clear the canvas.
		webgl_context.clearColor(0.0, 0.0, 0.0, 1.0);
		webgl_context.viewport(0, 0, webgl_context.viewportWidth, webgl_context.viewportHeight);
		webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);
		var mvMatrix = mat4.create();
		var pMatrix = mat4.create();
		function setMatrixUniforms(p,mv,ndmv) {
			webgl_context.uniformMatrix4fv(webgl_shader_program.pMatrixUniform, false, p);
			webgl_context.uniformMatrix4fv(webgl_shader_program.mvMatrixUniform, false, mv);
			webgl_context.uniform3fv(webgl_shader_program.ndMVMatrixUniform, ndmv);
		}

		//mat4.perspective(60, webgl_context.viewportWidth / webgl_context.viewportHeight, 0.1, 1000.0, pMatrix);
		//pMatrix = mat4.perspective(60, webgl_context.viewportWidth / webgl_context.viewportHeight, 0.1, 1000.0);
		pMatrix = mat4.ortho(-2,2,-2,2,0.1,20);

		mat4.identity(mvMatrix);
		mat4.translate(mvMatrix, [0,0,-5]);
		mat4.scale(mvMatrix, [1,1,1]);
		//mat4.translate(mvMatrix, [-1,0,-2]);
	
		webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_vertex_buffer);
		webgl_context.vertexAttribPointer(webgl_shader_program.vertexPositionAttribute,
			webgl_vertex_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

		webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_color_buffer);
		webgl_context.vertexAttribPointer(webgl_shader_program.vertexColorAttribute,
			webgl_color_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

		webgl_context.bindBuffer(webgl_context.ELEMENT_ARRAY_BUFFER, webgl_index_buffer);

		setMatrixUniforms(pMatrix,mvMatrix,ndmv.slice(0,0,3,ndmv.columns).data);

		//webgl_context.drawArrays(webgl_context.LINES, 0, webgl_vertex_buffer.item_count);
		webgl_context.drawElements(webgl_context.LINES,
				webgl_index_buffer.item_count*webgl_index_buffer.item_size,
				webgl_context.UNSIGNED_SHORT, 0);
	};

	var last_time = 0;
	function animate() {
		var time_now = new Date().getTime();
		if (last_time != 0) {
			var elapsed = time_now - last_time;
			for(var i=0;i<angles.length;i++) {
				angles[i] = Math.mod(angles[i]+angle_speeds[i]*elapsed,Math.PI*2);
			}

			ndmv = Matrix.identity(dimension);
			for(var i=0;i<spanners.length;i++) {
				ndmv = Matrix.rotation(spanners[i],angles[i]).multiply(ndmv);
			}
		}
		last_time = time_now;
	}
	var dimension = 3;
	set_dimension(dimension);
	function tick() {
		animate();
		redraw();
		window.setTimeout(tick, 1000/60);
	}
	tick();

	$('#dimension_option').change(function() {
		dimension = parseInt($('#dimension_option').val());
		set_dimension(dimension);
	});
});

