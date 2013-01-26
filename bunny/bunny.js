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

	webgl_shader_program.vertexNormalAttribute = webgl_context.getAttribLocation(webgl_shader_program, "aVertexNormal");
	webgl_context.enableVertexAttribArray(webgl_shader_program.vertexNormalAttribute);

	webgl_shader_program.pMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uPMatrix");
	webgl_shader_program.mvMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uMVMatrix");
	webgl_shader_program.nMatrixUniform = webgl_context.getUniformLocation(webgl_shader_program, "uNMatrix");
	webgl_shader_program.ambientColorUniform = webgl_context.getUniformLocation(webgl_shader_program, "uAmbientColor");
	webgl_shader_program.directionalColorUniform = webgl_context.getUniformLocation(webgl_shader_program, "uDirectionalColor");
	webgl_shader_program.lightingDirectionUniform = webgl_context.getUniformLocation(webgl_shader_program, "uLightingDirection");
	var webgl_vertex_buffer;
	var webgl_normal_buffer;
	var webgl_index_buffer;

	webgl_context.enable(webgl_context.DEPTH_TEST);

	webgl_context.enable(webgl_context.CULL_FACE);
	webgl_context.cullFace(webgl_context.FRONT);
	webgl_context.frontFace(webgl_context.CW);
	
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

	var center;
	var bunny_model;
	function select_model() {
		var model_filename = $('#model_option').val();

		$.ajax({async: false,
			type: 'GET',
			url: model_filename,
			data: null,
			success: function(d) {
				bunny_model = d;
			},
			dataType: 'json'
		});

// Load the vertices into WebGL.
		webgl_vertex_buffer = initialize_buffer(webgl_context, webgl_context.ARRAY_BUFFER,
			bunny_model['vertices'], Float32Array);

		webgl_normal_buffer = initialize_buffer(webgl_context, webgl_context.ARRAY_BUFFER,
			bunny_model['normals'], Float32Array);

		webgl_index_buffer = initialize_buffer(webgl_context, webgl_context.ELEMENT_ARRAY_BUFFER,
			bunny_model['indices'], Uint16Array);

		center = bunny_model['vertices'].reduce(
		function (acc,x) {
			var y = [];
			for(var i=0;i<acc.length;i++) {
				y[i] = acc[i] + x[i];
			}
			return y;
		});
		for(var i=0;i<center.length;i++) {
			center[i] /= bunny_model['vertices'].length;
		}
	}
	select_model();

	var lastTime = 0;
	var xRot = 0;
	var yRot = 0;
	var xSpeed = 0.00;
	var ySpeed = 0.00;
	var lighting_distance = 5;
	var lighting_longitude = 0;
	var lighting_latitude = 0;
	var lighting_longitude_speed = 31*Math.PI/180;
	var lighting_latitude_speed = 29*Math.PI/180;

	function animate() {
     	var timeNow = new Date().getTime();
	        if (lastTime != 0) {
			var elapsed = timeNow - lastTime;

			xRot += (xSpeed * elapsed) / 1000.0;
			yRot += (ySpeed * elapsed) / 1000.0;
			lighting_longitude += (lighting_longitude_speed * elapsed) / 1000.0;
			lighting_latitude += (lighting_latitude_speed * elapsed) / 1000.0;
			}
		lastTime = timeNow;
	}

	var redraw = function() {
// Clear the canvas.
		webgl_context.clearColor(0.0, 0.0, 0.0, 1.0);
		webgl_context.viewport(0, 0, webgl_context.viewportWidth, webgl_context.viewportHeight);
		webgl_context.clear(webgl_context.COLOR_BUFFER_BIT | webgl_context.DEPTH_BUFFER_BIT);
		var mvMatrix = mat4.create();
		var pMatrix = mat4.create();
		function setMatrixUniforms(p,mv) {
			webgl_context.uniformMatrix4fv(webgl_shader_program.pMatrixUniform, false, p);
			webgl_context.uniformMatrix4fv(webgl_shader_program.mvMatrixUniform, false, mv);

        	var normalMatrix = mat3.create();
        	mat4.toInverseMat3(mvMatrix, normalMatrix);
			mat3.transpose(normalMatrix);
			webgl_context.uniformMatrix3fv(webgl_shader_program.nMatrixUniform, false, normalMatrix);

			webgl_context.uniform3f(
				webgl_shader_program.ambientColorUniform,
				0.07,0.03,0.02);

			lightingDirection = [
				lighting_distance*Math.cos(lighting_latitude)*Math.cos(lighting_longitude),
				lighting_distance*Math.cos(lighting_latitude)*Math.sin(lighting_longitude),
				lighting_distance*Math.sin(lighting_latitude)];

			var adjustedLD=vec3.create();
			vec3.normalize(lightingDirection,adjustedLD);
			vec3.scale(adjustedLD,-1);
			webgl_context.uniform3fv(webgl_shader_program.lightingDirectionUniform,adjustedLD);

			webgl_context.uniform3f(
				webgl_shader_program.directionalColorUniform,
				1.2,1.5,2.7);
		}

		mat4.perspective(60, webgl_context.viewportWidth / webgl_context.viewportHeight, 1, 4.0, pMatrix);
/*mat4.multiply(
		[	0,0,0,0,
			0,1,0,0,
			1,0,0,0,
			0,0,0,1
		],pMatrix,pMatrix);*/


		mat4.identity(mvMatrix);
		mat4.translate(mvMatrix, [-center[0],-center[1],-center[2]-2])
		mat4.rotate(mvMatrix, xRot, [1,0,0]);
		mat4.rotate(mvMatrix, yRot, [0,1,0]);
		mat4.scale(mvMatrix,[7/1,7/1,7/1]);
	
		webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_vertex_buffer);
		webgl_context.vertexAttribPointer(webgl_shader_program.vertexPositionAttribute,
			webgl_vertex_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

		webgl_context.bindBuffer(webgl_context.ARRAY_BUFFER, webgl_normal_buffer);
		webgl_context.vertexAttribPointer(webgl_shader_program.vertexNormalAttribute,
			webgl_normal_buffer.item_size, webgl_context.FLOAT, false, 0, 0);

		setMatrixUniforms(pMatrix,mvMatrix);

		webgl_context.drawElements(webgl_context.TRIANGLES,
				webgl_index_buffer.item_count*webgl_index_buffer.item_size,
				webgl_context.UNSIGNED_SHORT, 0);
	};
	function tick()
	{
		animate();
		redraw();
		window.setTimeout(tick, 1000/60);
	}
	tick();

	$('#model_option').change(function() {
		select_model();
		redraw();
	});
	$(document).keydown(function(e){
		if(e.altKey) {
			switch(e.keyCode) {
				case 37: // Left
					lighting_longitude_speed += 40*Math.PI/180;
					e.preventDefault();
				break;
				case 38: // Up.
					lighting_latitude_speed += 40*Math.PI/180;
					e.preventDefault();
				break;
				case 39: // Right
					lighting_longitude_speed -= 40*Math.PI/180;
					e.preventDefault();
				break;
				case 40: // Down
					lighting_latitude_speed -= 4*Math.PI/180;
					e.preventDefault();
				break;
				default: break;
			}
		} else {
			switch(e.keyCode) {
				case 37: // Left
					ySpeed -= 1.0;
					e.preventDefault();
				break;
				case 38: // Up.
					xSpeed -= 1.0;
					e.preventDefault();
				break;
				case 39: // Right
					ySpeed += 1.0;
					e.preventDefault();
				break;
				case 40: // Down
					xSpeed += 1.0;
					e.preventDefault();
				break;
				default: break;
			}
		}
	});
});

