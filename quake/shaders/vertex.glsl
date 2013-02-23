
attribute vec3 aVertexPosition;
attribute vec3 aVertexNormal;
attribute vec4 aVertexTextureRange;
attribute vec4 aVertexTextureCoord;

uniform mat4 uMVMatrix;
uniform mat3 uNMatrix;
uniform mat4 uPMatrix;

uniform vec2 texture_size;

varying vec4 vTextureCoord;
varying vec4 vTextureRange;
varying vec3 vNormal;
varying vec3 vTransformedNormal;
varying vec4 view_position;
varying vec4 vertex_position;

void main(void) {
	vertex_position = vec4(aVertexPosition,1);

	view_position = uMVMatrix * vertex_position;
	gl_Position = uPMatrix * view_position;

	vTransformedNormal = normalize(uNMatrix * aVertexNormal);
	vNormal = aVertexNormal;
	vTextureRange = aVertexTextureRange;
	vTextureCoord = aVertexTextureCoord;
}
