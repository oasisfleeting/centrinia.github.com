
attribute vec3 aVertexPosition;
attribute vec3 aVertexColor;
attribute vec3 aVertexNormal;
attribute vec4 aVertexTextureRange;
attribute vec2 aVertexTextureCoordinate;

uniform mat4 uMVMatrix;
uniform mat3 uNMatrix;
uniform mat4 uPMatrix;

uniform vec2 texture_size;

varying vec3 vTransformedNormal;

varying vec2 vTextureCoord;
varying vec4 vTextureRange;
varying vec4 view_position;

void main(void) {
	view_position = uMVMatrix * vec4(aVertexPosition,1.0);
	gl_Position = uPMatrix * view_position;

	vTransformedNormal = normalize(uNMatrix * aVertexNormal);
	vTextureRange = aVertexTextureRange;
	vTextureCoord = aVertexTextureCoordinate/aVertexTextureRange.zw;
	//vTextureCoord = cubemap(view_position.xyz / view_position.w);
}
