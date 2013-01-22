// fragment.glsl

//precision lowp float;
precision mediump float;

uniform vec4 uDummy;
uniform vec2 texture_size;

uniform bool use_lighting;
uniform bool uUseTexturing;
uniform mat4 uMVMatrix;
uniform mat4 uMVRotTMatrix;
uniform mat4 uPMatrix;
uniform mat3 uNMatrix;
varying vec2 vTextureCoord;
varying vec4 vColor;
varying vec4 vTextureRange;
varying vec3 vTransformedNormal;
varying vec4 view_position;
varying vec4 vertex_position;

uniform sampler2D uSampler;

vec4 lookup_texture(vec2 uv,vec2 begin, vec2 size) {
	//vec2 begin = vTextureRange.xy;
	//vec2 size = vTextureRange.zw;
	// Coordinates for the inside of a patch. Inside [0,1)^2
	vec2 patch_coord = fract(uv)*size+begin;
	vec2 atlas_coord = patch_coord/texture_size;
	//vec2 patch_coord = fract(vTextureCoord/vTextureRange.zw);
	//vec2 atlas_coord = (patch_coord*vTextureRange.zw+vTextureRange.xy)/texture_size;
	vec2 texcoord = vec2(atlas_coord.s,atlas_coord.t);
	vec4 texcolor = texture2D(uSampler, texcoord);
	return texcolor;
}

// Single Image cubemap.
vec2 cubemap(vec3 vector)
{
	vec2 result;
	int t_index=0;
	float t = vector[0];
	for(int i=1;i<3;i++) {
		if(abs(vector[i]) > abs(t)) {
			t_index = i;
			t = vector[i];
		}
	}

	if(t_index == 0) {
		result = vector.yz;
	} else if(t_index == 1) {
		result = vector.xz;
	} else {
		result = vector.xy;
	}

	return result.xy/t;
}

void main(void) {
	vec3 uAmbientColor = vec3(0.2,0.2,0.2);
	vec3 uPointLightingColor = vec3(0.7,0.6,0.3)*10.0;
	//vec3 lantern_color = vec3(0.1,0.2,0.7)*5.0;
	vec3 lantern_color = vec3(0.1,0.2,0.7)*0.0;

	vec4 texcolor;
	vec3 lightWeighting = vec3(1,1,1);
	if(vTextureRange.z > 0.0) 
	{
		if(uUseTexturing) {
			texcolor = lookup_texture(vTextureCoord,
				vTextureRange.xy,vTextureRange.zw
			);
		} else {
			texcolor = vColor;
		}

		if(use_lighting) {
			vec3 lightDirection = normalize(vec3(0,0,0)-view_position.xyz);
			//vec3 lightDirection = normalize(vec3(0,0,1));

			float directionalLightWeighting = pow(max(dot(vTransformedNormal, lightDirection), 0.0),2.0);

			float inverse_square = pow(length(view_position.xyz),-2.0);
			lightWeighting = uAmbientColor + 
				uPointLightingColor * directionalLightWeighting +
				lantern_color * inverse_square;
		}
	} else {
		vec3 viewvec = (uMVMatrix*vec4(vertex_position.xyz,1)).xyz;

		if(uUseTexturing) {
			texcolor = lookup_texture(
				cubemap((uMVRotTMatrix*vec4(viewvec,1)).xyz),
				vec2(vTextureRange.x - vTextureRange.z/2.0, vTextureRange.y),
				vec2(-vTextureRange.z/2.0, -vTextureRange.w));
		} else {
			texcolor = vColor;
		}
		lightWeighting = vec3(1,1,1);
	}
	gl_FragColor = vec4(texcolor.rgb*lightWeighting,texcolor.a);
}
