// fragment.glsl

//precision lowp float;
precision mediump float;

uniform vec2 texture_size;

uniform bool use_lighting;
uniform bool uUseTexturing;
uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;
uniform mat3 uNMatrix;
varying vec4 vTextureCoord;
varying vec4 vTextureRange;
varying vec3 vTransformedNormal;
varying vec3 vNormal;
varying vec4 view_position;
varying vec4 vertex_position;

uniform sampler2D uSampler;

vec4 lookup_texture(vec2 uv,vec2 begin, vec2 size) {
	vec2 patch_coord = mod(uv,1.0)+begin;
	vec2 atlas_coord = size*patch_coord/texture_size;
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
	vec3 uPointLightingColor = vec3(0.7,0.6,0.3)*5.0;
	vec3 lantern_color = vec3(0.1,0.2,0.7)*2.0;
	//vec3 lantern_color = vec3(0.1,0.2,0.7)*0.0;

	vec3 lightWeighting = vec3(1,1,1);
	vec4 texcolor = vec4((vNormal+1.0)/2.0,1.0);
	if(vTextureRange.z > 0.0) 
	{
		if(uUseTexturing) {
			texcolor = lookup_texture(vTextureCoord.st,
				vTextureRange.xy,vTextureRange.zw
			);
		}

		if(use_lighting) {
			vec3 lightDirection = normalize(vec3(0,0,0)-view_position.xyz);
			//vec3 lightDirection = normalize(vec3(0,0,1));

			float directionalLightWeighting = max(dot(vTransformedNormal, lightDirection), 0.0);

			float inverse_square = pow(length(view_position.xyz),-2.0);
			lightWeighting = uAmbientColor + 
				uPointLightingColor * directionalLightWeighting +
				lantern_color * inverse_square;
		}
	} else {
		vec3 viewvec = (uMVMatrix*vec4(vertex_position.xyz,1)).xyz;
		vec2 size = vec2(-vTextureRange.z/2.0, -vTextureRange.w);
		vec2 begin = vec2(vTextureRange.x+1.0, vTextureRange.y);
		if(uUseTexturing) {
			texcolor = lookup_texture(
				cubemap(uNMatrix*viewvec),
				begin,size);
		}
	}
	gl_FragColor = vec4(texcolor.rgb*lightWeighting,texcolor.a);
}
