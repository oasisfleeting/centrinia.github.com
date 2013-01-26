precision mediump float;


varying vec3 vLightWeighting;

void main(void) {
    gl_FragColor = vec4(vLightWeighting,1);
}
