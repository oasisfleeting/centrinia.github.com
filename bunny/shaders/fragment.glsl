precision mediump float;


varying vec3 vLightWeighting;

void main(void) {
    gl_FragColor = vec4(vLightWeighting,1);
    //gl_FragColor = vec4(1,1,1,1);
}
