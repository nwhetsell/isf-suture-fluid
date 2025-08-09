/*{
    "CATEGORIES": [
        "Filter",
        "Generator"
    ],
    "CREDIT": "cornusammonis <https://www.shadertoy.com/user/cornusammonis>",
    "DESCRIPTION": "Fake fluid dynamical system that creates viscous-fingering–like flow pattern, converted from <https://www.shadertoy.com/view/XddSRX>",
    "INPUTS": [
        {
            "NAME" : "inputImage",
            "TYPE" : "image"
        },
        {
            "NAME": "inputImageAmount",
            "LABEL": "Input image amount",
            "TYPE": "float",
            "DEFAULT": 0,
            "MIN": 0,
            "MAX": 1
        },
        {
            "NAME": "restart",
            "LABEL": "Restart",
            "TYPE": "event"
        },
        {
            "NAME": "enableMouse",
            "LABEL": "Enable mouse",
            "TYPE": "bool",
            "DEFAULT": false
        },
        {
            "NAME": "mouse",
            "TYPE": "point2D",
            "DEFAULT": [0.5, 0.5],
            "MIN": [0, 0],
            "MAX": [1, 1]
        }
    ],
    "ISFVSN": "2",
    "PASSES": [
        {
            "TARGET": "fluid",
            "PERSISTENT": true,
            "FLOAT": true
        },
        {

        }
    ]
}
*/

//
// ShaderToy Buffer A
//

// Simplex noise by Inigo Quilez, from https://www.shadertoy.com/view/Msf3WH

// Copyright © 2013 Inigo Quilez.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the “Software”), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

vec2 hash( vec2 p ) // replace this by something better
{
	p = vec2( dot(p,vec2(127.1,311.7)),
			  dot(p,vec2(269.5,183.3)) );

	return -1.0 + 2.0*fract(sin(p)*43758.5453123);
}

float noise( in vec2 p )
{
    const float K1 = 0.366025404; // (sqrt(3)-1)/2;
    const float K2 = 0.211324865; // (3-sqrt(3))/6;

	vec2 i = floor( p + (p.x+p.y)*K1 );

    vec2 a = p - i + (i.x+i.y)*K2;
    vec2 o = step(a.yx,a.xy);
    vec2 b = a - o + K2;
	vec2 c = a - 1.0 + 2.0*K2;

    vec3 h = max( 0.5-vec3(dot(a,a), dot(b,b), dot(c,c) ), 0.0 );

	vec3 n = h*h*h*h*vec3( dot(a,hash(i+0.0)), dot(b,hash(i+o)), dot(c,hash(i+1.0)));

    return dot( n, vec3(70.0) );

}

// [End of Inigo Quilez’ simplex noise]

vec2 normz(vec2 x)
{
	return x == vec2(0) ? x : normalize(x);
}

// reverse advection
vec3 advect(vec2 ab, vec2 vUv, vec2 step, float sc)
{
    vec2 aUv = vUv - ab * sc * step;

    const float _G0 = 0.25; // center weight
    const float _G1 = 0.125; // edge-neighbors
    const float _G2 = 0.0625; // vertex-neighbors

    // 3x3 neighborhood coordinates
    float step_x = step.x;
    float step_y = step.y;
    vec2 n  = vec2(0, step_y);
    vec2 ne = vec2(step_x, step_y);
    vec2 e  = vec2(step_x, 0);
    vec2 se = vec2(step_x, -step_y);
    vec2 s  = vec2(0, -step_y);
    vec2 sw = vec2(-step_x, -step_y);
    vec2 w  = vec2(-step_x, 0);
    vec2 nw = vec2(-step_x, step_y);

    vec3 uv =    IMG_NORM_PIXEL(fluid, fract(aUv)).xyz;
    vec3 uv_n =  IMG_NORM_PIXEL(fluid, fract(aUv+n)).xyz;
    vec3 uv_e =  IMG_NORM_PIXEL(fluid, fract(aUv+e)).xyz;
    vec3 uv_s =  IMG_NORM_PIXEL(fluid, fract(aUv+s)).xyz;
    vec3 uv_w =  IMG_NORM_PIXEL(fluid, fract(aUv+w)).xyz;
    vec3 uv_nw = IMG_NORM_PIXEL(fluid, fract(aUv+nw)).xyz;
    vec3 uv_sw = IMG_NORM_PIXEL(fluid, fract(aUv+sw)).xyz;
    vec3 uv_ne = IMG_NORM_PIXEL(fluid, fract(aUv+ne)).xyz;
    vec3 uv_se = IMG_NORM_PIXEL(fluid, fract(aUv+se)).xyz;

    return _G0 * uv +
           _G1 * (uv_n + uv_e + uv_w + uv_s) +
           _G2 * (uv_nw + uv_sw + uv_ne + uv_se);
}


#define fragCoord gl_FragCoord
#define fragColor gl_FragColor
#define iFrame FRAMEINDEX
#define iResolution RENDERSIZE

void main()
{
    if (PASSINDEX == 0) // ShaderToy Buffer A
    {
        const float _K0 = -20./6.; // center weight
        const float _K1 = 4./6.;   // edge-neighbors
        const float _K2 = 1./6.;   // vertex-neighbors
        const float cs = -0.6;  // curl scale
        const float ls = 0.05;  // laplacian scale
        const float ps = -0.8;  // laplacian of divergence scale
        const float ds = -0.05; // divergence scale
        const float dp = -0.04; // divergence update scale
        const float pl = 0.3;   // divergence smoothing
        const float ad = 6.;   // advection distance scale
        const float pwr = 1.;  // power when deriving rotation angle from curl
        const float amp = 1.;  // self-amplification
        const float upd = 0.8;  // update smoothing
        const float sq2 = 0.6;  // diagonal weight

        vec2 vUv = fragCoord.xy / iResolution.xy;
        vec2 texel = 1. / iResolution.xy;

        // 3x3 neighborhood coordinates
        float step_x = texel.x;
        float step_y = texel.y;
        vec2 n  = vec2(0, step_y);
        vec2 ne = vec2(step_x, step_y);
        vec2 e  = vec2(step_x, 0);
        vec2 se = vec2(step_x, -step_y);
        vec2 s  = vec2(0, -step_y);
        vec2 sw = vec2(-step_x, -step_y);
        vec2 w  = vec2(-step_x, 0);
        vec2 nw = vec2(-step_x, step_y);

        vec3 uv =    IMG_NORM_PIXEL(fluid, fract(vUv)).xyz;
        vec3 uv_n =  IMG_NORM_PIXEL(fluid, fract(vUv+n)).xyz;
        vec3 uv_e =  IMG_NORM_PIXEL(fluid, fract(vUv+e)).xyz;
        vec3 uv_s =  IMG_NORM_PIXEL(fluid, fract(vUv+s)).xyz;
        vec3 uv_w =  IMG_NORM_PIXEL(fluid, fract(vUv+w)).xyz;
        vec3 uv_nw = IMG_NORM_PIXEL(fluid, fract(vUv+nw)).xyz;
        vec3 uv_sw = IMG_NORM_PIXEL(fluid, fract(vUv+sw)).xyz;
        vec3 uv_ne = IMG_NORM_PIXEL(fluid, fract(vUv+ne)).xyz;
        vec3 uv_se = IMG_NORM_PIXEL(fluid, fract(vUv+se)).xyz;

        // uv.x and uv.y are the x and y components, uv.z is divergence

        // laplacian of all components
        vec3 lapl  = _K0*uv + _K1*(uv_n + uv_e + uv_w + uv_s) + _K2*(uv_nw + uv_sw + uv_ne + uv_se);
        float sp = ps * lapl.z;

        // calculate curl
        // vectors point clockwise about the center point
        float curl = uv_n.x - uv_s.x - uv_e.y + uv_w.y + sq2 * (uv_nw.x + uv_nw.y + uv_ne.x - uv_ne.y + uv_sw.y - uv_sw.x - uv_se.y - uv_se.x);

        // compute angle of rotation from curl
        float sc = cs * sign(curl) * pow(abs(curl), pwr);

        // calculate divergence
        // vectors point inwards towards the center point
        float div  = uv_s.y - uv_n.y - uv_e.x + uv_w.x + sq2 * (uv_nw.x - uv_nw.y - uv_ne.x - uv_ne.y + uv_sw.x + uv_sw.y + uv_se.y - uv_se.x);
        float sd = uv.z + dp * div + pl * lapl.z;

        vec2 norm = normz(uv.xy);

        vec3 ab = advect(vec2(uv.x, uv.y), vUv, texel, ad);

        // temp values for the update rule
        float ta = amp * ab.x + ls * lapl.x + norm.x * sp + uv.x * ds * sd;
        float tb = amp * ab.y + ls * lapl.y + norm.y * sp + uv.y * ds * sd;

        // rotate
        float a = ta * cos(sc) - tb * sin(sc);
        float b = ta * sin(sc) + tb * cos(sc);

        vec3 abd = upd * uv + (1. - upd) * vec3(a,b,sd);

       	    vec2 d = fragCoord.xy - mouse * RENDERSIZE;
        if (enableMouse) {
            float m = exp(-0.1 * length(d));
            abd.xy += m * normz(d);
        }

        // initialize with noise
        if (iFrame < 1 || restart) {
            vec3 rnd = vec3(noise(16. * vUv + 1.1), noise(16. * vUv + 2.2), noise(16. * vUv + 3.3));
            fragColor = vec4(rnd, 1);
        } else {
            abd.z = clamp(abd.z, -1., 1.);
            abd.xy = clamp(length(abd.xy) > 1. ? normz(abd.xy) : abd.xy, -1., 1.);
            fragColor = vec4(abd, 1);
            gl_FragColor = (1. - inputImageAmount) * gl_FragColor + inputImageAmount * IMG_PIXEL(inputImage, fragCoord.xy);
        }
    }
    else // ShaderToy Image
    {
       	vec2 texel = 1. / iResolution.xy;
        vec2 uv = fragCoord.xy / iResolution.xy;
        vec3 c = IMG_NORM_PIXEL(fluid, uv).xyz;
        vec3 norm = normalize(c);

        vec3 div = vec3(0.1) * norm.z;
        vec3 rbcol = 0.5 + 0.6 * cross(norm.xyz, vec3(0.5, -0.4, 0.5));

        fragColor = vec4(rbcol + div, 1);
    }
}
