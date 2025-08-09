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
        },
        {
            "NAME": "curlScale",
            "LABEL": "Curl scale",
            "TYPE": "float",
            "DEFAULT": -0.6,
            "MIN": -1,
            "MAX": 0
        },
        {
            "NAME": "laplacianScale",
            "LABEL": "Laplacian scale",
            "TYPE": "float",
            "DEFAULT": 0.05,
            "MIN": -1,
            "MAX": 1
        },
        {
            "NAME": "laplacianDivergenceScale",
            "LABEL": "Laplacian divergence scale",
            "TYPE": "float",
            "DEFAULT": -0.8,
            "MIN": -1,
            "MAX": 1
        },
        {
            "NAME": "divergenceScale",
            "LABEL": "Divergence scale",
            "TYPE": "float",
            "DEFAULT": -0.05,
            "MIN": -1,
            "MAX": 1
        },
        {
            "NAME": "divergenceUpdateScale",
            "LABEL": "Divergence update scale",
            "TYPE": "float",
            "DEFAULT": -0.04,
            "MIN": -1,
            "MAX": 1
        },
        {
            "NAME": "divergenceSmoothing",
            "LABEL": "Divergence smoothing",
            "TYPE": "float",
            "DEFAULT": 0.3,
            "MIN": 0,
            "MAX": 1
        },
        {
            "NAME": "advectionDistanceScale",
            "LABEL": "Advection distance scale",
            "TYPE": "float",
            "DEFAULT": 6,
            "MIN": 1,
            "MAX": 10
        },
        {
            "NAME": "curlRotationAnglePower",
            "LABEL": "Curl rotation angle power",
            "TYPE": "float",
            "DEFAULT": 1,
            "MIN": 0,
            "MAX": 10
        },
        {
            "NAME": "selfAmplification",
            "LABEL": "Self-amplification",
            "TYPE": "float",
            "DEFAULT": 1,
            "MIN": 0,
            "MAX": 10
        },
        {
            "NAME": "updateSmoothing",
            "LABEL": "Update smoothing",
            "TYPE": "float",
            "DEFAULT": 0.8,
            "MIN": 0,
            "MAX": 1
        },
        {
            "NAME": "diagonalWeight",
            "LABEL": "Diagonal weight",
            "TYPE": "float",
            "DEFAULT": 0.6,
            "MIN": 0,
            "MAX": 1
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

// Function from LYGIA <https://github.com/patriciogonzalezvivo/lygia>
mat2 rotate2d(const in float r) {
    float c = cos(r);
    float s = sin(r);
    return mat2(c, s, -s, c);
}

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

struct FluidComponents {
    vec3 center;

    vec3 north;
    vec3 east;
    vec3 south;
    vec3 west;

    vec3 northwest;
    vec3 southwest;
    vec3 northeast;
    vec3 southeast;
};

FluidComponents FluidComponents_create(vec2 center, vec2 stepSizes)
{
    float step_x = stepSizes.x;
    float step_y = stepSizes.y;

    FluidComponents components;

    components.center = IMG_NORM_PIXEL(fluid, fract(center)).xyz;

    components.north = IMG_NORM_PIXEL(fluid, fract(center + vec2(      0,  step_y))).xyz;
    components.east =  IMG_NORM_PIXEL(fluid, fract(center + vec2( step_x,       0))).xyz;
    components.south = IMG_NORM_PIXEL(fluid, fract(center + vec2(      0, -step_y))).xyz;
    components.west =  IMG_NORM_PIXEL(fluid, fract(center + vec2(-step_x,       0))).xyz;

    components.northwest = IMG_NORM_PIXEL(fluid, fract(center + vec2(-step_x,  step_y))).xyz;
    components.southwest = IMG_NORM_PIXEL(fluid, fract(center + vec2(-step_x, -step_y))).xyz;
    components.northeast = IMG_NORM_PIXEL(fluid, fract(center + vec2( step_x,  step_y))).xyz;
    components.southeast = IMG_NORM_PIXEL(fluid, fract(center + vec2( step_x, -step_y))).xyz;

    return components;
}

vec3 FluidComponents_laplacian(FluidComponents components, float centerWeight, float edgeWeight, float vertexWeight)
{
    return centerWeight * components.center +
           edgeWeight   * (components.north + components.east + components.west + components.south) +
           vertexWeight * (components.northwest + components.southwest + components.northeast + components.southeast);
}


void main()
{
    vec2 texel = 1. / RENDERSIZE;
    vec2 uv = gl_FragCoord.xy / RENDERSIZE;

    if (PASSINDEX == 0) // ShaderToy Buffer A
    {
        // It’s unclear whether dividing by the advection distance scale is what
        // was intended in the original ShaderToy shader.
        float laplacianCenterWeight = -20. / advectionDistanceScale;
        float laplacianEdgeWeight = 4. / advectionDistanceScale;
        float laplacianVertexWeight = 1. / advectionDistanceScale;

        FluidComponents components = FluidComponents_create(uv, texel);

        // .x and .y are the x and y components, .z is divergence

        // laplacian of all components
        vec3 laplacian = FluidComponents_laplacian(components, laplacianCenterWeight, laplacianEdgeWeight, laplacianVertexWeight);
        float scaledLaplacianDivergence = laplacianDivergenceScale * laplacian.z;

        // calculate curl
        // vectors point clockwise about the center point
        float curl = components.north.x - components.south.x - components.east.y + components.west.y +
                     diagonalWeight * (components.northwest.x + components.northwest.y +
                                       components.northeast.x - components.northeast.y +
                                       components.southwest.y - components.southwest.x -
                                       components.southeast.y - components.southeast.x);

        // calculate divergence
        // vectors point inwards towards the center point
        float divergence = components.south.y - components.north.y - components.east.x + components.west.x +
                           diagonalWeight * (components.northwest.x - components.northwest.y -
                                             components.northeast.x - components.northeast.y +
                                             components.southwest.x + components.southwest.y +
                                             components.southeast.y - components.southeast.x);
        float smoothedDivergence = components.center.z + divergenceUpdateScale * divergence + divergenceSmoothing * laplacian.z;

        vec2 normalizedCenterComponent = components.center.xy == vec2(0) ? components.center.xy : normalize(components.center.xy);

        // reverse advection
        FluidComponents advectionComponents = FluidComponents_create(uv - components.center.xy * advectionDistanceScale * texel, texel);
        vec3 advectionLaplacian = FluidComponents_laplacian(advectionComponents, 0.25, 0.125, 0.0625);

        // temp values for the update rule
        vec2 ab = selfAmplification * advectionLaplacian.xy +
                  laplacianScale * laplacian.xy +
                  scaledLaplacianDivergence * normalizedCenterComponent.xy +
                  divergenceScale * smoothedDivergence * components.center.xy;

        // rotate
        ab = rotate2d(curlScale * sign(curl) * pow(abs(curl), curlRotationAnglePower)) * ab;

        vec3 abd = updateSmoothing * components.center + (1. - updateSmoothing) * vec3(ab, smoothedDivergence);

        if (enableMouse) {
       	    vec2 displacement = gl_FragCoord.xy - mouse * RENDERSIZE;
            float distance = length(displacement);
            if (distance > 0) {
                abd.xy += exp(-0.1 * distance) * normalize(displacement);
            }
        }

        // initialize with noise
        if (FRAMEINDEX < 1 || restart) {
            gl_FragColor.rgb = vec3(noise(16. * uv + 1.1), noise(16. * uv + 2.2), noise(16. * uv + 3.3));
            gl_FragColor.a = 1;
        } else {
            gl_FragColor.rg = clamp(length(abd.xy) > 1. ? normalize(abd.xy) : abd.xy, -1., 1.);
            gl_FragColor.b = clamp(abd.z, -1., 1.);
            gl_FragColor.a = 1;
            gl_FragColor = (1. - inputImageAmount) * gl_FragColor + inputImageAmount * IMG_PIXEL(inputImage, gl_FragCoord.xy);
        }
    }
    else // ShaderToy Image
    {
        vec3 abd = normalize(IMG_NORM_PIXEL(fluid, uv).xyz);

        vec3 color = 0.5 + 0.6 * cross(abd, vec3(0.5, -0.4, 0.5));
        vec3 divergence = vec3(0.1 * abd.z);

        gl_FragColor = vec4(color + divergence, 1);
    }
}
