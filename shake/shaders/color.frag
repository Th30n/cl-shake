// Copyright (C) 2016 Teon Banek
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#version 330 core

uniform sampler2DArray tex_albedo;
uniform int has_albedo = 0;

uniform vec3 light_dir = vec3(1.0f, 1.0f, -1.0f);
uniform vec3 ambient = vec3(0.6f, 0.6f, 0.6f);

in VS_OUT
{
    vec2 uv;
    vec3 color;
    vec3 normal;
} fs_in;


out vec4 frag_color;

void main(void)
{
    // frag_color = vec4(1, 0, 0, 1);
    float ndotl = max(dot(normalize(fs_in.normal), normalize(light_dir)), 0.0f);
    vec3 albedo = fs_in.color;
    if (has_albedo >= 0) {
        float layer = float(has_albedo);
        albedo = albedo * texture(tex_albedo, vec3(fs_in.uv, layer)).rgb;
    }
    frag_color = vec4(albedo * ambient + albedo * ndotl, 1.0f);
}
