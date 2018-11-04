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

uniform sampler2D tex_sprite;
uniform int invert_y = 0;

in vec2 uv;

out vec4 color;

void main(void)
{
    vec2 inv_uv = vec2(uv.x, invert_y - ((2 * invert_y) - 1) * uv.y);
    color = texture(tex_sprite, inv_uv);
}
