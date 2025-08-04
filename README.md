<p align="center">
  <img width="456" alt="Screenshot" src="https://github.com/user-attachments/assets/50fb007e-046c-4f3b-9299-ac2ed7101d3b" />
</p>

This is an [ISF shader](https://isf.video) for a fake fluid dynamical system
that creates viscous-fingering–like flow patterns and suturing along boundaries.
This particular shader is converted from
[this ShaderToy shader](https://www.shadertoy.com/view/XddSRX). The above
screenshot is from [Videosync](https://videosync.showsync.com).

This is a multi-pass shader that is intended to be used with floating-point
buffers. Not all ISF hosts support floating-point buffers.
[Videosync](https://videosync.showsync.com/download) supports floating-point
buffers in
[v2.0.12](https://support.showsync.com/release-notes/videosync/2.0#2012)) and
later, but https://editor.isf.video does not appear to support floating-point
buffers. This shader will produce *very* different output if floating-point
buffers are not used.
