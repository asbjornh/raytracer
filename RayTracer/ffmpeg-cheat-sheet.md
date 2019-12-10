```
ffmpeg -pattern_type sequence -framerate 20 -i "%03d.ppm" -vcodec h264 -pix_fmt yuv420p anim.mov
```
