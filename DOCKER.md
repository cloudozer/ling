
# Building with Docker

To start building your own images that contain your code, you
need to build `railing` -- a builder for openling images.

Prerequisites: [docker](http://www.docker.io)

* build the image that contains all build dependencies for openling:

```sh
% cd deps && docker build -t openling-env .
```

* build the image that contains the openling binary and railing
(this also builds an empty image in `/tmp`):

```sh
% docker build -t openling .
```

