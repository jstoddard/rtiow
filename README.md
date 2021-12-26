# Common Lisp Implementation of Ray Tracing in One Weekend

This is a Common Lisp implementation of the ray tracer built in *[Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html)*, a book by Peter Shirley. The ray tracer has been run under SBCL and Clozure CL. It is expected to work under other Common Lisp implementations, though the output file in main, "~/image.ppm" may need to be modified to avoid the tilde, which is not a part of the ANSI standard and likely unsupported in some implementations.

The ray tracer is set up to render the final scene from the book (this copy was rendered on lower settings):

![Ray Traced Spheres](image-small.png)

## Running

Place the files somewhere asdf can find them and evaluate `(asdf:load-system "rtiow")`. If you have Quicklisp installed, you can have it handle that for you: Make a "rtiow" subdirectory in Quicklisp's local-projects directory, place the files into it, and evaluate `(ql:quickload "rtiow")`.

Once the system is loaded, you can run the ray tracer by evaluating `(rtiow:main)`. If you don't want to wait several hours for execution to complete, consider the comments in the following section.

## Execution Time

The samples-per-pixel and image-width settings are lowered quite a bit from the final example in the book, which used 500 samples-per-pixel and a 1200 pixel wide image. On my computer, with the original settings, it takes several hours to render in SBCL, and even longer in Clozure CL. There is plenty of room for improvement, however, both in terms of efficiency and in making the code more Lispy. You can also get a quicker result by tweaking the constants at the top of rtiow.lisp: Try lowering the samples per pixel (this may produced jagged edges) and reducing the image width.

## Known Issues

One known issue is that in SBCL, the status text (i.e. "Scanlines remaining: 235") is often late in being displayed and not regularly updated. I have not made any effort to troubleshoot this. It may be as simple as calling `finish-output` after the format statement. The text appears as intended in Clozure CL.

## License

Since Peter Shirley dedicated his Ray Tracing in One Weekend book series to the public domain using CC0, I have decided to do the same with my Common Lisp version based on his work. See the file COPYING for details.

Jeremiah Stoddard, December 25, 2021.