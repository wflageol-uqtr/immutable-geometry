# Design Pattern for Reusing Immutable Methods in Object-Oriented Languages

This repository contains code examples of the design pattern described in the paper titled "Design Pattern for Reusing Immutable Methods in Object-Oriented Languages" published in the Proceedings of the 28th European Conference on Pattern Languages of Programs (EuroPlop 2023) and its extension sent to the Journal of Computer Languages.

The examples are in 8 programming languages: Clojure, Common Lisp, C#, Java, Kotlin, OCaml, Rust, and Scala.

There are two languages with multiple examples:

- There is a "naive" version of the Java example which does not use the proposed design pattern (java-nopattern.java).
- There are two versions of the Common Lisp example:
    * common-lisp.lisp uses the proposed design pattern with the base language.
    * common-list-expended.lisp uses the proposed language extension instead.

Each example implements a fully immutable geometry system with the types point, size, and rectangle. They showcase reusing the methods declared for point and size for rectangle without having to reimplement or create delegeting methods for rectangle.

Note that the coordinate system for each example considers the origin (0,0) as the top left corner.

Each example implement the following methods:

- For points, or "movable":
  * Move/Translate: Changes the x/y coordinates of the object by the specified delta.
  * Rotate: Rotates the object around its center 90 degrees clockwise.
  * RotateAround: Rotates the object around a specific origin 90 degrees clockwise.
- For size, or "scalable":
  * Scale: Applies a specified factor to the w and h properties of the object.
  * FitIn: Shrinks or grows the object to fit into the specified size without changing its aspect ratio.
- For rectangle only:
  * Intersect: Returns the intersection with a specified rectangle.
