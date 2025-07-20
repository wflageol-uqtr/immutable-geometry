use std::cmp::{max, min};
use std::fmt;
use std::fmt::Formatter;

struct Point {
    x: i32,
    y: i32,
    color: i32,
}

impl Point {
    fn from_coords(x: i32, y: i32) -> Self {
        Point { x: x, y: y, color: 0 }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Point({}, {}, {})", self.x, self.y, self.color)
    }
}

struct Size {
    w: i32,
    h: i32
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Size({}, {})", self.w, self.h)
    }
}

struct Rectangle {
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    color: i32,
}

impl Rectangle {
    fn from_extents(x: i32, y: i32, w: i32, h: i32) -> Self {
        Rectangle { x: x, y: y, w: w, h: h, color: 0 }
    }
}

impl fmt::Display for Rectangle {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Rectangle({}, {}, {}, {}, {})", self.x, self.y, self.w, self.h, self.color)
    }
}

trait Movable<T> {
    fn x(&self) -> i32;
    fn y(&self) -> i32;
    fn center(&self) -> Point;
    fn update_movable(&self, x: i32, y: i32) -> T;

    fn translate(&self, dx: i32, dy: i32) -> T { self.update_movable(dx + self.x(), dy + self.y()) }
    fn rotate(&self) -> T { self.rotate_around(&self.center()) }

    fn rotate_around(&self, origin: &Point) -> T {
        let x = self.x() - origin.x();
        let y = self.y() - origin.y();

        self.update_movable(-y + origin.x(), x + origin.y())
    }
}

trait Scalable<T> {
    fn w(&self) -> i32;
    fn h(&self) -> i32;

    fn update_scalable(&self, w: i32, h: i32) -> T;

    fn scale(&self, value: f32) -> T {
        self.update_scalable((value * self.w() as f32) as i32,
                             (value * self.h() as f32) as i32)
    }

    fn fit_into(&self, target: &Size) -> T {
        let x_scale = target.h as f32 / self.h() as f32;
        let y_scale = target.w as f32 / self.w() as f32;
        let scale_value = if x_scale < y_scale { x_scale } else { y_scale };
        self.scale(scale_value)
    }
}

impl Movable<Point> for Point {
    fn x(&self) -> i32 { self.x }
    fn y(&self) -> i32 { self.y }

    fn center(&self) -> Point { self.update_movable(self.x, self.y) }

    fn update_movable(&self, x: i32, y: i32) -> Point {
        Self { x, y, .. *self }
    }
}

impl Movable<Rectangle> for Rectangle {
    fn x(&self) -> i32 { self.x }
    fn y(&self) -> i32 { self.y }

    fn center(&self) -> Point {
        Point::from_coords(
          self.x + self.w / 2,
          self.y + self.h / 2
        )
    }

    fn update_movable(&self, x: i32, y: i32) -> Rectangle {
        Self { x, y, .. *self}
    }

    fn rotate_around(&self, origin: &Point) -> Rectangle {

        // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
        let top_left = Point::from_coords(self.x, self.y);
        let top_right = Point::from_coords(self.x + self.w, self.y);
        let bot_left = Point::from_coords(self.x, self.y + self.h);

        // Rotate the 3 points, then find the new top, left, right, and bottom.
        let p1 = top_left.rotate_around(origin);
        let p2 = top_right.rotate_around(origin);
        let p3 = bot_left.rotate_around(origin);

        let top = *[p1.y, p2.y, p3.y].iter().min().unwrap();
        let bot = *[p1.y, p2.y, p3.y].iter().max().unwrap();
        let left = *[p1.x, p2.x, p3.x].iter().min().unwrap();
        let right = *[p1.x, p2.x, p3.x].iter().max().unwrap();

        // Build the new rectangle.
        Self {
            x: left,
            y: top,
            w: right - left,
            h: bot - top,
            .. *self
        }
    }
}

impl Scalable<Size> for Size {
    fn w(&self) -> i32 { self.w }

    fn h(&self) -> i32 { self.h }

    fn update_scalable(&self, w: i32, h: i32) -> Size {
        Self { w, h, .. *self }
    }
}

impl Scalable<Rectangle> for Rectangle {
    fn w(&self) -> i32 { self.w }

    fn h(&self) -> i32 { self.h }

    fn update_scalable(&self, w: i32, h: i32) -> Rectangle {
        Self { w, h, .. *self}
    }
}

impl Rectangle {
    fn left(&self) -> i32 { self.x }
    fn right(&self) -> i32 { self.x + self.w }
    fn top(&self) -> i32 { self.y }
    fn bottom(&self) -> i32 { self.y + self.h }

    fn does_intersect(&self, target: &Rectangle) -> bool {
        target.left() < self.right()
        || target.top() < self.bottom()
        || self.left() < target.right()
        || self.top() < target.bottom()
    }

    fn intersect(&self, target: &Rectangle) -> Option<Rectangle> {
        if self.does_intersect(target) {
            let top = max(self.top(), target.top());
            let bot = min(self.bottom(), target.bottom());
            let left = max (self.left(), target.left());
            let right = min(self.right(), target.right());

            Some (Self { x: left, y: top, w: right - left, h: bot - top, .. *self })
        } else {
            None
        }
    }
}

fn main() {
    let p = Point { x: 2, y: 2, color: 1 };
    let p2 = p.translate(1, 2);
    let p3 = p.rotate();
    let p4 = p.rotate_around(&Point::from_coords(0, 0));

    let s = Size { w: 4, h: 6 };
    let s2 = s.scale(3.0);
    let s3 = s.fit_into(&Size { w: 3, h: 3});

    let r = Rectangle { x: 2, y: 2, w: 3, h: 4, color: 2};
    let r2 = r.translate(1, 2);
    let r3 = r.rotate();
    let r4 = r.rotate_around(&Point::from_coords(0, 0));

    let r5 = r.scale(5.0);
    let r6 = r.fit_into(&Size { w: 8, h: 8});

    let r7 = r.intersect(&Rectangle::from_extents(4, 3, 3, 2));

    println!("{}", p);
    println!("{}", p2);
    println!("{}", p3);
    println!("{}", p4);

    println!("{}", s);
    println!("{}", s2);
    println!("{}", s3);

    println!("{}", r);
    println!("{}", r2);
    println!("{}", r3);
    println!("{}", r4);
    println!("{}", r5);
    println!("{}", r6);
    println!("{}", r7.unwrap());
}