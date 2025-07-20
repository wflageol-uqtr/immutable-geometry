import java.util.Arrays;
import java.util.List;

interface Movable<T> {
    int getX();
    int getY();
    Point center();

    T updateMovable(int x, int y);

    default T move(int dx, int dy) {
        return updateMovable(getX() + dx, getY() + dy);
    }

    default T rotate() {
        return rotateAround(center());
    }

    default T rotateAround(Point origin) {
        int x = getX() - origin.getX();
        int y = getY() - origin.getY();

        return updateMovable(-y + origin.getX(), x + origin.getY());
    }
}

interface Scalable<T> {
    int getW();
    int getH();

    T updateScalable(int w, int h);

    default T scale(float value) {
        return updateScalable((int)(getW() * value), (int)(getH() * value));
    }

    default T fitInto(Size target) {
        float scale = Math.min(target.getH() / (float)getH(), target.getW() / (float)getW());
        return scale(scale);
    }
}

class Point implements Movable<Point> {
    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int getX() {
        return x;
    }

    @Override
    public int getY() {
        return y;
    }

    @Override
    public Point center() { return this; }

    @Override
    public Point updateMovable(int x, int y) {
        return new Point(x, y);
    }

    @Override
    public String toString() {
        return String.format("Point(%d, %d)", getX(), getY());
    }
}

class Size implements Scalable<Size> {
    private final int w;
    private final int h;

    Size(int w, int h) {
        this.w = w;
        this.h = h;
    }

    @Override
    public int getW() {
        return w;
    }

    @Override
    public int getH() {
        return h;
    }

    @Override
    public Size updateScalable(int w, int h) {
        return new Size(w, h);
    }

    @Override
    public String toString() {
        return String.format("Size(%d, %d)", getW(), getH());
    }
}

class Rectangle implements Movable<Rectangle>, Scalable<Rectangle> {
    private final int x;
    private final int y;
    private final int w;
    private final int h;

    Rectangle(int x, int y, int w, int h) {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
    }

    public int left() { return x; }
    public int top() { return y; }
    public int right() { return x + w; }
    public int bottom() { return y + h; }

    @Override
    public int getX() {
        return x;
    }

    @Override
    public int getY() {
        return y;
    }

    @Override
    public Point center() {
        return new Point(getX() + getW() / 2, getY() + getH() / 2);
    }

    @Override
    public int getW() {
        return w;
    }

    @Override
    public int getH() {
        return h;
    }

    @Override
    public Rectangle updateMovable(int x, int y) {
        return new Rectangle(x, y, getW(), getH());
    }

    @Override
    public Rectangle updateScalable(int w, int h) {
        return new Rectangle(getX(), getY(), w, h);
    }

    @Override
    public Rectangle rotateAround(Point origin) {
        // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
        Point topLeft = new Point(getX(), getY());
        Point topRight = new Point(getX() + getW(), getY());
        Point botLeft = new Point(getX(), getY() + getH());

        Point p1 = topLeft.rotateAround(origin);
        Point p2 = topRight.rotateAround(origin);
        Point p3 = botLeft.rotateAround(origin);

        List<Integer> yList = Arrays.asList(p1.getY(), p2.getY(), p3.getY());
        List<Integer> xList = Arrays.asList(p2.getX(), p2.getX(), p3.getX());

        int top = yList.stream().min(Integer::compare).get();
        int bot = yList.stream().max(Integer::compare).get();
        int left = xList.stream().min(Integer::compare).get();
        int right = xList.stream().max(Integer::compare).get();

        return new Rectangle(
                left,
                top,
                right - left,
                bot - top
        );
    }

    private boolean doesIntersect(Rectangle target) {
        return target.left() < right()
                || target.top() < bottom()
                || left() < target.right()
                || top() < target.bottom();
    }

    public Rectangle intersect(Rectangle target) {
        if(doesIntersect(target)) {
            int top = Math.max(top(), target.top());
            int bot = Math.min(bottom(), target.bottom());
            int left = Math.max(left(), target.left());
            int right = Math.min(right(), target.right());

            return new Rectangle(left, top, right - left, bot - top);
        } else {
            return null;
        }
    }

    @Override
    public String toString() {
        return String.format("Rectangle(%d, %d, %d, %d)", getX(), getY(), getW(), getH());
    }
}

public class Geometry {
    public static void main(String[] args) {
        Point p = new Point(2, 2);
        Point p2 = p.move(1, 2);
        Point p3 = p.rotate();
        Point p4 = p.rotateAround(new Point(0, 0));

        Size s = new Size(4, 6);
        Size s2 = s.scale(3);
        Size s3 = s.fitInto(new Size(3, 3));

        Rectangle r = new Rectangle(2, 2, 3, 4);
        Rectangle r2 = r.move(1, 2);
        Rectangle r3 = r.rotate();
        Rectangle r4 = r.rotateAround(new Point(0,0));

        Rectangle r5 = r.scale(5);
        Rectangle r6 = r.fitInto(new Size(8, 8));

        Rectangle r7 = r.intersect(new Rectangle(4, 3, 3, 2));

        System.out.println(p);
        System.out.println(p2);
        System.out.println(p3);
        System.out.println(p4);

        System.out.println(s);
        System.out.println(s2);
        System.out.println(s3);

        System.out.println(r);
        System.out.println(r2);
        System.out.println(r3);
        System.out.println(r4);
        System.out.println(r5);
        System.out.println(r6);
        System.out.println(r7);
    }
}