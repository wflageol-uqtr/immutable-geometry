import java.util.Arrays;
import java.util.List;

interface Movable {
    int getX();
    int getY();
    Point center();
    
    Movable move(int dx, int dy);
    Movable rotate();
    Movable rotateAround(Point origin);
}

interface Scalable {
    int getW();
    int getH();
    
    Scalable scale(float value);
    Scalable fitInto(Size target);
}

class Point implements Movable {
  private final int x;
  private final int y;
  private final String color;
  
  public Point(int x, int y) {
    this(x, y, "black");
  }
  
  public Point(int x, int y, String color) {
    this.x = x;
    this.y = y;
    this.color = color;
  }
  
  @Override
  public int getX() {
    return x;
  }
  
  @Override
  public int getY() {
    return y;
  }
  
  public String getColor() {
    return color;
  }
  
  @Override
  public Point center() { return this; }
  
  @Override
  public Movable move(int dx, int dy) {
    return new Point(getX() + dx, getY() + dy, getColor());
  }
  
  @Override
  public Movable rotate() {
    return rotateAround(center());
  }
  
  @Override
  public Movable rotateAround(Point origin) {
    int x = getX() - origin.getX();
    int y = getY() - origin.getY();
    
    return new Point(-y + origin.getX(), x + origin.getY(), getColor());
  }
  
  @Override
  public String toString() {
      return String.format("Point(%d, %d, %s)", getX(), getY(), getColor());
  }
}

class Size implements Scalable {
    private final int w;
    private final int h;
    
    public Size(int w, int h) {
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
    public Scalable scale(float value) {
        return new Size((int)(getW() * value), (int)(getH() * value));
    }
    
    @Override
    public Scalable fitInto(Size target) {
        float scale = Math.min(target.getH() / (float)getH(), target.getW() / (float)getW());
        return scale(scale);
    }
    
    @Override
    public String toString() {
        return String.format("Size(%d, %d)", getW(), getH());
    }
}

class Rectangle implements Movable, Scalable {
    private final int x;
    private final int y;
    private final int w;
    private final int h;
    private final String color;
    
    Rectangle(int x, int y, int w, int h) {
        this(x, y, w, h, "black");
    }
    
    Rectangle(int x, int y, int w, int h, String color) {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.color = color;
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
    
    public String getColor() {
        return color;
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
    public Movable move(int dx, int dy) {
        return new Rectangle(
            getX() + dx, 
            getY() + dy,
            w, h, getColor());
    }
    
    @Override
    public Movable rotate() {
        return rotateAround(center());
    }
    
    @Override
    public Scalable scale(float value) {
        return new Rectangle(
            x, y,
            (int)(getW() * value),
            (int)(getH() * value),
            getColor());
    }
    
    @Override
    public Scalable fitInto(Size target) {
        float scale = Math.min(target.getH() / (float)getH(), target.getW() / (float)getW());
        return scale(scale);
    }
    
    @Override
    public Movable rotateAround(Point origin) {
        // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
        Point topLeft = new Point(getX(), getY());
        Point topRight = new Point(getX() + getW(), getY());
        Point botLeft = new Point(getX(), getY() + getH());
    
        Point p1 = (Point)topLeft.rotateAround(origin);
        Point p2 = (Point)topRight.rotateAround(origin);
        Point p3 = (Point)botLeft.rotateAround(origin);
    
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
                bot - top,
                getColor()
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
    
            return new Rectangle(left, top, right - left, bot - top, getColor());
        } else {
            return null;
        }
    }
    
    @Override
    public String toString() {
        return String.format("Rectangle(%d, %d, %d, %d, %s)", getX(), getY(), getW(), getH(), getColor());
    }
}

public class Geometry {
    public static void main(String[] args) {
        Point p = new Point(2, 2, "yellow");
        Movable p2 = p.move(1, 2);
        Movable p3 = p.rotate();
        Movable p4 = p.rotateAround(new Point(0, 0));

        Size s = new Size(4, 6);
        Scalable s2 = s.scale(3);
        Scalable s3 = s.fitInto(new Size(3, 3));

        Rectangle r = new Rectangle(2, 2, 3, 4, "green");
        Movable r2 = r.move(1, 2);
        Movable r3 = r.rotate();
        Movable r4 = r.rotateAround(new Point(0,0));

        Scalable r5 = r.scale(5);
        Scalable r6 = r.fitInto(new Size(8, 8));

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