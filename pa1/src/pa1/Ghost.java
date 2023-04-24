package pa1;

public class Ghost extends GameObject{
	
	public Ghost(int x, int y) {
		this.setX(x);
		this.setY(y);
	}
	
	public void move(int x,int y) {
		x+=1;
		y+=1;
		if(x<this.getX())
			this.setX((this.getX()-1)*(-1));
		else if(x>this.getX())
			this.setX((this.getX()+1)*(-1));
		else
			this.setX((this.getX())*(-1));
		if(y<this.getY())
			this.setY((this.getY()-1)*(-1));
		else if(y>this.getY())
			this.setY((this.getY()+1)*(-1));
		else
			this.setY((this.getY())*(-1));
	}
	
}
