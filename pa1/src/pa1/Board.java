package pa1;
import java.util.Scanner;

public class Board implements Game {
	Player player;
	Ghost ghost;
	Key key;
	Door door;

	public Board() {}
	
	public void printBoard() {
		char[][] board = new char[][] {
			{' ','■',' ',' ',' ',' ','■',' ','■',' ',' ',' ',' ',' ',' '},
			{' ','■',' ','■','■','■','■',' ','■',' ','■',' ','■','■',' '},
			{' ','■',' ',' ',' ','■',' ',' ','■',' ','■',' ',' ','■',' '},
			{' ','■',' ','■',' ','■',' ','■','■',' ','■','■',' ','■',' '},
			{' ',' ',' ','■',' ','■',' ',' ',' ',' ',' ',' ',' ',' ',' '},
			{' ','■','■','■',' ',' ',' ','■','■','■','■','■','■','■',' '},
			{' ',' ',' ',' ',' ','■',' ',' ',' ',' ',' ',' ',' ','■',' '},
			{'■','■',' ','■','■','■',' ','■','■','■','■','■',' ','■',' '},
			{' ',' ',' ',' ',' ',' ',' ','■',' ',' ',' ',' ',' ','■',' '},
			{' ','■','■',' ','■','■',' ','■',' ','■','■','■',' ','■',' '},
			{' ','■',' ',' ',' ',' ',' ','■',' ',' ',' ','■',' ','■',' '},
			{' ','■',' ','■','■','■','■','■','■','■','■','■',' ','■',' '},
			{' ','■',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','■',' '},
			{' ','■','■','■',' ','■','■','■','■','■','■','■',' ','■',' '},
			{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '}
		};
		int i,j;
		for(i=0;i<=14;i++) {
			for(j=0;j<=14;j++) {
				if(player.getX()==i && player.getY()==j)
					System.out.print("P ");
				else if(this.abs(ghost.getX())-1==i && this.abs(ghost.getY())-1==j)
					System.out.print("G ");
				else if(key.getX()==i && key.getY()==j)
					System.out.print("K ");
				else if(door.getX()==i && door.getY()==j)
					System.out.print("D ");
				else
					System.out.print(board[i][j]+" ");
			}
			System.out.println();
		}
	}

	public void initObjects() {
		Scanner scn = new Scanner(System.in);
		char[][] board = new char[][] {
			{' ','■',' ',' ',' ',' ','■',' ','■',' ',' ',' ',' ',' ',' '},
			{' ','■',' ','■','■','■','■',' ','■',' ','■',' ','■','■',' '},
			{' ','■',' ',' ',' ','■',' ',' ','■',' ','■',' ',' ','■',' '},
			{' ','■',' ','■',' ','■',' ','■','■',' ','■','■',' ','■',' '},
			{' ',' ',' ','■',' ','■',' ',' ',' ',' ',' ',' ',' ',' ',' '},
			{' ','■','■','■',' ',' ',' ','■','■','■','■','■','■','■',' '},
			{' ',' ',' ',' ',' ','■',' ',' ',' ',' ',' ',' ',' ','■',' '},
			{'■','■',' ','■','■','■',' ','■','■','■','■','■',' ','■',' '},
			{' ',' ',' ',' ',' ',' ',' ','■',' ',' ',' ',' ',' ','■',' '},
			{' ','■','■',' ','■','■',' ','■',' ','■','■','■',' ','■',' '},
			{' ','■',' ',' ',' ',' ',' ','■',' ',' ',' ','■',' ','■',' '},
			{' ','■',' ','■','■','■','■','■','■','■','■','■',' ','■',' '},
			{' ','■',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','■',' '},
			{' ','■','■','■',' ','■','■','■','■','■','■','■',' ','■',' '},
			{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '}
		};
		while(true) {
			System.out.println("input player x, y (0 ~ 14) :");
			int y = scn.nextInt();
			int x = scn.nextInt();
			if(x>=0 && x<=14 && y>=0 && y<=14 && board[x][y]==' ') {
				player = new Player(x,y);
				break;
			}
		}
		while(true) {
			System.out.println("input ghost x, y (0 ~ 14) :");
			int y = scn.nextInt();
			int x = scn.nextInt();
			if(x>=0 && x<=14 && y>=0 && y<=14 && (player.getX()!=x || player.getY()!=y)) {
				ghost = new Ghost(x+1,y+1);
				break;
			}
		}
		while(true) {
			System.out.println("input key x, y (0 ~ 14) :");
			int y = scn.nextInt();
			int x = scn.nextInt();
			if(x>=0 && x<=14 && y>=0 && y<=14 && (player.getX()!=x || player.getY()!=y) && (ghost.getX()!=x || ghost.getY()!=y) && board[x][y]==' ') {
				key = new Key(x,y);
				break;
			}
		}
		while(true) {
			System.out.println("input door x, y (0 ~ 14) :");
			int y = scn.nextInt();
			int x = scn.nextInt();
			if(x>=0 && x<=14 && y>=0 && y<=14 && (player.getX()!=x || player.getY()!=y) && (ghost.getX()!=x || ghost.getY()!=y) && (key.getX()!=x || key.getY()!=y) && board[x][y]==' ') {
				door = new Door(x,y);
				break;
			}
		}
	}
	
	public void movePlayer() {
		Scanner scn = new Scanner(System.in);
		char direction = scn.next().charAt(0);
		player.move(direction);
	}	
	
	public void moveGhost() {
		if(this.getCheckghost()==1){
			ghost.move(player.getX(),player.getY());
		}
		else
			this.setCheckghost();
	}
	
	public boolean isFinish() {
		if(player.getX()==key.getX() && player.getY()==key.getY()) {
			key.setX(15);
			key.setY(15);
		}
		if(player.getX()==door.getX() && player.getY()==door.getY() && this.getCheckkey()==1) {
			System.out.println("YOU WIN");
			return true;
		}
		else if(player.getX()==abs(ghost.getX())-1 && player.getY()==abs(ghost.getY())-1) {
			System.out.println("YOU LOSE");
			return true;
		}
		return false;
	}
	
	public void setCheckghost() {
		ghost.setX(this.abs(ghost.getX()));
		ghost.setY(this.abs(ghost.getY()));
	}
	
	public int getCheckghost() {
		if(ghost.getX()<0)
			return 0;
		return 1;
	}
	
	public int getCheckkey() {
		if(key.getX()==15 && key.getY()==15)
			return 1;
		return 0;
	}
	
	public int abs(int a) {
		if(a<0)
			a=a*(-1);
		return a;
	}
}
