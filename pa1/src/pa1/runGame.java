package pa1;

public class runGame {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		// Generate chess board
		Board game = new Board();
		game.initObjects();
		
		while(game.isFinish() != true) {
			game.printBoard();
			game.movePlayer();
			game.moveGhost();
		}
	}

}
