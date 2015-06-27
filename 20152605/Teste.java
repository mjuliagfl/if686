public class Teste extends Thread {

	public long a;
	public long b;

	public teste(long a, long b){

		this.a = a;
		this.b = b;

	}

	public static void main(String[] args) {
		

		for(int i = 0; i < 100; i++){
		
			long limit = 1000000000 + (i*10000000);
			Thread t = new Teste(limit, limit + 10000000);
			
			t.start();
			
		}



	}

	public void run(){
		
		for(long i = a; i < b; i++ ){
			System.out.println(i);
		}
		

	}

}