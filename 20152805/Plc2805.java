import java.util.ArrayList;

public class Plc2805{

	
	public static void main(String[] args) {
		
		long n = 1000;
		int x = 8;
		//ArrayList<Auxiliar> primes = new ArrayList<Auxiliar>();
		
		//int t = 9;
		//boolean r = false;
		long quant = (n/x);
		
		Thread[] threads= new Thread[x];
		//System.out.println("created");
		
		for(int i = 0; i < x; i++){
			threads[i] =  new Auxiliar((quant *i), quant);
		}
		//System.out.println("created auxiliar");
		int time = (int)System.currentTimeMillis(); 
		
		for(int j = 0; j < x; j++){
			
			threads[j].start();
		}
		//System.out.println("started");
		for(int j = 0; j < x; j++){
			try { 
				threads[j].join();

			 } catch(InterruptedException ie) {
				 System.out.println("crash");
			 }
			
		}
		//System.out.println("finished join");
		for(int j = 0; j < x; j++){
			
			ArrayList<Long> vals = ((Auxiliar) threads[j]).getVals();
			
			for(int i = 0; i < vals.size(); i++){
				if(vals.get(i) <= n){
					System.out.println(vals.get(i));
					
				}
				
			}
			
		}
		
		int time2 = (int)System.currentTimeMillis(); 
		System.out.println("tempo: " + (time2- time));
		
		
	}



}

class Auxiliar extends Thread {

	long t;
	long quant;
	ArrayList<Long> vals = new ArrayList<Long>();
	
	public Auxiliar(long i, long q){
		this.t = i;
		this.quant = q;
	}
	
	ArrayList<Long> getVals() {
		return vals;
	}
	

	public void run() {
		
		for(long a = t; a <(t+ quant); a++){
			boolean is = true;
			for(long i = 2; i <= Math.sqrt(a); i++){
				
				if(a%i == 0){
					is = false;
				}
				
				
			}
			if(is && (a !=0) && (a != 1)) {
				vals.add(a);
				
			}
		}

		
	}
	
	
	
	
}