class ContadorPares{
	int n;
	int f(int k){
	    int n;
	    int c; 
	    n = 1;
	    c = 0;
		while(n<=k){
           if(n%2==0){
           		c = c+1;
           }
           n = n+1;
		}
        return c;
	}

	void main(){
		n = f(4);
	}
}