class MuestraErrores{
	int n;
	int f(int k){
	    int n;
	    n = 1;
	    c = 0;
		while(n<=k){
           if(n%2){
				c = c+1;
           }
           n = n+1;
		}
        return c;
	}
	boolean g(){
		return 2;
	}

	void main(){
		n = f(5);
		f();
        g(3,4);
        n = g();
	}

	boolean n;

	void h(){
		return true;
	}
}