class Par {
	boolean b;
	boolean esPar(int x){
		if(x%2==0){
			return true;
		}
		else{
			return false;
		}
	}

	void main(){
		b = esPar(4);
	}
}