class ReturnMetodo {
	boolean x;
	void main(){

	}
	boolean xor(boolean a,boolean b){
		if(a){
			if(b){	return false;}
			else{   return true;}
		}
		else{
			if(b){	return true;}
			else{	return false;}	
		}	
		
	}

	int g(int a,boolean p){
		int digitos;
		digitos = 0;
		while(a>0){
			digitos = digitos+1;
			a = a/10;
			if(p){	return digitos;}
			return 1;
		}
		digitos = 3;		
		return digitos;
	}
}