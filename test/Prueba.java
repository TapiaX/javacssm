class Prueba{
    int a;
    
    boolean variable;
    
    void g(int x,int y){
        int res;
        res = 0;
        if(x>y){
            int z;
            z = x;
            x = y;
            y = z;
        }
        while(x>0)
        {   if(x%2==1)
            { res = res + y;
            }
            x = x/2;
            y = y*2;            
            
        }
    }

    void main(){
        a = 16;
        b = 5;
        g(a,b);
    }

    int b;       
}
