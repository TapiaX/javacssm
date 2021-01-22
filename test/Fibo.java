class B{
   int a;
   int b;
   void main(){
      a = f(4);
   }

   int f(int n){
      int fib;
      fib = 1;
      if(n>1){
          fib = f(n-1) + f(n-2);
      }
      return fib;
   }
}