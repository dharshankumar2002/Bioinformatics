  println("IBS4 - Assignment 4.2")                //> IBS4 - Assignment 4.2
  
  val S1="ATGCT"                                  //> S1  : String = ATGCT
  val S2="AGCT"                                   //> S2  : String = AGCT
  
  
  //Converting sequence string into a list
  val seq1 = S1.toArray                           //> seq1  : Array[Char] = Array(A, T, G, C, T)
  val seq2 = S2.toArray                           //> seq2  : Array[Char] = Array(A, G, C, T)
  
  val m = seq1.length                             //> m  : Int = 5
  val n = seq2.length                             //> n  : Int = 4
  
  //Converting the AGCT nucleotides of seq 1 into numbers for easy computation
  def AGCT_to_num(seq1:String) = {
		  var seqs1 = List(0).tail
		  for(i <- seq1){
				var i2 = i.toString
		  	if("A".equals(i2)){
		  		seqs1 = 1 :: seqs1
		  	}
		  	else if(i2=="G"){
		  		seqs1 = 2 :: seqs1
		  	}
		  	else if(i2=="C"){
		  		seqs1 = 3 :: seqs1
		  	}
		  	else if(i2=="T"){
		  		seqs1 = 4 :: seqs1
		  	}
		  }
		  seqs1 = seqs1.reverse
		  seqs1
  }                                               //> AGCT_to_num: (seq1: String)List[Int]
  
  var seqs1 = List(0).tail                        //> seqs1  : List[Int] = List()
  seqs1 = AGCT_to_num(S1)
  seqs1                                           //> res0: List[Int] = List(1, 4, 2, 3, 4)
  
  var seqs2 = List(0).tail                        //> seqs2  : List[Int] = List()
  seqs2 = AGCT_to_num(S2)
  seqs2                                           //> res1: List[Int] = List(1, 2, 3, 4)
  
  //Creating a m x n matrix will full of 0s
  var nw = Array.fill(m,n+3)(0)                   //> nw  : Array[Array[Int]] = Array(Array(0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0
                                                  //| , 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0), Array(0,
                                                  //|  0, 0, 0, 0, 0, 0))
   
  //Creating header row for sequence 1
  var new_row = List(0,0)                         //> new_row  : List[Int] = List(0, 0)
  for(i <- seqs1){
  	new_row = (i :: new_row)
  }
  new_row = new_row.reverse
  new_row                                         //> res2: List[Int] = List(0, 0, 1, 4, 2, 3, 4)
   
  //Creating header coulmn for sequence 2
  var new_col = List(0,0)                         //> new_col  : List[Int] = List(0, 0)
  for(i <- seqs2){
  	new_col = (i :: new_col)
  }
  new_col = new_col.reverse
  new_col                                         //> res3: List[Int] = List(0, 0, 1, 2, 3, 4)
    
   
  //Inserting the created header row in our nw matrix
	var nn = Array(new_row.toArray) ++ nw     //> nn  : Array[Array[Int]] = Array(Array(0, 0, 1, 4, 2, 3, 4), Array(0, 0, 0, 
                                                  //| 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0), Array(
                                                  //| 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0))
  // Traversing elements using loop
  for(i<- 0 to nn.length){
  	//nn(i)(0) =  new_col(i)
        
 }
 
 
 //Dimensions of matrix
    var x = nn.length - 1                         //> x  : Int = 5
    var y = 0                                     //> y  : Int = 0
    if (nn(2).isInstanceOf[Array[Int]]){y = nn(2).asInstanceOf[Array[Int]].size -1 }
    y                                             //> res4: Int = 6
 
 
  //If a value is negative, then changes to 0
  //If a value is positive, then it remains the same
  def if_neg(val1:Int) = {
  	if(val1<0){
  		0
  	}
  	else{
  		val1
  	}
  }                                               //> if_neg: (val1: Int)Int
 
  x=7
  y=7
 
	//Displaying Matrix
	def show(){
		for(i<- 0 to x){               // Traversing elements using loop
		   for(j<- 0 to y){
		        if(nn(i)(j)<0){
		        	print(" "+nn(i)(j))
		        }
		        else{
		        	print("  "+nn(i)(j))
		        }
		    }
		    		   
            	println()

		}
	}                                         //> show: ()Unit
  
  show()                                         

	//Inserting the created header column in our nw matrix
	
	for(i<- 0 to x){               // Traversing elements using loop
           //for(j<- 0 to y){
                nn(i)(0)=new_col(i)
            //}
            println()
        }
   show()
    
    //Initialising the first row of the matrix
    var initx=0
    for(j<- 1 to y){
    	nn(1)(j)=initx
    	initx = initx-2
    	initx = if_neg(initx)
    }
    
    show()
     
    
    //Initialising the first column of the matrix
    var inity=0
    for(i<- 1 to x){
    	nn(i)(1)=inity
    	inity = inity-2
    	inity = if_neg(inity)
    }
    
    show()
    
    //2) Matrix Filling:
     
    def find_left(left:Int) = {
    	var curr = left-2
    	curr = if_neg(curr)
    	curr
    }
    
    def find_up(up:Int) = {
    	var curr = up-2
    	curr = if_neg(curr)
    	curr
    }
    
    def find_diag(diag:Int, nucleotide_up:Int, necleotide_left:Int) = {
    	var curr=0
    	if(nucleotide_up == necleotide_left){
        curr = diag+1
      }
      else {
        curr = diag-1
      }
      curr = if_neg(curr)
    	curr
    }
	  
	  
	  
	  for(i<- 2 to x){
	  	for(j<- 2 to y){
	  			var val_left = find_left(nn(i)(j-1))
	  			var val_up = find_up(nn(i-1)(j))
	  			var val_diag = find_diag(nn(i-1)(j-1),nn(0)(j),nn(i)(0))
	  			var all_three = List(val_left, val_up, val_diag)
	  			nn(i)(j)= all_three.max
	  	}
	  }
	  
	  nn
        
	  show()
	  
	//3) Traceback
	var i = y-1
	var j = x-1
	//var index = Array(List(i,j).toArray)
  var match1 = Array[Int]()
  //var arrow = Array()
  
  
  while((i>0) && (j>0)){
  	println(nn(i)(j))
  	//index = index ++ Array(List(i,j).toArray)
  	if(nn(0)(j) == nn(i)(0)){
  		match1 :+ nn(0)(j)
  		i = i-1
  		j = j-1
  	 }
  	 else{
  	 	if((nn(i)(j-1) > nn(i-1)(j)) && (nn(i)(j-1)>nn(i-1)(j-1))){
  	 		//match1 :+ 0
  	 		j =j-1
  	 	}
  	 	else if((nn(i-1)(j) > nn(i)(j-1)) && (nn(i-1)(j)>nn(i-1)(j-1))){
  	 		//match1 :+ nn(0)(j)
  	 		i =i-1
  	 	}
  	 	else if((nn(i-1)(j-1) > nn(i-1)(j)) && (nn(i-1)(j-1)>nn(i)(j-1))){
  	 		//match1 :+ nn(0)(j)
  	 		i = i-1
  	 		j = j-1
  	 	}
  	 }
  }
  
  
  //index
  match1
  
  
  match1= Array(4,3,2)
  
  var match_new = Array[String]()
  for (i <- match1){
    print(i)
  	if(i.toInt==1){
    	match_new = match_new :+ "A"
  	}
  	else if(i.toInt==2){
  		match_new = match_new :+ "G"
  	}
  	else if(i.toInt==3){
  		match_new = match_new :+ "C"
  	}
  	else if(i.toInt==4){
  		match_new = match_new :+ "T"
  	}
  	else if(i.toInt==0){
  		match_new = match_new :+ "-"
  	}
  }
  match_new = match_new.reverse
  match_new
  var seq1_final = match_new.mkString(" ")
  //Final reseult
  println("Matching Sequence: " + seq1_final)
  
