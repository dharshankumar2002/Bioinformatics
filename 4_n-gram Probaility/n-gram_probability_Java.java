import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Scanner;
import java.util.Set;

public class ngram_probabilites {
	public static void main(String[] args) throws FileNotFoundException {
			
			// File reading and storing the data into a List
		    List<List<String>> sequences = new ArrayList<>();
			File myObj = new File("MN908947.txt");
		      Scanner myReader = new Scanner(myObj);
		      while (myReader.hasNextLine()) {
		        String data = myReader.nextLine();
		        sequences.add(Arrays.asList(data));
		      }
		      myReader.close();
		   
		      int k=5;
		      List<List<String>> kmers=kmers_tokenization(k, sequences);  // of the form [["ADAS","ASDD"],["DSFSDF","ADA"]]
		      
		        // unigram
		        List<List<String>> unigram=new ArrayList<>();  
		        for(List<String> line: kmers) {
		        	unigram.add(n_grams(line, 1));
		        }
		        List<String> unigrams = unigram.stream().collect(ArrayList::new, List::addAll, List::addAll); 
		        System.out.println("unigrams size: "+unigrams.size());
		        
		        // bigrams
		        List<List<String>> bigram=new ArrayList<>();   
		        for(List<String> line: kmers) {
		        	bigram.add(n_grams(line, 2));
		        }
		        List<String> bigrams = bigram.stream().collect(ArrayList::new, List::addAll, List::addAll);
		        System.out.println("bigrams size: "+bigrams.size());
		        
		        // trigrams
		        List<List<String>> trigram=new ArrayList<>();  
		        for(List<String> line: kmers) {
		        	trigram.add(n_grams(line, 3));
		        }
		        List<String> trigrams = trigram.stream().collect(ArrayList::new, List::addAll, List::addAll);
		        System.out.println("trigrams size: "+trigrams.size());
		        
		        // unigram Probability
		        Dictionary unigrams_count=countFrequencies(unigrams);
		        Enumeration enu = unigrams_count.elements(); 
		        double unigrams_count_total=sum(enu);
		        System.out.println("Probability for [AATGT] :");
		        System.out.println((int)unigrams_count.get("[AATGT]")/unigrams_count_total);
		        
		        
		        // bigram Probability
		        Dictionary bigrams_count=countFrequencies(bigrams);
		        Enumeration enu_2 = bigrams_count.elements(); 
		        double bigrams_count_total=sum(enu_2);
		        System.out.println("Probability for [TGGTA, AAATT] :");
		        System.out.println((int)bigrams_count.get("[TGGTA, AAATT]")/bigrams_count_total);
		        
		        
		        // trigram Probability
		        Dictionary trigrams_count=countFrequencies(trigrams);
		        Enumeration enu_3 = trigrams_count.elements(); 
		        double trigrams_count_total=sum(enu_3);
		        System.out.println("Probability for [AATCT, GTGTG, GCTGT] :");
		        System.out.println((int)trigrams_count.get("[AATCT, GTGTG, GCTGT]")/trigrams_count_total);  
	}

	
     // function to generate kmers
	 public static List<String> generate_kmers(int k, String seq) 
	 {	    
	        int tot = seq.length();
	        int l=1;
	        List<String> kmers=new ArrayList<String>();
	        while(l<tot-k) {
	        	kmers.add(seq.substring(l, l+k));
	        	l = l+k;
	        }
	        return kmers;
	      }

	 // function to tokenize kmers
 	 public static List<List<String>> kmers_tokenization(int k,List<List<String>> sequences){
		List<List<String>> mers_tokenized = new ArrayList<List<String>>();
		for(List<String> line: sequences) {
			List<String> kmers=generate_kmers(k,line.toString());
			mers_tokenized.add(kmers);
		}
		return mers_tokenized;
	}
 
	// function to generate n-grams
	public static List<String> n_grams(List<String> seq,int n) {
		List<String> gram=new ArrayList<String>();
		for(int i=0;i<=seq.size()-n;i++) {
			if(seq.subList(i, i+n).size()==n) {
				gram.add(seq.subList(i, i+n).toString());
			}	 
	}
		return gram;
	}
	
	// function to count the frequency of a given n-gram
	public static Dictionary countFrequencies(List<String> list) 
	{ 
	    Set<String> st = new HashSet<String>(list); 
	    Dictionary geek = new Hashtable();
	    for (String s : st) 
	        geek.put(s,Collections.frequency(list, s));
	    
	    return geek;	 
	} 
	
	// function to calculate the sum of the values in a dictionary
	public static int sum(Enumeration enu) 
	{	
		 ArrayList<Integer> x=new ArrayList<Integer>();
		 while (enu.hasMoreElements()) { 
			    x.add((int)enu.nextElement()); 
	        } 
		 int sum1=0;
		 
		 for(Integer a : x) {
			 sum1+=a;
		 }
		 
		 return sum1;		 
		 }
}
