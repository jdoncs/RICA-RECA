import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.TreeMap;

public class RICA {
	TreeMap<Pair<Integer, Integer>, EQclass> EQclass_list;
	TreeMap<Integer, Integer> Allocations;
	
	public RICA(File preferencefile) throws IOException {
		EQclass_list = new TreeMap<Pair<Integer, Integer>, EQclass>();
		Scanner scanner = new Scanner(preferencefile);
		String bufferline = scanner.nextLine();
		String[] linearray = bufferline.split(" ");
		int kvalue = Integer.parseInt(linearray[6]);

		int Agent = 0;
		int level = 1;
	
		
		//for(int i = 0; i < linearray2.length; i++) {
			//System.out.println(linearray2[i]);
		//}
		while (scanner.hasNextLine()) {
			String bufferline2 = scanner.nextLine();
			String[] linearray2 = bufferline2.split(", ");
			for(int i = 0; i < linearray2.length; i++) {
				String stripped = linearray2[i].trim();
				int resource = Integer.parseInt(stripped);
				//System.out.println("Agent: " + Agent + "level: " + level + "resource: " + resource);
				
				Pair eqpair = new Pair(level, resource);
				if(EQclass_list.containsKey(eqpair)) {
					EQclass_list.get(eqpair).addAgent(Agent);
				}
				else {
					EQclass_list.put(eqpair, new EQclass(level, resource, Agent));
				}
				level++;
			}
			Agent++;
			level = 1;
			
			
		}
		
		for (Entry<Pair<Integer, Integer>, EQclass> entry : EQclass_list.entrySet()) {
			 EQclass temp = entry.getValue();
		     System.out.println("level: " + temp.getLevel() + ". resource: " + temp.getResource());
		     for(int i = 0; i < temp.getAgents().size(); i++) {
		    	 System.out.println(temp.getAgents().get(i));
		     }
		}
	
	
	
	}
	
	public static void main(String[] args) throws IOException {
		File pref = new File("new.txt");
		RICA thiss = new RICA(pref);
	
	}
	
	
}
