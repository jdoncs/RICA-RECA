import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.TreeMap;

public class RICA {
	TreeMap<Pair<Integer, Integer>, EQclass> EQclass_list;
	TreeMap<Integer, Integer> Allocations;
	ArrayList<Integer> UnassignedAgents;
	ArrayList<Integer> UnassignedResources;
	int Kvalue;
	
	public RICA(File preferencefile) throws IOException {
		EQclass_list = new TreeMap<Pair<Integer, Integer>, EQclass>();
		Scanner scanner = new Scanner(preferencefile);
		String bufferline = scanner.nextLine();
		String[] linearray = bufferline.split(" ");
		Kvalue = Integer.parseInt(linearray[6]);

		int Agent = 0;
		int level = 1;
	
		
		while (scanner.hasNextLine()) {
			String bufferline2 = scanner.nextLine();
			String[] linearray2 = bufferline2.split(", ");
			for(int i = 0; i < linearray2.length; i++) {
				String stripped = linearray2[i].trim();
				int resource = Integer.parseInt(stripped);
				
				
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
		/** for testing
		for (Entry<Pair<Integer, Integer>, EQclass> entry : EQclass_list.entrySet()) {
			 EQclass temp = entry.getValue();
		     System.out.println("level: " + temp.getLevel() + ". resource: " + temp.getResource());
		     for(int i = 0; i < temp.getAgents().size(); i++) {
		    	 System.out.println(temp.getAgents().get(i));
		     }
		}
	  */
	
	
	}
	public void shift(int Agent) {
		for(int i = 0; i < EQclass_list.size(); i++) {
			Pair key = (Pair) EQclass_list.keySet().toArray()[i];
			EQclass value = EQclass_list.get(key);
			for(int k = 0; k < value.getAgents().size(); k++) {
				if(value.getAgents().get(k).equals(Agent)) {
					value.getAgents().remove(k);
					if(value.mLevel == 1) {
						
					}
					else {
						Pair newpair = new Pair(value.mLevel - 1, value.mResource);
						EQclass_list.get(newpair).Agents.add(k);
					}
					
				}
			}
		}
	}
	
	public void deleteAllResources(int resource) {
		for(int i = 0; i < EQclass_list.size(); i++) {
			Pair key = (Pair) EQclass_list.keySet().toArray()[i];
			if(key.getRight().equals(resource)){
				EQclass_list.remove(key);
			}
			
		}
	}
	
	public void deleteAllAgents(int Agent) {
		ArrayList<Integer> shiftingAgents = new ArrayList<Integer>();
		for(int i = 0; i < EQclass_list.size(); i++) {
			Pair key = (Pair) EQclass_list.keySet().toArray()[i];
			EQclass value = EQclass_list.get(key);
			for(int k = 0; k < value.getAgents().size(); k++) {
				if(value.getAgents().get(k).equals(Agent)) {
					value.getAgents().remove(k);
					shiftingAgents.addAll(value.getAgents());
				}
			}
		}
		
		for(int j = 0; j < shiftingAgents.size(); j++) {
			shift(shiftingAgents.get(j));
		}
	}
	
	public void allocate() throws FileNotFoundException {
		PrintWriter pw = new PrintWriter(new File("Alloc.txt"));
        StringBuilder sb = new StringBuilder();
        
		int currentEQ = 1;
		int maxsize = EQclass_list.size();
		int currentsize = maxsize;
		Pair mxkey = new Pair(null,null);
		while(!EQclass_list.isEmpty()) {
			for(int i = 0; i < EQclass_list.size(); i++) {
				Pair key = (Pair) EQclass_list.keySet().toArray()[i];
				EQclass value = EQclass_list.get(key);
				if(!key.getLeft().equals(currentEQ)) {
					break;
				}
				if(value.getAgents().size() < currentsize) {
					currentsize = value.getAgents().size();
					mxkey = key;
				}
				
				
			}
			if(currentEQ == Kvalue) {
				currentEQ = 1;
			}
			else currentEQ++;
			currentsize = maxsize;
			int ag = EQclass_list.get(mxkey).getRandomAgent();
			int currentresource = EQclass_list.get(mxkey).getResource();
			
			sb.append("Resource ");
			sb.append(currentresource);
			sb.append(": ");
			sb.append(ag);
			sb.append('\n');
			
			deleteAllResources(currentresource);
			deleteAllAgents(ag);
			
			
			
			
			
			
			
		}
	}
	public static void main(String[] args) throws IOException {
		File pref = new File("new.txt");
		RICA thiss = new RICA(pref);
		thiss.allocate();
	
	}
	
	
}
