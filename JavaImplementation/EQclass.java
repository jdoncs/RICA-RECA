import java.util.ArrayList;
import java.util.Collections;

public class EQclass implements Comparable<EQclass>{
	int mLevel;
	int mResource;
	ArrayList<Integer> Agents;
	
	public EQclass(int level, int resource, int initalag) {
		mLevel = level;
		mResource = resource;
		Agents = new ArrayList<Integer>();
		Agents.add(initalag);
	}
	
	public void addAgent(int agent) {
		Agents.add(agent);
	}
	
	public int getLevel() {
		return mLevel;
	}
	
	public int getResource() {
		return mResource;
	}
	
	public ArrayList<Integer> getAgents(){
		return Agents;
	}
	
	public int getRandomAgent() {
		Collections.shuffle(Agents);
		return Agents.get(0);
	}

	@Override
	public int compareTo(EQclass arg0) {
		
		return 0;
	}
}
