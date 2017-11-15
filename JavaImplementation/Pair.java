public class Pair<L extends Integer,R extends Integer> implements Comparable<Pair<Integer,Integer>> {

  private final Integer left;
  private final Integer right;

  public Pair(Integer left, Integer right) {
    this.left = left;
    this.right = right;
  }

  public Integer getLeft() { return left; }
  public Integer getRight() { return right; }
  
  @Override
  public boolean equals(Object o) {
    if (!(o instanceof Pair)) return false;
    Pair pairo = (Pair) o;
    return this.left.equals(pairo.getLeft()) &&
           this.right.equals(pairo.getRight());
  }

@Override
public int compareTo(Pair<Integer, Integer> o) {
	// TODO Auto-generated method stub
	if (this.getLeft().compareTo(o.getLeft()) == 0) {
		if(this.getRight().compareTo(o.getRight()) > 0) return 1;
		else if(this.getRight().compareTo(o.getRight()) < 0) return -1;
		else return 0;
	}
	else if (this.getLeft().compareTo(o.getLeft()) > 0) return 1;
	else return -1;
	
}
}