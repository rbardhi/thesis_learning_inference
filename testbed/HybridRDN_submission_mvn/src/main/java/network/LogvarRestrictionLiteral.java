package hybrid.network;

public class LogvarRestrictionLiteral extends Literal{
    private Logvar logvar1;
    private Logvar logvar2;
	private String restriction;
	
	public LogvarRestrictionLiteral(String restriction,Logvar logvar1,Logvar logvar2){
       this.restriction=restriction;
       this.logvar1=logvar1;
       this.logvar2=logvar2;
	}
	
	@Override
	public  String createFOLTerm(){
		return this.toString();
	}

	public String toString(){
		return logvar1+this.restriction.toString()+logvar2;
	}
	
	



}
