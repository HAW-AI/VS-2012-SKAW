package branch_access;

public abstract class Manager {
	public abstract String createAccount(String owner);
	public abstract double getBalance(String accountID);
}