/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cash_access;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 *
 * @author sebastian
 */
class AccountSkeletonConnection extends Thread {
    private Socket socket;
    private PrintWriter out;
    private BufferedReader in;
    private Account account;
    private String inputLine;
    
    public AccountSkeletonConnection(Socket socket,Account account){
        this.socket=socket;
            try {
                out = new PrintWriter(socket.getOutputStream(), true);
                in = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
            } catch (IOException e) {
		e.printStackTrace();
            }
        this.account = account;
    }
    
    public void run() {
		try {
			System.out.println("New AccountSkeletonConnection started");
			while (!socket.isClosed() && ((inputLine = in.readLine()) != null)) {
                            String args[] = inputLine.split(";");
                            System.out.println("AccountSkeletonConnection got new request: "+ inputLine);
                            if(args[0].equals("deposit")){
                                try{
                                    account.deposit(Double.valueOf(args[1]));
                                    out.println("Result;void");
                                }catch (RuntimeException e){
                                    out.println("RuntimeException;"+e.getMessage());
                                }
                            }else if(args[0].equals("withdraw")){
                                try{
                                    account.withdraw(Double.valueOf(args[1]));
                                    out.println("Result;void");
                                } catch (OverdraftException ex) {
                                    out.println("OverdraftException;"+ex.getMessage());
                                } catch (RuntimeException e2){
                                    out.println("RuntimeException;"+e2.getMessage());
                                }
                            }else if(args[0].equals("getBalance")){
                                    double amount = account.getBalance();
                                    out.println("Result;"+String.valueOf(amount));
                            }
			}
			in.close();
			out.close();
			socket.close();
		} catch (IOException e) {
//			e.printStackTrace();
		} finally {
			System.out.println("AccountSkeletonConnection stopped");
		}
	}
    
}
