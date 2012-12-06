/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package branch_access;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author sebastian
 */
class ManagerSkeletonConnection extends Thread {
    
    private Socket socket;
    private PrintWriter out;
    private BufferedReader in;
    private Manager manager;
    private String inputLine;
    
    public ManagerSkeletonConnection(Socket socket,Manager manager){
        this.socket=socket;
            try {
                out = new PrintWriter(socket.getOutputStream(), true);
                in = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
            } catch (IOException e) {
		e.printStackTrace();
            }
        this.manager = manager;
    }
        public void run() {
		try {
			System.out.println("New ManagerSkeletonConnection started");
			while (!socket.isClosed() && ((inputLine = in.readLine()) != null)) {
                            System.out.println("got new request: "+inputLine);
                            String information[] = inputLine.split(";");
				if(information[0].equals("getBalance")){
                                        double balance = manager.getBalance(information[1]);
                                        out.println("Result;"+String.valueOf(balance));
                                        System.out.println(" sent: Result;"+String.valueOf(balance));
                                }else if(information[0].equals("createAccount")){
                                    System.out.println("in createAccount");
                                        String result = manager.createAccount(information[1]);
                                        out.println("Result;"+result);
                                        System.out.println("sent: Result;"+result);
                                }else{
                                    out.println("UnkownMessageException;"+"Unknown Message: "+inputLine);
                                    System.out.println("UnkownMessageException;"+"Unknown Message: "+inputLine);
                                }
			}
			in.close();
			out.close();
			socket.close();
		} catch (IOException e) {
//			e.printStackTrace();
		} finally {
			System.out.println("Communicator stopped");
		}
	}
    
}
