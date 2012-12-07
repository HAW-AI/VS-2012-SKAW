/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cash_access;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;
import mware_lib.Skeleton;

/**
 *
 * @author sebastian
 */
public class AccountSkeleton extends Skeleton{
    private String name;
    private Account account;
    private int port;
    private ServerSocket serverSocket;
    
    public AccountSkeleton(String name, Account account){
        this.name = name;
        this.account = account;
        try {
            this.serverSocket = new ServerSocket(0);
            this.port = serverSocket.getLocalPort();
        } catch (IOException ex) {
            Logger.getLogger(AccountSkeleton.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }
    
    public void run(){
        try {
                System.out.println("In AccountSkeleton run");
                System.out.println("Port: "+this.port);
		while (true) {
			Socket socket = serverSocket.accept();
			AccountSkeletonConnection connection = new AccountSkeletonConnection(socket, account);
			System.out.println("starting new ns thread");
			connection.start();
		}

		} catch (IOException e) {
			e.printStackTrace();
		}

    }
    
    public int port(){
        return this.port;
    }
    
    public String type(){
        return "cash_access.Account";
    }
}
