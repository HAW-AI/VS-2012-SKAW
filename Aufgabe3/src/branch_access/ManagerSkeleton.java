/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package branch_access;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;
import mware_lib.Skeleton;

/**
 *
 * @author sebastian
 */
public class ManagerSkeleton extends Skeleton {
    private String name;
    private Manager manager;
    private int port;
    private ServerSocket serverSocket;
    
    public ManagerSkeleton(String name, Manager m){
        this.name = name;
        this.manager = m;
        try {
            this.serverSocket = new ServerSocket(0);
            this.port = serverSocket.getLocalPort();
        } catch (IOException ex) {
            Logger.getLogger(ManagerSkeleton.class.getName()).log(Level.SEVERE, null, ex);
        }
        System.out.println("Manager erstellt: "+name);
    }
    
    public static Skeleton create(String name, Manager m){
        return new ManagerSkeleton(name, m);
    }
    
    public void run(){
        try {
                System.out.println("In AccountSkeleton run");
                System.out.println("Port: "+this.port);
		while (true) {
			Socket socket = serverSocket.accept();
			ManagerSkeletonConnection connection = new ManagerSkeletonConnection(socket, manager);
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

    @Override
    public String type() {
        return "branch_access.Manager";
    }

    
    
}
