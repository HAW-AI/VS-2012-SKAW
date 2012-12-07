/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package branch_access;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 *
 * @author sebastian
 */
public class ManagerProxy extends Manager {
    private String name;
    private InetSocketAddress address;
    private Socket server;
    BufferedReader in;
    PrintWriter out;

    public ManagerProxy(String name, InetSocketAddress address){
        this.name = name;
        this.address = address;
        try {
            server = new Socket(address.getAddress(),address.getPort());
            in = new BufferedReader(new InputStreamReader(server.getInputStream()));
            out = new PrintWriter(server.getOutputStream(),true);
            System.out.println("ManagerProxy <"+server.getLocalAddress().toString()+String.valueOf(server.getLocalPort())+"> created");
        } catch (IOException ex) {
            throw new RuntimeException(ex.getMessage());
        }
    }
    @Override
    public String createAccount(String owner) {
        out.println("createAccount;"+owner);
        System.out.println("ManagerProxy <"+server.getLocalAddress().toString()+String.valueOf(server.getLocalPort())+"> sent: createAccount"+owner);
        try {
            String incoming = in.readLine();
            String results[] = incoming.split(";");
            System.out.println("ManagerProxy <"+server.getLocalAddress().toString()+String.valueOf(server.getLocalPort())+"> received: "+incoming);
            if(results[0].equals("Result")){
                return results[1];
            }else{
                throw new RuntimeException(results[1]);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex.getMessage());
        }
    }

    @Override
    public double getBalance(String accountID) {
        out.println("getBalance;"+accountID);
        System.out.println("ManagerProxy <"+server.getLocalAddress().toString()+String.valueOf(server.getLocalPort())+"> sent: getBalance;"+accountID);
        try {
            String incoming = in.readLine();
            String results[] = incoming.split(";");
            System.out.println("ManagerProxy <"+server.getLocalAddress().toString()+String.valueOf(server.getLocalPort())+"> received: "+incoming);
            if(results[0].equals("Result")){
                return Double.valueOf(results[1]);
            }else{
                throw new RuntimeException(results[1]);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex.getMessage());
        }
    }
    
}
