/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cash_access;

import branch_access.ManagerProxy;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author sebastian
 */
public class AccountProxy extends Account{
    private String name;
    private InetSocketAddress address;
    private Socket server;
    BufferedReader in;
    PrintWriter out;
    
    public AccountProxy(String name, InetSocketAddress address){
        this.name = name;
        this.address = address;
        try {
            server = new Socket(address.getAddress(),address.getPort());
            in = new BufferedReader(new InputStreamReader(server.getInputStream()));
            out = new PrintWriter(server.getOutputStream(),true);
        } catch (IOException ex) {
            Logger.getLogger(ManagerProxy.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public String createAccount(String owner) {
        out.println("createAccount;"+owner);
        System.out.println("Schicke createAccount: "+owner);
        try {
            String results[] = in.readLine().split(";");
            System.out.println("got account_id:"+results[1]);
            return results[1];
        } catch (IOException ex) {
            System.out.println("IOException bei createAccount");
        }
        return null;
    }

    @Override
    public void deposit(double amount) {
        out.println("deposit;"+String.valueOf(amount));
        try {
            String results[] = in.readLine().split(";");
            if(results[0].equals("InterruptedException")){
                throw new RuntimeException(results[1]);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex.getMessage());
        }
    }

    @Override
    public void withdraw(double amount) throws OverdraftException {
        out.println("withdraw;"+String.valueOf(amount));
        try{
            String results[] = in.readLine().split(";");
            if(results[0].equals("InterruptedException")){
                throw new RuntimeException(results[1]);
            }else if(results[0].equals("OverdraftException")){
                throw new OverdraftException(results[1]);
            }
        }catch(IOException e){
            throw new RuntimeException(e.getMessage());
        }
    }

    @Override
    public double getBalance() {
        out.println("getBalance;"+this.name);
        try{
            String results[] = in.readLine().split(";");
            if(results[0].equals("InterruptedException")){
                throw new RuntimeException(results[1]);
            }else{
                return Double.valueOf(results[1]);
            }
        }catch(IOException e){
            throw new RuntimeException(e.getMessage());
        }
    }
    

    
}
