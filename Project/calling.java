import java.util.*;

public class calling extends Thread {

    String name;
    ArrayList<?> contactList;

    long endWaitTime = System.currentTimeMillis();

    calling(String name, ArrayList<?> contactList) {

        this.name = name;
        this.contactList = contactList;
    }

    @Override
    public void run() {
        
        try {
            // Send Messages Here
            for (Object person : contactList) {
                // Send Message to all Contacts
                for (calling t : exchange.threads) {
                    if (person.toString().equals(t.name)) {
                        // Sleeping
                        sleep(new Random().nextInt(50)+new Random().nextInt(50));
                        
                        synchronized(this) {
                            long timestamp = System.currentTimeMillis();
                            t.sendMessage(this,t.name+" recived intro message from "+name+" ["+timestamp+"]",timestamp);
                            notify();
                        }
                    }
                }
            }

            //Wait for 5 Seconds after last message recived
            while (true) {

                if((System.currentTimeMillis() - endWaitTime) > 5000) {
                    System.out.println("\nProcess " + name + " has received no calls for 5 seconds, ending...\n");
                    break;
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public void sendMessage(calling Sender,String message,long timestamp) throws InterruptedException {
    
        // Print Message
        exchange.printMessage(message);
        
        // Sleeping
        Thread.sleep(new Random().nextInt(50)+new Random().nextInt(50));
        
        // Send reply to Sender
        Sender.receiveMessage(Sender.name+" recived reply message from "+name+"["+timestamp+"]");
        
    }

    public void receiveMessage(String message) throws InterruptedException {

        // Print Message
        exchange.printMessage(message);
                
        // Reset Timer
        endWaitTime =  System.currentTimeMillis();

        // Sleeping
        Thread.sleep(new Random().nextInt(50)+new Random().nextInt(50));

    }
}