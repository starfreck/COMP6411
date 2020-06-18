import java.util.ArrayList;
import java.util.Random;

public class calling extends Thread {

    String name;
    ArrayList contactList;

    long endWaitTime = System.currentTimeMillis() + (5 * 1000);
    boolean isConditionMet = false;

    calling(String name, ArrayList contactList) {

        this.name = name;
        this.contactList = contactList;
    }

    public void run() {

        // Do something here
        for (Object person : contactList) {
           
            try {
                 // Sleeping
                Thread.sleep(new Random().nextInt(100) + 1);

                synchronized (this) {
                    // Add looping logic
                    for (calling t : exchange.threads) {
    
                        if (person.toString().equals(t.getCallerName())) {
    
                            t.reciveMessage(this, person.toString() + " received intro message from " + name + " ["
                                    + System.currentTimeMillis() + "]");
                            // msg.notify();
                            // msg.notifyAll();
                        }
                    }
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        while (System.currentTimeMillis() < endWaitTime) {

            // Do something here

        }

        System.out.println("Process " + name + " has received no calls for 5 seconds, ending...");
    }

    public void sendMessage(String message) {
        exchange.printMessage(message);
    }

    public void reciveMessage(calling sender, String Message) {
        exchange.printMessage(Message);
        sender.sendMessage(
                sender.name + " received reply message from " + name + " [" + System.currentTimeMillis() + "]");

    }

    public String getCallerName() {
        return name;
    }
}