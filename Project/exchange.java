import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;


class exchange {

    // Name of the file
    String DB_FILE = "calls.txt";
    // List of threads
    static List<calling> threads = new LinkedList<calling>();
    // Users Database
    public LinkedHashMap<String, ArrayList<String>> DB = new LinkedHashMap<String, ArrayList<String>>();

    exchange() {
        this.loadDB();
    }

    public static void main(String[] args) throws InterruptedException {

        // Loading DB
        exchange exchangeVar = new exchange();
        // Add New Line
        System.out.println();

        // Creating Threads
        exchangeVar.DB.entrySet().forEach(entry -> {
            calling thread = new calling(entry.getKey(), entry.getValue());
            exchange.threads.add(thread);
        });

        // Starting Threads
        for (calling thread : exchange.threads) {
            thread.start();
        }

        // Joining Threads
        for (calling thread : exchange.threads) {
            thread.join();
        }

        // Wait for 10 Seconds
        Thread.sleep(5000);
        // Print message and end the program
        System.out.println("\nMaster has received no replies for 10 seconds, ending...");

    }

    private void loadDB() {

        BufferedReader reader;

        try {
            reader = new BufferedReader(new FileReader(DB_FILE));
            String line = reader.readLine();
            while (line != null) {
                // Process Line
                String Key = "";
                ArrayList<String> Value = new ArrayList<String>();
                String tempLine[] = line.split(",");

                for (int i = 0; i < tempLine.length; i++) {
                    
                    if (i == 0) {
                        Key = tempLine[i].replaceAll("[^a-zA-Z0-9]", "");
                    } else {
                        Value.add(tempLine[i].replaceAll("[^a-zA-Z0-9]", ""));
                    }
                }
                // Store in DB
                DB.put(Key, Value);
                // read next line
                line = reader.readLine();
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Printing
        System.out.println("** Calls to be made **");
        DB.entrySet().forEach(entry -> {
            System.out.println(entry.getKey() + ":" + entry.getValue());
        });
    }

    static void printMessage(String message) {
        System.out.println(message);
    }
}