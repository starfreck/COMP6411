import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

class exchange {

    String DB_FILE = "calls.txt";
    public LinkedHashMap<String, ArrayList<String>> DB = new LinkedHashMap<String, ArrayList<String>>();
    
    static List<calling> threads = new LinkedList<calling>();

    static long endWaitTime = System.currentTimeMillis() + 10000;
    boolean isConditionMet = false;

    exchange() {
        this.loadDB();
    }

    public static void main(String[] args) {

        // Loading DB
        exchange exchangeVar = new exchange();

        System.out.println();

        exchangeVar.DB.entrySet().forEach(entry -> {
            calling thread = new calling(entry.getKey(), entry.getValue());
            exchange.threads.add(thread);
        });

        for (calling thread : exchange.threads) {
            thread.start();
        }

        while(true){
            if(System.currentTimeMillis() > endWaitTime){
                notifyMaster();
                break;
            }
        }
    }

    private void loadDB() {

        BufferedReader reader;

        try {
            reader = new BufferedReader(new FileReader(DB_FILE));
            String line = reader.readLine();
            while (line != null) {
                // System.out.println(line);
                // Process Line
                String Key = "";
                ArrayList<String> Value = new ArrayList<String>();
                String tempLine[] = line.split(",");

                for (int i = 0; i < tempLine.length; i++) {
                    // System.out.println(tempLine[i].replaceAll("[^a-zA-Z0-9]", ""));

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

    static void notifyMaster() {
        System.out.println("Master has received no replies for 10 seconds, ending...");
    }
}