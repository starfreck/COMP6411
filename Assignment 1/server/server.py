import pathlib
import socketserver

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
            

    def handle(self):

        # Check if file is exist or not 
        if not pathlib.Path('data.txt').exists():
            self.database_file = open("data.txt","w+")
        else:
            self.database_file = open("data.txt","r")
        # reading file and loading into the memory
        self.memory_db = {}
        for line in self.database_file:
            print(line.strip().split('|'))
       

        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
        #print("{} wrote:".format(self.client_address[0]))
        #print(self.data)
        try:
            self.menu(int(self.data))
        except:
            self.request.sendall(self.str_to_byte("Only \"Integer\" values are accepted"))


    def menu(self,argument):
        switcher = {
            1: "find_customer",
            2: "add_customer",
            3: "delete_customer",
            4: "update_customer_age",
            5: "update_customer_address",
            6: "update_customer_phone",
            7: "print_report",
            8: "exit"
        }
        # Get the function from switcher dictionary
        default = self.request.sendall(self.str_to_byte("Please, select valid option..."))
        return getattr(self,switcher[argument],lambda: default)()

    # 1. Find customer
    def find_customer(self):
        print("Find customer")
        # just send back the data
        self.request.sendall(self.str_to_byte("Find customer"))

    # 2. Add customer
    def add_customer(self):
        print("Add customer")
        # just send back the data
        self.request.sendall(self.str_to_byte("Add customer"))

    # 3. Delete customer
    def delete_customer(self):
        print("Delete customer")
        # just send back the data
        self.request.sendall(self.str_to_byte("Delete customer"))
    
    # 4. Update customer age
    def update_customer_age(self):
        print("Update customer age")
        # just send back the data
        self.request.sendall(self.str_to_byte("Update customer age"))

    # 5. Update customer address
    def update_customer_address(self):
        print("Update customer address")
        # just send back the data
        self.request.sendall(self.str_to_byte("Update customer address"))

    # 6. Update customer phone
    def update_customer_phone(self):
        print("Update customer phone")
        # just send back the data
        self.request.sendall(self.str_to_byte("Update customer phone"))

    # 7. Print report
    def print_report(self):
        print("Print report")
        # just send back the data
        self.request.sendall(self.str_to_byte("Print report"))

    # 8. Exit
    def exit(self):
        print("Exit")
        # just send back the data
        self.request.sendall(self.str_to_byte("Exit Server"))

    def str_to_byte(self,string):
        return bytes(string + "\n", "utf-8")

if __name__ == "__main__":

    HOST, PORT = "localhost", 9999

    # Create the server, binding to localhost on port 9999
    with socketserver.TCPServer((HOST, PORT), MyTCPHandler) as server:
        # Activate the server; this will keep running until you
        # interrupt the program with Ctrl-C
        server.serve_forever()