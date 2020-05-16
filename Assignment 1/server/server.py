import pathlib
import json
import socketserver

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
            

    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()        
        #pass to the menu
        self.menu(str(self.data,'utf-8'))

    def menu(self,argument):
        if argument == "find_customer" :
            self.find_customer()
        elif argument == "add_customer" :
            self.add_customer()
        elif argument == "delete_customer" :
            self.delete_customer()
        elif argument == "update_customer_age" :
            self.update_customer_age()
        elif argument == "update_customer_address" :
            self.update_customer_address()
        elif argument == "update_customer_phone" :
            self.update_customer_phone()
        elif argument == "print_report" :
            self.print_report()
        elif argument == "exit" :
            self.exit()

    # 1. Find customer
    def find_customer(self):
        # Get customer name from client
        customer_name = str(self.request.recv(1024).strip(), "utf-8")
        # find custmer into database
        result = memory_db.get(customer_name, "Customer not found")
        if type(result) == dict:
            result = json.dumps(result)
        # Send back result to client
        self.request.sendall(self.str_to_byte(result))
    
    # 2. Add customer
    def add_customer(self):
        # Get new customer's details from client
        new_customer = str(self.request.recv(1024).strip(), "utf-8")
        new_customer = json.loads(new_customer)
        # find custmer into database
        result = memory_db.get(new_customer['name'])
        # just send back the data
        if result is None:
            # Add customer
            memory_db[new_customer['name']] = new_customer
            self.request.sendall(self.str_to_byte("Customer has been added"))
        else:
            # Customer name already exist
            self.request.sendall(self.str_to_byte("Customer already exists"))
            

    # 3. Delete customer
    def delete_customer(self):
        try:
            # Get customer name from client
            customer_name = str(self.request.recv(1024).strip(), "utf-8")
            # find custmer into database
            result = memory_db.get(customer_name, "Customer does not exist")
            if type(result) == dict:
                # Delete customer
                del memory_db[customer_name] 
                # Send back result to client
                self.request.sendall(self.str_to_byte("Customer has been deleted"))
            else:
                self.request.sendall(self.str_to_byte(result))
        except e:
            Print(e)
    
    # 4. Update customer age
    def update_customer_age(self):
        # Get customer's name and age from client
        customer_details = str(self.request.recv(1024).strip(), "utf-8")
        customer_info = json.loads(customer_details)
        # find custmer into database
        result = memory_db.get(customer_info['name'], "Customer not found")
        # just send back the data
        if type(result) == dict:
            # Update customer's age
            memory_db[customer_info['name']]['age'] = customer_info['age']
            self.request.sendall(self.str_to_byte("Customer's age has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))
            


    # 5. Update customer address
    def update_customer_address(self):
        # Get customer's name and age from client
        customer_details = str(self.request.recv(1024).strip(), "utf-8")
        customer_info = json.loads(customer_details)
        # find custmer into database
        result = memory_db.get(customer_info['name'], "Customer not found")
        # just send back the data
        if type(result) == dict:
            # Update customer's address
            memory_db[customer_info['name']]['address'] = customer_info['address']
            self.request.sendall(self.str_to_byte("Customer's address has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))

    # 6. Update customer phone
    def update_customer_phone(self):
        # Get customer's name and age from client
        customer_details = str(self.request.recv(1024).strip(), "utf-8")
        customer_info = json.loads(customer_details)
        # find custmer into database
        result = memory_db.get(customer_info['name'], "Customer not found")
        # just send back the data
        if type(result) == dict:
            # Update customer's age
            memory_db[customer_info['name']]['phone'] = customer_info['phone']
            self.request.sendall(self.str_to_byte("Customer's phone has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))

    # 7. Print report
    def print_report(self):
        #Sort
        sorted_db = {k: memory_db[k] for k in sorted(memory_db)}
        # just send back the data
        customers = json.dumps(sorted_db)
        self.request.sendall(self.str_to_byte(customers))


    def str_to_byte(self,string):
        return bytes(string + "\n", "utf-8")

if __name__ == "__main__":

    HOST, PORT = "localhost", 9999

    # Check if file is exist or not 
    if not pathlib.Path('data.txt').exists():
        database_file = open("data.txt","w+")
    else:
        database_file = open("data.txt","r")
    
    # reading file and loading into the memory
    memory_db = {}

    for line in database_file:
        user = line.strip().split('|')
        if not user[0] == '':
            user_db ={}
            user_db['name'] = user[0].strip()
            user_db['age'] = user[1].strip()
            user_db['address'] = user[2].strip()
            user_db['phone'] = user[3].strip()
            memory_db[user[0]] =  user_db
        
    # Create the server, binding to localhost on port 9999
    with socketserver.TCPServer((HOST, PORT), MyTCPHandler) as server:
        # Activate the server; this will keep running until you
        # interrupt the program with Ctrl-C
        server.serve_forever()