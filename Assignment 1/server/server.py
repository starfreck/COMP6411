import pathlib
import json
import socketserver

# Global Vars
HOST, PORT, MAX_SIZE = "localhost", 9999, 1024

class MyTCPHandler(socketserver.BaseRequestHandler):    

    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(MAX_SIZE).strip()
        str_data  = str(self.data,'utf-8')        
        #pass to the menu
        request_data = json.loads(str_data)
        if not str_data in (None, ''):
            self.menu(request_data)
            
    # Menu action controller
    def menu(self,argument):    
        if argument['action'] == "find_customer" :
            self.find_customer(argument)
        elif argument['action'] == "add_customer" :
            self.add_customer(argument)
        elif argument['action'] == "delete_customer" :
            self.delete_customer(argument)
        elif argument['action'] == "update_customer_age" :
            self.update_customer_age(argument)
        elif argument['action'] == "update_customer_address" :
            self.update_customer_address(argument)
        elif argument['action'] == "update_customer_phone" :
            self.update_customer_phone(argument)
        elif argument['action'] == "print_report" :
            self.print_report()

    # 1. Find customer
    def find_customer(self,argument):
        # Get customer name from client
        customer_name = argument['name']
        # Find custmer into database
        result = memory_db.get(customer_name)
        if result is None:
            self.request.sendall(self.str_to_byte("Customer not found"))
        else:
            # Send back result to client
            self.request.sendall(self.str_to_byte(json.dumps(result)))
        return
            
    # 2. Add customer
    def add_customer(self,argument):
        
        del argument['action']
        # Find custmer into database
        result = memory_db.get(argument['name'])
        
        if result is None:
            # Store customer into memory_db
            memory_db[argument['name']] = argument
            self.request.sendall(self.str_to_byte("Customer has been added"))
        else:
            # Customer name already exist
            self.request.sendall(self.str_to_byte("Customer already exists"))
        return

    # 3. Delete customer
    def delete_customer(self,argument):
        # Get customer name from client
        customer_name = argument['name']
        # Find custmer into database
        result = memory_db.get(customer_name, "Customer does not exist")
        if type(result) == dict:
            # Delete customer
            del memory_db[customer_name] 
            # Send back result to client
            self.request.sendall(self.str_to_byte("Customer has been deleted"))
        else:
            self.request.sendall(self.str_to_byte(result))
        return

    # 4. Update customer age
    def update_customer_age(self,argument):
        
        # Find custmer into database
        result = memory_db.get(argument['name'], "Customer not found")
        
        if type(result) == dict:
            # Update customer's age
            memory_db[argument['name']]['age'] = argument['age']
            self.request.sendall(self.str_to_byte("Customer's age has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))
        return    


    # 5. Update customer address
    def update_customer_address(self,argument):
    
        # Find custmer into database
        result = memory_db.get(argument['name'], "Customer not found")
        
        if type(result) == dict:
            # Update customer's address
            memory_db[argument['name']]['address'] = argument['address']
            self.request.sendall(self.str_to_byte("Customer's address has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))
        return

    # 6. Update customer phone
    def update_customer_phone(self,argument):

        # Find custmer into database
        result = memory_db.get(argument['name'], "Customer not found")
        
        if type(result) == dict:
            # Update customer's phone
            memory_db[argument['name']]['phone'] = argument['phone']
            self.request.sendall(self.str_to_byte("Customer's phone has been updated"))
        else:
            self.request.sendall(self.str_to_byte(result))
        return

    # 7. Print report
    def print_report(self):
        # Sort customers
        sorted_db = {k: memory_db[k] for k in sorted(memory_db)}
        # Covert from 'dict' to 'json'
        customers = json.dumps(sorted_db)
        self.request.sendall(self.str_to_byte(customers))
        return

    # Convert sting to bytes
    def str_to_byte(self,string):
        return bytes(string + "\n", "utf-8")

    # Store in File
    def store_in_file(self,data):
        print(data)
        

if __name__ == "__main__":

    # Check if file is exist or not 
    if not pathlib.Path('data.txt').exists():
        #database_file = open("data.txt","w+")
        print("Error >>> data.txt file doesn't exists in current directory")
        exit(0)
    else:
        database_file = open("data.txt","r")
    
    # Reading file and loading into the memory
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