import sys
import json
import socket

HOST, PORT = "localhost", 9999


def menu():
    print("""Python DB Menu

1. Find customer
2. Add customer
3. Delete customer
4. Update customer age
5. Update customer address
6. Update customer phone
7. Print report
8. Exit
""")
    return input("Select:")    
    

# 1. Find customer
def find_customer(sock):
    #get customer name
    customer_name = input("Enter customer :")
    
    if not customer_name:
        print("\nError >>> {}".format("Customer's name cannot be empty\n"))
        return
    # send action name to server
    sock.sendall(str_to_byte("find_customer"))
    # send customer_name to server
    sock.sendall(str_to_byte(customer_name))
    # Receive data from the server and shut down
    received = str(sock.recv(1024), "utf-8")
    print()
    print("Server Response >>> {}".format(received))

# 2. Add customer
def add_customer(sock):    
    #get customer details
    customer_name    = input("Enter a new customer's name:")

    if not customer_name:
        print("\nError >>> {}".format("Customer's name cannot be empty\n"))
        return
    customer_age     = input("Enter a new customer's age:")
    customer_address = input("Enter a new customer's address:")
    customer_phone   = input("Enter a new customer's phone:")

    # create user's dictionary
    new_user = {"name":customer_name,"age":customer_age,"address":customer_address,"phone":customer_phone}
    
    # send action to server
    sock.sendall(str_to_byte("add_customer"))
    # send new user's data to server
    sock.sendall(str_to_byte(json.dumps(new_user)))
    # Receive data from the server and shut down
    received = str(sock.recv(1024), "utf-8")
    print()
    print("Server Response >>> {}".format(received))


# 3. Delete customer
def delete_customer(sock):
    #get customer name
    customer_name = input("Enter customer :")

    if not customer_name:
        print("\nError >>> {}".format("Customer's name cannot be empty\n"))
        return

    # send action to server
    sock.sendall(str_to_byte("delete_customer"))
    # send customer_name to server
    sock.sendall(str_to_byte(customer_name))
    # Receive data from the server and shut down
    received = str(sock.recv(1024), "utf-8")
    print()
    print("Server Response >>> {}".format(received))

# 4. Update customer age
def update_customer_age(sock):
    #Get customer name
    customer_name = input("Enter customer's name :")
    if not customer_name:
        print("\nError >>> {}".format("Customer's name cannot be empty\n"))
        return
    #Get new age of customer
    customer_age = input("Enter new age for {}:".format(customer_name))
    if not customer_age or not is_int(customer_age):
        print("\nError >>> {}".format("Customer's age cannot be empty and/or string\n"))
        return

    # send action to server
    sock.sendall(str_to_byte("update_customer_age"))
    # send customer_name and customer_age to server
    sock.sendall(str_to_byte(json.dumps({"name":customer_name,"age":customer_age})))
    # Receive data from the server and shut down
    received = str(sock.recv(1024), "utf-8")
    print()
    print("Server Response >>> {}".format(received))

# 5. Update customer address
def update_customer_address(sock):
    print("Update customer address")
    # just send back the data
    sock.sendall(str_to_byte("Update customer address"))

# 6. Update customer phone
def update_customer_phone(sock):
    print("Update customer phone")
    # just send back the data
    sock.sendall(str_to_byte("Update customer phone"))

# 7. Print report
def print_report(sock):
    # send action to server
    sock.sendall(str_to_byte("print_report"))

    # Receive data from the server and shut down
    received = str(sock.recv(1024), "utf-8")

    customers = json.load(received)
    
    print ("{:<8} {:<15} {:<10} {:<10}".format('Name','Age','Address','Phone'))
    for customer in customers.items():
        print("{:<8} {:<15} {:<10} {:<10}".format(customer['name'],customer['age'],customer['address'],customer['phone']))

# 8. Exit
def exit(sock):
    print("Exit")
    # just send back the data
    sock.sendall(str_to_byte("Exit Server"))

def str_to_byte(string):
    return bytes(string + "\n", "utf-8")

def is_int(val):
    try:
        num = int(val)
    except ValueError:
        return False
    return True

# Menu for Client






if __name__ == "__main__":

    while True:
        # Create a socket (SOCK_STREAM means a TCP socket)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            # Connect to server and send data
            sock.connect((HOST, PORT))

            # build Switch case here
            try:
                choice = int(menu())

                # 1. Find customer
                if choice == 1 :
                    find_customer(sock)
                elif choice == 2 :
                    add_customer(sock)
                elif choice == 3 :
                    delete_customer(sock)
                elif choice == 4 :
                    update_customer_age(sock)
                elif choice == 5 :
                    update_customer_address(sock)
                elif choice == 6 :
                    update_customer_phone(sock)
                elif choice == 7 :
                    print_report(sock)
                elif choice == 8 :
                    exit(sock)
                else:
                    print("Please, select valid option...")
            except:
                print("Only \"Integer\" values are accepted")

           



           
            # Receive data from the server and shut down
            #received = str(sock.recv(1024), "utf-8")
        #print()
        # print("Sent:     {}".format(data))
        #print("Server Response >>> {}".format(received))
