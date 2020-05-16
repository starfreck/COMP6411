import sys
import json
import socket
from collections import OrderedDict

# Global Vars
HOST, PORT, MAX_SIZE = "localhost", 9999, 1024

# Menu for Client
def menu():
    print("Python DB Menu\n1. Find customer\n2. Add customer\n3. Delete customer\n4. Update customer age\n5. Update customer address\n6. Update customer phone\n7. Print report\n8. Exit\n")
    return input("Select:").strip()   
    
# 1. Find customer
def find_customer(sock):
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return
    # send action to server
    sock.sendall(str_to_byte("find_customer"))
    # send customer_name to server
    sock.sendall(str_to_byte(customer_name))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)

# 2. Add customer
def add_customer(sock):    
    # Get customer details
    customer_name    = input("Enter a new customer's name:").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    customer_age     = input("Enter a new customer's age:").strip()
    customer_address = input("Enter a new customer's address:").strip()
    customer_phone   = input("Enter a new customer's phone:").strip()

    # Create user's dictionary
    new_user = {"name":customer_name,"age":customer_age,"address":customer_address,"phone":customer_phone}
    
    # Send action to server
    sock.sendall(str_to_byte("add_customer"))
    # Send new user's data to server
    sock.sendall(str_to_byte(json.dumps(new_user)))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)


# 3. Delete customer
def delete_customer(sock):
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    # Send action to server
    sock.sendall(str_to_byte("delete_customer"))
    # Send customer_name to server
    sock.sendall(str_to_byte(customer_name))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)

# 4. Update customer age
def update_customer_age(sock):
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    # Get new age of customer
    customer_age = input("Enter new age for {}:".format(customer_name)).strip()
    # Validate customer_age
    if not customer_age or not is_int(customer_age):
        print_error("Customer's age cannot be empty and/or String")
        return

    # Send action to server
    sock.sendall(str_to_byte("update_customer_age"))
    # Send customer_name and customer_age to server
    sock.sendall(str_to_byte(json.dumps({"name":customer_name,"age":customer_age})))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)

# 5. Update customer address
def update_customer_address(sock):
    #Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return
    # Get new address of customer
    customer_address = input("Enter new address for {}:".format(customer_name)).strip()
    # Validate customer_address
    if not customer_address or is_int(customer_address):
        print_error("Customer's age cannot be empty and/or String")
        return

    # Send action to server
    sock.sendall(str_to_byte("update_customer_address"))
    # Send customer_name and customer_age to server
    sock.sendall(str_to_byte(json.dumps({"name":customer_name,"address":customer_address})))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)

# 6. Update customer phone
def update_customer_phone(sock):
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return
    # Get new phone of customer
    customer_phone = input("Enter new address for {}:".format(customer_name)).strip()
    # Validate customer_phone
    if not customer_phone:
        print_error("Customer's phone cannot be empty")
        return

    # Send action to server
    sock.sendall(str_to_byte("update_customer_phone"))
    # Send customer_name and customer_age to server
    sock.sendall(str_to_byte(json.dumps({"name":customer_name,"phone":customer_phone})))
    # Receive data from the server
    received = str(sock.recv(MAX_SIZE), "utf-8")
    print_server_msg(received)

# 7. Print report
def print_report(sock):
    # Send action to server
    sock.sendall(str_to_byte("print_report"))
    # Receive data from the server and shut down
    received = str(sock.recv(MAX_SIZE), "utf-8")
    # Converting into 'dict'
    customers = json.loads(received)    
    
    print ("\n{:<10} {:<10} {:<20} {:<25}".format('Name','Age','Address','Phone'))
    for customer in customers.values():
        print("{:<10} {:<10} {:<20} {:<25}".format(customer['name'],customer['age'],customer['address'],customer['phone']))
    print()

# Conver String to Bytes
def str_to_byte(string):
    return bytes(string + "\n", "utf-8")

# Check if given value is Integer
def is_int(val):
    try:
        num = int(val)
    except ValueError:
        return False
    return True

# Print Error message 
def print_error(message):
    print("\n\033[91m Error >>> {}\033[00m\n".format(message))

# Print Server message 
def print_server_msg(message):
    print("\n\033[92m Server Response >>> {}\033[00m\n".format(message))


if __name__ == "__main__":

    while True:

        try:
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
                        print("\nGood bye")
                        break
                        exit(0)
                    else:
                        print_error("Please, select valid option")
                except ValueError:
                    print_error("Only \"Integer\" values are accepted")
                except Exception as e:
                    print_error(e)
        except Exception as e:
            print_error("Server is not running")
            break
