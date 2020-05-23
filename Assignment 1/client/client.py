# File : client.py
# Author : Vasu Ratanpara
# Student ID : 40135264
# E-mail : vasu.ratanpara@mail.concordia.ca

import re
import sys
import json
import socket

# Global Vars
HOST, PORT, MAX_SIZE = "localhost", 9999, 1024

# ANSI Escape Sequence Colors
GreenColor, RedColor, YellowColor, LightPurpleColor, PurpleColor, CyanColor, LightGrayColor, BlackColor, ResetColor ="\033[92m", "\033[91m", "\033[93m", "\033[94m", "\033[95m", "\033[96m","\033[97m","\033[98m","\033[00m"

# Menu for Client
def menu():
    print("\nPython DB Menu\n1. Find customer\n2. Add customer\n3. Delete customer\n4. Update customer age\n5. Update customer address\n6. Update customer phone\n7. Print report\n8. Exit\n")
    return input("Select:").strip()   
    
# 1. Find customer
def find_customer():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return

    # Preparing Data to send   
    data = {"action":"find_customer","name":customer_name}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received,True)

# 2. Add customer
def add_customer():    
    # Get customer details
    customer_name    = input("Enter a new customer's name:").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return

    customer_age     = input("Enter a new customer's age:").strip()
    # Validate customer_age
    if not is_valid_age(customer_age):return

    customer_address = input("Enter a new customer's address:").strip()
    # Validate customer_address
    if not is_valid_address(customer_address):return

    customer_phone   = input("Enter a new customer's phone:").strip()
    # Validate customer_phone
    if not is_valid_phone(customer_phone):return

    # Create user's dictionary
    data = {"action":"add_customer","name":customer_name,"age":customer_age,"address":customer_address,"phone":customer_phone}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)
    if json.loads(received)['status'] == "true":
        print_report()


# 3. Delete customer
def delete_customer():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return

    # Preparing Data to send   
    data = {"action":"delete_customer","name":customer_name}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)
    if json.loads(received)['status'] == "true":
        print_report()

# 4. Update customer age
def update_customer_age():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return

    # Get new age of customer
    customer_age = input("Enter new age for {}:".format(customer_name)).strip()
    # Validate customer_age
    if not is_valid_age(customer_age):return

    # Preparing Data to send   
    data = {"action":"update_customer_age","name":customer_name,"age":customer_age}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)
    if json.loads(received)['status'] == "true":
        print_report()

# 5. Update customer address
def update_customer_address():
    #Get customer name
    customer_name = input("Enter a customer's name :").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return
    # Get new address of customer
    customer_address = input("Enter new address for {}:".format(customer_name)).strip()
    # Validate customer_address
    if not is_valid_address(customer_address):return

    # Preparing Data to send   
    data = {"action":"update_customer_address","name":customer_name,"address":customer_address}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)
    if json.loads(received)['status'] == "true":
        print_report()

# 6. Update customer phone
def update_customer_phone():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip().lower()
    # Validate customer_name
    if not is_valid_name(customer_name):return
    # Get new phone of customer
    customer_phone = input("Enter new phone for {}:".format(customer_name)).strip()
    # Validate customer_phone
    if not is_valid_phone(customer_phone):return
    # Preparing Data to send   
    data = {"action":"update_customer_phone","name":customer_name,"phone":customer_phone}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)
    if json.loads(received)['status'] == "true":
        print_report()

# 7. Print report
def print_report():
    # Preparing Data to send   
    data = {"action":"print_report"}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Converting into 'dict'
    customers = json.loads(received)    
    
    print (GreenColor+"\n{:<10} {:<10} {:<20} {:<25}".format('Name','Age','Address','Phone')+ResetColor)
    for customer in customers.values():
        print(CyanColor+"{:<10} {:<10} {:<20} {:<25}".format(customer['name'],customer['age'],customer['address'],customer['phone'])+ResetColor)

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

# Check if the name is valid
def is_valid_name(customer_name):
    if customer_name == "" or not customer_name.isalpha() or customer_name.isdigit():
        print_error("Customer's name cannot be empty, Integer and Alphanumeric")
        return False
    else:
        return True

# Check if the age is valid
def is_valid_age(customer_age):
    if  customer_age != "" and not customer_age.isdigit():
        print_error("Customer's age cannot be string")
        return False
    elif customer_age != "" and 0 <= int(customer_age) > 100:
        print_error("Customer's age can only between 0 to 100")
        return False
    else:
        return True

# Check if the address number is valid
def is_valid_address(customer_address):
    if customer_address != "" and customer_address.isdigit():
        print_error("Customer's address cannot be only Integer")
        return False
    else:
        return True

# Check if the phone number is valid
def is_valid_phone(customer_phone):
    charRe = re.compile(r'[^-+() 0-9]')
    string = charRe.search(customer_phone)
    if len(list(filter(lambda x: x.isdigit(),customer_phone))) > 11 :
        print_error("Customer's phone can only contain 11 digits E.g. +1 (123) 456-7890")
    if bool(string):
        print_error("Customer's phone can only contain '+','-','(',')' and numbers E.g. +1 (123) 456-7890")
    return not bool(string)

# Print Error message 
def print_error(message):
    print(RedColor+"\n Error >>> {}\n".format(message)+ResetColor)

# Print Server message 
def print_server_msg(message,PrintUser=False):
    
    message = json.loads(message)
    
    # set color
    if message['status'] == "false":
        color = RedColor
    else:
        color = GreenColor

    if PrintUser and message['status'] == "true":
        user_details = (message['message']['name'],message['message']['age'],message['message']['address'],message['message']['phone'])
        print(YellowColor+"\n Server Response >>>"+ResetColor+color+" {}\n".format(user_details)+ResetColor)
    else:    
        print(YellowColor+"\n Server Response >>>"+ResetColor+color+" {}\n".format(message['message'])+ResetColor)


def communicate(data):
    try:
        # Create a socket (SOCK_STREAM means a TCP socket)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            # Connect to server and send data
            sock.connect((HOST, PORT))
            sock.sendall(bytes(data + "\n", "utf-8"))

            # Receive data from the server and shut down
            received = str(sock.recv(1024), "utf-8")
        return received

    except Exception as e:
        print_error("Server is not running "+str(e))


if __name__ == "__main__":

    while True:
        try:
            choice = int(menu())
            # 1. Find customer
            if choice == 1 :
                find_customer()
            elif choice == 2 :
                add_customer()
            elif choice == 3 :
                delete_customer()
            elif choice == 4 :
                update_customer_age()
            elif choice == 5 :
                update_customer_address()
            elif choice == 6 :
                update_customer_phone()
            elif choice == 7 :
                print_report()
            elif choice == 8 :
                print("\nGood bye")
                break
            else:
                print_error("Please, select valid option")
        except ValueError:
            print_error("Only \"Integer\" values are accepted")
        except Exception as e:
            print_error(e)