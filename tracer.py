#!/usr/bin/python

# import modules to be used during the execution
import socket, argparse, sys, re, time

# defines default port, max_ttl and timeout
port = 33434
maxHops = 30
timeout = 2000
numPings = 3

# variables to store target data
targetHostname = ""
targetAddress = ""

# get protocol codes by their names
icmp = socket.getprotobyname("icmp")
udp = socket.getprotobyname("udp")

# creates an udp socket (ssnd) to send a test package
# and a icmp socket (srcv) to receive data from the route
ssnd = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, udp)
srcv = socket.socket(socket.AF_INET, socket.SOCK_RAW, icmp)

# bind the receiver to the commonly used port
srcv.bind(("", port))

# main function, that holds all the tracer logic
def run(ttl):
    # defines IP_TTL field to the package we are going to send
    # and then we just send it
    pings = ""
    for i in range(0, numPings):
        start = time.time()

        ssnd.setsockopt(socket.SOL_IP, socket.IP_TTL, ttl)
        ssnd.sendto(" ", (targetAddress, port))

        # after that, we should receive a icmp response
        # containing data about the device where the package died
        srcv.settimeout(timeout)
        _, currentAddress = srcv.recvfrom(1024)
        currentAddress = currentAddress[0]

        final = time.time()
        elapsedTime = (final - start) * 1000
        pings += str(int(elapsedTime)) + "ms "

    # we try to find device's name
    try:
        currentHostname = socket.gethostbyaddr(currentAddress)[0]
    except socket.error:
        currentHostname = currentAddress

    # finally, we just print the data we got
    print("  %03d - %s (%s) - %s" % (ttl, currentAddress, currentHostname, pings))

    # checks if we've tried the max desired ttl value
    # or if we reached to the target host
    # if true, just closes ssnd and srcv
    # else, hop to another device, running tracer with a higher ttl
    if currentAddress == targetAddress or ttl > maxHops:
        print("\n")
        ssnd.close()
        srcv.close()
    else:
        run(ttl + 1)

if __name__ == "__main__":
    # configuring cli arguments, which are:
    # host, to know where we should trace a route to
    # max-hops, to change the default max_ttl (maxHops) value
    # timeout, to change the default timeout value
    parser = argparse.ArgumentParser()
    parser.add_argument("host", help="specifies the target to trace a route to",
        type=str)
    parser.add_argument("--max-hops", type=int, default=maxHops,
        help="define the max number of hops (max_ttl). Defaults to " + str(maxHops))
    parser.add_argument("--timeout", type=int, default=timeout,
        help="sets timeout for waiting each ping. Defaults to " + str(timeout))

    # if everything is okay, we should not get any exceptions
    try:
        # parse arguments values
        args = parser.parse_args()
        maxHops = args.max_hops
        timeout = args.timeout

        # if user provided the host name, it will evaluate to true
        # so, we just treat it like a host name
        if(re.search('[a-zA-Z]+', args.host)):
            targetHostname = args.host
            targetAddress = socket.gethostbyname(args.host)
        else:
            # in this case, the user provided the target ip address
            # not a host name, so we just consider its address
            targetAddress = args.host
            targetHostname = args.host

        # print a header to the execution
        print("tracer to %s (%s), max_ttl = %d, timeout = %d" %
            (targetHostname, targetAddress, maxHops, timeout))
        # then, start running tracer with ttl equals to 1
        run(1)
    # if user interrupts, we let tracer die
    except KeyboardInterrupt:
        sys.exit(0)
    # # if arguments throw any exception, we just print help information
    except:
        parser.print_help()
