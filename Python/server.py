import asyncio
import sys
import time
import json
import aiohttp
import async_timeout
import logging
import re
import argparse

logging.basicConfig(filename='server.log', filemode='a', level=logging.INFO)

PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
MY_API_KEY = "AIzaSyBoCLqWfJpxFtFaE9GYoYfzXv4r4k7Gik0"

SERVER_TO_PORT = {
	"Hill" : 11595,
    "Jaquez" : 11596,
    "Smith" : 11597,
    "Campbell" : 11598,
    "Singleton" : 11599
}

SERVER_CONNECTIONS = {
    "Hill": ["Jaquez", "Smith", "Singleton"],
    "Jaquez": ["Hill", "Singleton"],
    "Smith": ["Hill", "Singleton", "Campbell"],
    "Campbell": ["Smith"],
    "Singleton": ["Hill", "Jaquez", "Smith"],
}

class Server:
    
    def __init__(self, serverID, ip='127.0.0.1', message_max_length=1e6):
        self.serverID = serverID
        self.ip = ip
        self.port = SERVER_TO_PORT[serverID]
        self.message_max_length = int(message_max_length)
        self.clients = dict()
        
    # ---------------------------------------------------------------------------
    # LOGGING
    # ---------------------------------------------------------------------------
    # def logMessage(self, message):
    #     print(message)
    #     logging.info(message+"\n")
    #     return

    def logMessage(self, msg):	
        if msg == None:
            return
        try:
            logFile.write(msg+"\n")
        except:
            print("ERROR logging: " + msg)
            return
        print(msg)

    # ---------------------------------------------------------------------------
    # HELPERS
    # ---------------------------------------------------------------------------

    # check for valid command type and arguments
    def checkCommand(self, commandType, args):
        coordinates = None
        time = None

        if commandType == "IAMAT" or commandType =="WHATSAT":
            if len(args) != 3:
                self.logMessage("ERROR: invalid args for " + commandType)
                return False
            coordinates = args[1]
            time = args[2]
        elif commandType == "AT":
            if len(args) != 6:
                self.logMessage("ERROR: invalid args for " + commandType)
                return False
            self.serverID = args[0]
            coordinates = args[3]
            time = args[4]

            if self.serverID not in SERVER_TO_PORT:
                self.logMessage("ERROR: invalid server ID " + self.serverID)
                return False
        else:
            self.logMessage("ERROR: invalid command type " + commandType)
            return False

        if coordinates != None:
            if not re.fullmatch('[+-]\d*\.?\d+[+-]\d*\.?\d+', coordinates):
                self.logMessage("ERROR: invalid coordinates " + coordinates)
                return False
        if time != None:
            try:
                float(time)
            except ValueError:
                self.logMessage("ERROR: invalid time " + time)
                return False
        
        return True

    def formatTime(self, clientTime):
        timeDiff = time.time() - float(clientTime)
        if timeDiff > 0:
            timeDiffString = "+" + str(timeDiff)
        else:
            timeDiffString = str(timeDiff)

        return timeDiffString

    def splitLatLong(self, latlong):
        i = 0
        coordinates = 0
        while i < len(latlong) and coordinates < 2:
            if latlong[i] == "+" or latlong[i] == "-":
                coordinates += 1
            i += 1
        latitude = latlong[:(i-1)]
        longitude = latlong[(i-1):]
        return (latitude, longitude)

    # sends api request to Google Places
    async def request(self, session, url):
        async with async_timeout.timeout(10):
            async with session.get(url) as response:
                return await response.json()

    async def respondInvalid(self, clientID, command, writer):
        response = "? " + command
        self.logMessage("INVALID COMMAND: " + clientID + "\n\n")
        self.logMessage("RESPONDING: " + response + "\n\n")
        return response

    # ---------------------------------------------------------------------------
    # HANDLE COMMANDS
    # ---------------------------------------------------------------------------

    async def handleIAMAT(self, clientID, latlong, cmdTime):
        time.sleep(2)
        latitude, longitude = self.splitLatLong(latlong)

        if clientID in self.clients:
            if float(self.clients[clientID]["cmdTime"]) >= float(cmdTime): 
                self.logMessage("Ignoring outdated IAMAT from clientID: " + clientID + "\n")
                return
            else:
                self.logMessage("Processing updated IAMAT from clientID: " + clientID + "\n")
        else:
            self.logMessage("Processing first IAMAT from new clientID: " + clientID + "\n")

        timeDiff = time.time() - float(cmdTime)

        self.clients[clientID] = {
            "serverID" : self.serverID,
            "latitude" : latitude,
            "longitude" : longitude,
            "timeDiff" : float(timeDiff),
            "cmdTime" : float(cmdTime)
        }
        print("NEW ENTRY from IAMAT under " + self.serverID ) #+"\n"+self.clients[clientID]

        # make AT message for client and write
        atMessage = "AT %s +%s %s %s %s" % (self.serverID, str(timeDiff), clientID, latlong, str(cmdTime))

        await self.floodATServers(clientID, atMessage, [self.serverID, clientID])

        return atMessage

    async def floodATServers(self, clientID, atMessage, sentServers):
        for server in SERVER_CONNECTIONS[self.serverID]:
            if server not in sentServers:
                targetServer = SERVER_TO_PORT[server]
                self.logMessage("Forwarding AT from Client %s to Server %s" % (clientID, server))
                try:
                    reader, writer = await asyncio.open_connection("127.0.0.1", targetServer)
                    self.logMessage("Opened connectiong to Server " + server + "\n")
                    self.logMessage("Forwarding AT from Client %s to Server %s:\n%s\n" % (clientID, server, atMessage))
                    #await writeToConnection(writer, atMessage)
                    writer.write(atMessage.encode())
                    data = await reader.read(int(1e6))
                    print(f'Received: {data.decode()!r}')
                    writer.close()
                    self.logMessage("Closing connection to Server " + server + "\n")
                except:
                    self.logMessage("ERROR: Failed forwarding AT to Server " + server)
        return

    async def handleAT(self, serverID, timeDiff, clientID, latlong, cmdTime):
        if clientID in self.clients:
            if float(self.clients[clientID]["cmdTime"]) >= float(cmdTime): 
                self.logMessage("Ignoring outdated AT from clientID: " + clientID + "\n")
                return "IGNORING"
            else:
                self.logMessage("Processing updated AT from clientID: " + clientID + "\n")
        else:
            self.logMessage("Processing first AT from new clientID: " + clientID + "\n")

        latitude, longitude = self.splitLatLong(latlong)

        self.clients[clientID] = {
            "serverID" : serverID,
            "latitude" : latitude,
            "longitude" : longitude,
            "timeDiff" : float(timeDiff),
            "cmdTime" : float(cmdTime)
        }
        print("NEW ENTRY from AT under " + self.serverID + " from " + serverID) # +"\n"+self.clients[clientID]

        ATforServers = "AT %s %s %s %s %s" % (serverID, str(timeDiff), clientID, latlong, str(cmdTime))
        await self.floodATServers(clientID, ATforServers, [clientID, self.serverID, serverID])
        return ATforServers


    async def handleWHATSAT(self, clientID, radius, maxResults):

        timeDiff = self.clients[clientID]["timeDiff"]
        latitude = self.clients[clientID]["latitude"]
        longitude = self.clients[clientID]["longitude"]
        serverID = self.clients[clientID]["serverID"]
        latlong = latitude+longitude

        #query = f"{PLACES_URL}key={MY_API_KEY}&location={latitude},{longitude}&radius={float(radius)}"
        loc = "{0},{1}".format(latitude, longitude)
        query = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(
            MY_API_KEY, loc, radius)

        async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False,),) as session:
            async with session.get(query) as response:
                apiResponse = await response.json()

                print("APIRESPONSE:")
                print(apiResponse)
                apiResponse["results"] = apiResponse["results"][:int(maxResults)]

                reply = f"AT {serverID} {str(timeDiff)} {clientID} {latlong} {time.time()} \n{json.dumps(apiResponse, indent=4)}\n\n"
                self.logMessage("Replying to WHATSAT from client " + clientID + ": \n" + reply + "\n\n")
                return reply


    async def parse_message(self, message):
        command_table = {
            "IAMAT": self.handleIAMAT,
            "WHATSAT": self.handleWHATSAT,
            "AT": self.handleAT
        }
        message_list = [msg for msg in message.strip().split() if len(msg)]

        cmd = command_table.get(message_list[0], lambda x, y, z: f"ERROR: command name {message_list[0]} unrecognized")
        return await cmd(*message_list[1:])

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        parsed_message = await self.parse_message(message) if len(message) else "IGNORE"
        addr = writer.get_extra_info('peername')
        print("{} received {} from {}".format(self.serverID, message, addr))

        sendback_message = parsed_message
        if sendback_message == "IGNORE":
            sendback_message = "? "+message

        print("{} send: {}".format(self.serverID, sendback_message))
        writer.write(sendback_message.encode())
        await writer.drain()

        print("close the client socket")
        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    print("Hello, welcome to server {}".format(args.server_name))

    log = args.server_name + "-log.txt"
    global logFile
    open(log, "w").close() #clears the file
    logFile = open(log, 'a+')
    logFile.write(args.server_name + '\n')

    server = Server(args.server_name)
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        print('\n' + args.server_name +' closed at time:', time.time())
        logFile.write(args.server_name + ' closed at time:' + str(time.time()))
        pass

if __name__ == "__main__":
    main()
