import asyncio
import aiohttp
import time
import json 
import sys
import os

API_KEY = "USE YOUR OWN"
API_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
MAX_RADIUS = 50
MAX_INFO = 20

PORTS = {
    "Bailey": 10000,
    "Bona": 10001,
    "Campbell": 10002,
    "Clark": 10003,
    "Jaquez": 10004
}

NEIGHBORS = {
    "Clark": ["Jaquez", "Bona"],
    "Campbell": ["Jaquez", "Bona", "Bailey"],
    "Bona": ["Clark", "Bailey", "Campbell"],
    "Jaquez": ["Clark", "Campbell"], 
    "Bailey": ["Bona", "Campbell"]
}

class Server:
    def __init__(self, name):
        self.name = name
        self.port = PORTS[name]
        self.neighbors = NEIGHBORS[name]
        self.clients = {}
        self.log_file = "log/" + self.name + ".txt"
        os.makedirs("log", exist_ok=True)


    def log(self, message):
        with open(self.log_file, "a") as f:
            f.write(message + "\n")


    async def handle_client(self, reader, writer):
        while True:
            data = await reader.readline()
            if not data:
                break
            message = data.decode().strip()
            self.log("Recieved from client: " + message)

            response = await self.process_message(message)
            if response:
                writer.write((response + "\n").encode())
                await writer.drain()
                self.log("Sent to client: " + response)

        writer.close()
        await writer.wait_closed()
        self.log("Client connection closed")


    async def process_message(self, message):
        split_message = message.strip().split()

        if not split_message:
            self.log("ERROR: Empty message: " + message)
            return "? " + message

        if split_message[0] == "IAMAT" and len(split_message) == 4:
            self.log("Processing IAMAT: " + message)
            return await self.process_iamat(split_message)
        
        elif split_message[0] == "WHATSAT" and len(split_message) == 4:
            self.log("Processing WHATSAT: " + message)
            return await self.process_whatsat(split_message)
        
        elif split_message[0] == "AT" and len(split_message) == 6:
            self.log("Processing AT: " + message)
            await self.process_at(split_message)
            return None
        
        else:
            self.log("ERROR: Invalid message: " + message)
            return "? " + message


    async def process_iamat(self, split_message):

        message = " ".join(split_message)

        try:
            client_id = split_message[1]
            coordinates = split_message[2]
            client_time = float(split_message[3])

            time_diff = time.time() - client_time

            if time_diff >= 0:
                time_diff_str = "+" + str(time_diff)
            else:
                time_diff_str = str(time_diff)

            response = "AT " + self.name + " " + time_diff_str + " " + client_id + " " + coordinates + " " + str(client_time)
            self.clients[client_id] = (response, client_time)
            self.log("IAMAT processed for client " + client_id)
            await self.flood(response)
            return response
        
        except Exception:
            self.log("ERROR Processing IAMAT")
            return "? " + message


    async def process_whatsat(self, split_message):

        message = " ".join(split_message)

        try:
            client_id = split_message[1]
            radius = float(split_message[2])
            upper_bound = int(split_message[3])

            if (client_id not in self.clients) or (radius > MAX_RADIUS) or (upper_bound > MAX_INFO):
                return "? " + message

            response, _ = self.clients[client_id]
            coordinates = response.split()[4]
            lat, lon = self.split_coordinate(coordinates)

            json_data = await self.process_api_request(lat, lon, radius, upper_bound)
            self.log("WHATSAT request from: " + client_id)
            return response + "\n" + json_data
        except Exception:
            self.log("ERROR processing WHATSAT")
            return "? " + message


    async def process_api_request(self, lat, lon, radius, upper_bound):
        url = API_URL + "?location=" + str(lat) + "," + str(lon) + "&radius=" + str(int(radius * 1000)) + "&key=" + API_KEY

        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                result = await response.json()

        result["results"] = result["results"][:upper_bound]
        return json.dumps(result, indent=2) + "\n\n"


    async def process_at(self, split_message):

        message = " ".join(split_message)

        try:
            client_id = split_message[3]
            client_time = float(split_message[5])

            if (client_id not in self.clients) or (client_time > float(self.clients[client_id][1])):
                self.clients[client_id] = (message, client_time)
                self.log("AT message accepted for client: " + client_id)
                await self.flood(message)
                
        except Exception as e:
            self.log("ERROR processing AT: " + str(e))


    async def flood(self, message):
        for neighbor in self.neighbors:
            try:
                reader, writer = await asyncio.open_connection("127.0.0.1", PORTS[neighbor])
                self.log("Connected to " + neighbor)

                writer.write((message + "\n").encode())
                await writer.drain()
                self.log("Flooded " + neighbor + " with: " + message)
                
                writer.close()
                await writer.wait_closed()
                self.log("Closed connection to " + neighbor)

            except Exception as e:
                self.log("Failed to connect with " + neighbor + ": " + str(e))


    def split_coordinate(self, coordinates):
        for i in range(1, len(coordinates)):
            if coordinates[i] in "+-":
                index = i
                break

        lat = coordinates[:index]
        lon = coordinates[index:]

        return float(lat), float(lon)

async def main():

    name = sys.argv[1]

    if name not in PORTS:
        print("Invalid server name: " + name)
        sys.exit(1)
        
    server_obj = Server(name)
    server = await asyncio.start_server(server_obj.handle_client, "127.0.0.1", server_obj.port)

    addrs = ', '.join(str(sock.getsockname()) for sock in server.sockets)

    server_obj.log("Serving on " + addrs)

    async with server:
        await server.serve_forever()

if __name__ == "__main__":
    asyncio.run(main())
