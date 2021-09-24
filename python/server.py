import asyncio
import logging
import re
import sys
import time
import json
import aiohttp

_API_KEY = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'

_SERVER_NEIGHBORS = {
    'Hill': ['Jaquez', 'Smith'],
    'Singleton': ['Jaquez', 'Smith', 'Campbell'],
    'Smith': ['Hill', 'Campbell', 'Singleton'],
    'Jaquez': ['Hill', 'Singleton'],
    'Campbell': ['Singleton', 'Smith'],
}

_SERVER_PORTS = {
    'Hill': 12345,
    'Singleton': 23456,
    'Smith': 34567,
    'Jaquez': 45678,
    'Campbell': 56789
}

class PlacesServer:
    def __init__(self, name):
        self.log = logging.getLogger(f'{name}Server')

        try:
            self.neighbor_servers = _SERVER_NEIGHBORS[name]
        except KeyError:
            self.log.critical(f'Invalid server name {name}. Exiting...')
            exit(2)

        self.port = _SERVER_PORTS[name]
        self.name = name

        self.clients = {}

    async def run_server_forever(self):
        self.log.info(f'Starting {self.name} server')
        server = await asyncio.start_server(self._handle_client, '127.0.0.1', self.port)

        async with server:
            await server.serve_forever()

    async def _handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        data = await reader.read()
        raw_msg = data.decode()
        self.log.info(f'Received message with data "{raw_msg}"')
        msg = raw_msg.split()
        command, data = msg[0], msg[1:]

        resp = None
        if command == 'IAMAT':
            try:
                resp = await self.process_iamat(*data)
            except TypeError as e:
                self.log.warning(f'Invalid IAMAT message: {e}')
                resp = f'? {raw_msg}'
        elif command == 'WHATSAT':
            try:
                resp = await self.process_whatsat(*data)
            except TypeError as e:
                self.log.warning(f'Invalid WHATSAT message: {e}')
                resp = f'? {raw_msg}'
        elif command == 'AT':
            try:
                self.log.info('Processing AT message')
                await self.process_at(data)
            except TypeError as e:
                self.log.warning(f'Invalid AT message: {e}')
        else:
            resp = f'? {raw_msg}'

        if resp is None:
            self.log.info('No response being sent back')
        else:
            self.log.info(f'Sending "{resp}" back')
            writer.write(resp.encode())
            await writer.drain()

        writer.close()

    async def process_iamat(self, client, loc, cli_time):
        if not re.match(r'[+-][0-9]+\.?[0-9]*[+-][0-9]+\.?[0-9]*$', loc):
            raise TypeError(f'Invalid lat/long {loc}')

        try:
            delta_t = time.time() - float(cli_time)
        except ValueError:
            raise TypeError(f'Invalid time {cli_time}')

        await self.update_client(self.name, delta_t, client, loc, cli_time)
        return f'AT {self.name} {"+" if delta_t > 0 else ""}{delta_t} {client} {loc} {cli_time}'

    async def update_client(self, server, delta_t, client, loc, cli_time):
        new_data = (server, delta_t, client, loc, cli_time)

        try:
            if self.clients[client] == new_data:
                self.log.info('Not updating!')
                return
        except KeyError:
            self.log.info(f'Adding new client {client}')

        self.log.info(f'Updating and flooding information for {client}')
        self.clients[client] = new_data
        await self.flood_neighbors(client, new_data)

    async def flood_neighbors(self, client, new_data):
        for neighbor_server in self.neighbor_servers:
            self.log.info(f'Flooding neighbor {neighbor_server} at port {_SERVER_PORTS[neighbor_server]} with {new_data}')
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', _SERVER_PORTS[neighbor_server])
            except ConnectionRefusedError as e:
                self.log.warning(e)
                continue
            writer.write('AT {} {} {} {} {}'.format(*new_data).encode())
            await writer.drain()
            writer.close()

    async def process_at(self, data):
        await self.update_client(*data)

    async def process_whatsat(self, client, radius, info_bound):
        try:
            server, delta_t, client, latlong, cli_time = self.clients[client]
        except KeyError:
            raise TypeError(f'Invalid client {client}')

        try:
            radius = int(radius) * 1000
            if radius > 50000:
                raise ValueError
        except ValueError:
            raise TypeError(f'Invalid radius {radius}')

        try:
            info_bound = int(info_bound)
            if info_bound > 20:
                raise ValueError
        except ValueError:
            raise TypeError(f'Invalid info bound {info_bound}')

        try:
            latitude, longitude = re.findall(r'[+-][0-9]+\.?[0-9]*', latlong)
        except ValueError:
            raise TypeError(f'Invalid latlong measurement {latlong}')

        async with aiohttp.ClientSession() as session:
            url = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'\
                + f'key={_API_KEY}&'\
                + f'location={float(latitude)},{float(longitude)}&'\
                + f'radius={radius}'
            async with session.get(url) as response:
                resp_json = await response.json()
            resp_json['results'] = resp_json['results'][:info_bound]
        if resp_json['status'] != 'OK':
            self.log.critical('Place API invalid resonse!')
        self.log.info(f'AT {server} {delta_t} {client} {latlong} {cli_time}\n{json.dumps(resp_json, indent=4)}')
        return f'AT {server} {delta_t} {client} {latlong} {cli_time}\n{json.dumps(resp_json, indent=4)}\n'

async def main(name):
    logging.basicConfig(level=logging.INFO,
                        format='[%(name)s][%(levelname)s] %(message)s',
                        filename=f'{name}Server.log')
    s = PlacesServer(name)
    await s.run_server_forever()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Invalid usage: python server.py SERVERNAME')
        exit(1)
    asyncio.run(main(sys.argv[1]))
